# 1. data_prep.R for Conversations Gone Awry (CMV) - Political Comments Analysis

# This script loads the three CSVs exported from Python/Convokit data extraction, 
# corrects column types, derives key variables, and builds dataframes ready for 
# joint analysis of the datasets, cacheing them to prepped_data/ as .rds files.

# This script runs before analysis files.

# Outputs:
#   prepped_data/comments_enriched.rds  (main analysis table)
#   prepped_data/conversations.rds
#   prepped_data/speakers.rds

# -- 0. Packages --
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate)
fmt <- function(x) format(x, big.mark = ",")

# -- 1. Paths --
# Runs on the original export (usernames) and writes prepped_data/ (local only).
# 1b_pseudonymize_users.R then writes the shareable copies:
#   input_data_anonymized/ and prepped_data_anonymized/
INPUT_DIR  <- "input_data_from_python"
OUTPUT_DIR <- "prepped_data"
dir.create(OUTPUT_DIR, showWarnings = FALSE)

# -- 2. Load raw CSVs --

message("Loading CSVs...")

comments      <- read_csv(file.path(INPUT_DIR, "comments.csv"),      show_col_types = FALSE)
conversations <- read_csv(file.path(INPUT_DIR, "conversations.csv"), show_col_types = FALSE)
speakers      <- read_csv(file.path(INPUT_DIR, "speakers.csv"),      show_col_types = FALSE)

message(sprintf("  comments:      %s rows", format(nrow(comments),      big.mark = ",")))
message(sprintf("  conversations: %s rows", format(nrow(conversations), big.mark = ",")))
message(sprintf("  speakers:      %s rows", format(nrow(speakers),      big.mark = ",")))


# -- 3. Data cleaning for R format --


# Clean timestamp data, gilded, stickied data, and metadata for root, top_level_reply, 
# nested_reply; Compute word counts, convert potential N/A text fields to blanks

comments <- comments %>%
  mutate(
    # datetime + temporal extracts
    timestamp_dt = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"),
    year         = year(timestamp_dt),
    month        = month(timestamp_dt),
    # award and moderation flags
    gilded   = as.integer(gilded),
    stickied = stickied == "True",
    # thread position
    comment_type = case_when(
      comment_id == conversation_id                    ~ "root",
      !is.na(reply_to) & reply_to == conversation_id  ~ "top_level_reply",
      TRUE                                             ~ "nested_reply"
    ) %>% factor(levels = c("root", "top_level_reply", "nested_reply")),
    # text length
    char_count = nchar(coalesce(text, "")),
    word_count = str_count(coalesce(text, ""), "\\S+")
  )


# -- 4. Build "enriched" comments table --

# Joins comments to it's conversational outcome (awry vs not-awry) and metadata. This is key
# table for downstream scripts, as most analyses require "has_removed_comment" (i.e., awry)
# conversation at the comment level.

comments_enriched <- comments %>%
  left_join(
    conversations %>%
      select(conversation_id, pair_id, has_removed_comment, split),
    by = "conversation_id"
  )

# Identify the specific removed comment within each awry conversation.
# The corpus has no explicit per-comment "removed" flag; per the dataset's
# construction methodology (Zhang et al. 2018), an awry conversation is one
# where moderators removed the FINAL comment for a Rule 2 violation. We infer
# the removed comment as the latest-timestamp comment within each
# has_removed_comment == TRUE conversation.
comments_enriched <- comments_enriched %>%
  arrange(conversation_id, timestamp) %>%
  group_by(conversation_id) %>%
  mutate(is_removed_comment = has_removed_comment & row_number() == n()) %>%
  ungroup()

# Thread depth — a finer-grained complement to comment_type.
# root = depth 0, top_level_reply = depth 1, nested_reply = depth 2+.
# Computed by iteratively propagating depth along the reply_to chain, since
# reply_to only gives the direct parent (one level), not the full ancestry.

comments_enriched <- comments_enriched %>%
  mutate(thread_depth = ifelse(comment_id == conversation_id, 0L, NA_integer_))

repeat {
  unresolved_before <- sum(is.na(comments_enriched$thread_depth))
  if (unresolved_before == 0) break
  
  comments_enriched <- comments_enriched %>%
    left_join(
      comments_enriched %>% select(comment_id, parent_depth = thread_depth),
      by = c("reply_to" = "comment_id")
    ) %>%
    mutate(thread_depth = ifelse(is.na(thread_depth) & !is.na(parent_depth),
                                 parent_depth + 1L, thread_depth)) %>%
    select(-parent_depth)
  
  unresolved_after <- sum(is.na(comments_enriched$thread_depth))
  if (unresolved_after == unresolved_before) {
    warning(sprintf(
      "%d comments could not be resolved to a thread depth — check for broken reply_to chains.",
      unresolved_after
    ))
    break
  }
}

cat(sprintf("Thread depth resolved for %s of %s comments.\n",
            format(sum(!is.na(comments_enriched$thread_depth)), big.mark = ","),
            format(nrow(comments_enriched), big.mark = ",")))

# Sanity check: every comment should match a conversation.
n_unmatched <- sum(is.na(comments_enriched$has_removed_comment))
if (n_unmatched > 0) {
  warning(sprintf(
    "%s comments did not match a conversation — check for ID mismatches in the CSVs.",
    format(n_unmatched, big.mark = ",")
  ))
} else {
  message("Join complete: all comments matched to a conversation.")
}


# -- 4b. Pair key, post ID, and retrieval timing --

# Pair key: ConvoKit's pair_id holds the PARTNER's conversation_id (A -> B, B -> A),
# so it is unique per conversation. Build a symmetric key shared by both halves.
conversations <- conversations %>%
  mutate(pair_key = paste(pmin(conversation_id, pair_id),
                          pmax(conversation_id, pair_id), sep = "_"))

# Check symmetry: every partner should exist and point back
n_asymmetric <- conversations %>%
  left_join(conversations %>% select(conversation_id, partner_points_to = pair_id),
            by = c("pair_id" = "conversation_id")) %>%
  summarise(n = sum(is.na(partner_points_to) | partner_points_to != conversation_id)) %>%
  pull(n)

cat(sprintf("Pairs: %s | asymmetric pair links (should be 0): %d\n",
            fmt(n_distinct(conversations$pair_key)), n_asymmetric))

# Post ID and title slug from permalink:
#   /r/changemyview/comments/<post_id>/<title_slug>/<comment_id>/
# Retrieval timing: retrieved_on is when the comment was scraped (epoch seconds),
# so age at retrieval = retrieved_on - timestamp.
comments_enriched <- comments_enriched %>%
  mutate(
    post_id_raw           = str_match(permalink, "/comments/([a-z0-9]+)/")[, 2],
    post_slug_raw         = str_match(permalink, "/comments/[a-z0-9]+/([^/]+)/")[, 2],
    retrieved_dt          = as.POSIXct(retrieved_on, origin = "1970-01-01", tz = "UTC"),
    age_at_retrieval_days = (retrieved_on - timestamp) / 86400
  )

# Conversation level: all comments in a conversation should share one post
conv_post <- comments_enriched %>%
  filter(!is.na(post_id_raw)) %>%
  group_by(conversation_id) %>%
  summarise(
    n_post_ids_conv = n_distinct(post_id_raw),
    post_id_own     = first(post_id_raw),
    post_slug_own   = first(post_slug_raw),
    .groups = "drop"
  )

n_conv_conflict <- sum(conv_post$n_post_ids_conv > 1)

# Pair level: both halves of a pair should come from the same post. Fill a
# conversation's post ID from its partner where its own comments lack permalinks.
pair_post <- conversations %>%
  select(conversation_id, pair_key) %>%
  left_join(conv_post, by = "conversation_id") %>%
  group_by(pair_key) %>%
  summarise(
    n_post_ids_pair = n_distinct(post_id_own, na.rm = TRUE),
    post_id         = first(na.omit(post_id_own)),
    post_slug       = first(na.omit(post_slug_own)),
    .groups = "drop"
  )

n_pair_conflict <- sum(pair_post$n_post_ids_pair > 1)

conversations <- conversations %>%
  left_join(conv_post %>% select(conversation_id, post_id_own), by = "conversation_id") %>%
  left_join(pair_post %>% select(pair_key, post_id, post_slug), by = "pair_key") %>%
  mutate(post_id_source = case_when(
    !is.na(post_id_own) ~ "own conversation",
    !is.na(post_id)     ~ "filled from pair partner",
    TRUE                ~ "missing"
  )) %>%
  select(-post_id_own)

# Sample flags: conversation start time, period, and post-ID availability.
# Period is assigned at the PAIR level (earliest start year in the pair), so the
# two halves of a pair are never split across periods.
conv_start <- comments_enriched %>%
  group_by(conversation_id) %>%
  summarise(conv_start_ts = min(timestamp), .groups = "drop")

conversations <- conversations %>%
  left_join(conv_start, by = "conversation_id") %>%
  mutate(
    conv_start_dt = as.POSIXct(conv_start_ts, origin = "1970-01-01", tz = "UTC"),
    conv_year     = year(conv_start_dt)
  ) %>%
  group_by(pair_key) %>%
  mutate(pair_year = min(conv_year)) %>%
  ungroup() %>%
  mutate(
    period      = if_else(pair_year <= 2017, "2015-2017", "2018-2022"),
    has_post_id = !is.na(post_id)
  ) %>%
  select(-conv_start_ts)

n_pairs_split_years <- conversations %>%
  group_by(pair_key) %>%
  summarise(split = n_distinct(conv_year) > 1, .groups = "drop") %>%
  summarise(n = sum(split)) %>%
  pull(n)

cat(sprintf("Pairs whose two halves start in different calendar years: %d\n", n_pairs_split_years))

comments_enriched <- comments_enriched %>%
  select(-post_id_raw, -post_slug_raw) %>%
  left_join(conversations %>% select(conversation_id, pair_key, post_id, post_slug,
                                     period, has_post_id),
            by = "conversation_id")

# Coverage diagnostics
cat("\n-- Post ID coverage --\n")
cat(sprintf("Comments with permalink:        %.1f%%\n",
            100 * mean(!is.na(comments_enriched$permalink))))
cat(sprintf("Conversations with own post ID: %.1f%%\n",
            100 * mean(conversations$post_id_source == "own conversation")))
cat(sprintf("Conversations with any post ID: %.1f%%\n",
            100 * mean(!is.na(conversations$post_id))))
cat(sprintf("Pairs with post ID:             %.1f%%\n",
            100 * mean(!is.na(pair_post$post_id))))
cat(sprintf("Conversations spanning >1 post (should be 0): %d\n", n_conv_conflict))
cat(sprintf("Pairs spanning >1 post (should be 0):         %d\n", n_pair_conflict))
cat(sprintf("Unique posts: %s\n", fmt(n_distinct(na.omit(conversations$post_id)))))

cat("\nPost ID coverage by conversation start year:\n")
comments_enriched %>%
  group_by(conversation_id) %>%
  summarise(year = min(year), has_post = !is.na(first(post_id)), .groups = "drop") %>%
  group_by(year) %>%
  summarise(n_conversations = n(), pct_with_post_id = round(100 * mean(has_post), 1),
            .groups = "drop") %>%
  print()

# -- 5. Save to prepped data --

message("Saving .rds files to prepped_data/...")
saveRDS(comments_enriched, file.path(OUTPUT_DIR, "comments_enriched.rds"))
saveRDS(conversations,     file.path(OUTPUT_DIR, "conversations.rds"))
saveRDS(speakers,          file.path(OUTPUT_DIR, "speakers.rds"))


# -- 6. Summary checks --


n_awry     <- sum( conversations$has_removed_comment, na.rm = TRUE)
n_on_track <- sum(!conversations$has_removed_comment, na.rm = TRUE)
date_min   <- min(comments$timestamp_dt, na.rm = TRUE)
date_max   <- max(comments$timestamp_dt, na.rm = TRUE)

cat(sprintf("\nPrep complete. %s comments | %s conversations | %s pairs | %s posts | %s speakers\n",
            fmt(nrow(comments_enriched)),
            fmt(nrow(conversations)),
            fmt(n_distinct(conversations$pair_key)),
            fmt(n_distinct(na.omit(conversations$post_id))),
            fmt(nrow(speakers))))
cat(sprintf("Awry: %s / %s (%.1f%%) | Date range: %s to %s\n",
            format(n_awry),
            format(n_awry + n_on_track),
            100 * n_awry / (n_awry + n_on_track),
            format(date_min, "%b %Y"), format(date_max, "%b %Y")))
print(table(comments_enriched$comment_type))

# -- 7. Validation (step 0a): is the final comment in each awry path the removed one? --

# is_removed_comment (section 4) assumes the latest comment in each awry path is the
# one moderators removed under Rule 2. This section tests that assumption in two ways:
#   7a. Structural checks — is the flagged comment the deepest comment, a leaf (no replies),
#       and the unique latest timestamp in its path? Is its text intact?
#   7b. A reading sample — 30 awry paths written to CSV for manual coding against Rule 2.

CHECK_DIR <- "descriptive_results/data_checks"
dir.create(CHECK_DIR, recursive = TRUE, showWarnings = FALSE)

cat("\n=== STEP 0a: REMOVED COMMENT VALIDATION ===\n")

# -- 7a. Structural checks --

# Each "conversation" should be a single path: no comment has more than one reply
n_branch_points <- comments_enriched %>%
  filter(!is.na(reply_to)) %>%
  count(conversation_id, reply_to, name = "n_children") %>%
  filter(n_children > 1) %>%
  nrow()

cat(sprintf("Comments with >1 reply inside a conversation (0 = every conversation is a single path): %d\n",
            n_branch_points))

# Position of the flagged comment within its path
path_position <- comments_enriched %>%
  group_by(conversation_id) %>%
  mutate(
    is_deepest       = thread_depth == max(thread_depth),
    has_reply        = comment_id %in% reply_to,
    n_at_latest_time = sum(timestamp == max(timestamp))
  ) %>%
  ungroup()

removed_structure <- path_position %>%
  filter(is_removed_comment) %>%
  summarise(
    n_removed_flagged        = n(),
    n_not_deepest            = sum(!is_deepest),
    n_has_reply              = sum(has_reply),
    n_latest_timestamp_tied  = sum(n_at_latest_time > 1)
  )

cat("\nFlagged removed comments — structural position (all but the first should be 0):\n")
print(removed_structure)

# Text status: if removed comments were scraped after removal, text would read "[removed]"
text_status <- comments_enriched %>%
  mutate(
    group = if_else(is_removed_comment, "Flagged removed comment", "All other comments"),
    text_status = case_when(
      is.na(text) | str_trim(text) == "" ~ "empty",
      str_trim(text) == "[removed]"      ~ "[removed]",
      str_trim(text) == "[deleted]"      ~ "[deleted]",
      TRUE                               ~ "full text"
    )
  ) %>%
  count(group, text_status) %>%
  group_by(group) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()

cat("\nText status, flagged removed comments vs all others:\n")
print(text_status)

# Age at retrieval: was the flagged comment scraped before moderators could act?
retrieval_age <- comments_enriched %>%
  filter(!is.na(age_at_retrieval_days)) %>%
  mutate(group = if_else(is_removed_comment, "Flagged removed comment", "All other comments")) %>%
  group_by(group) %>%
  summarise(
    n                = n(),
    median_age_days  = round(median(age_at_retrieval_days), 2),
    pct_under_1_hour = round(100 * mean(age_at_retrieval_days < 1/24), 1),
    pct_under_1_day  = round(100 * mean(age_at_retrieval_days < 1), 1),
    .groups = "drop"
  )

cat("\nAge at retrieval (days), flagged removed comments vs all others:\n")
print(retrieval_age)

write_csv(removed_structure, file.path(CHECK_DIR, "0a_removed_comment_structure.csv"))
write_csv(text_status,       file.path(CHECK_DIR, "0a_removed_comment_text_status.csv"))
write_csv(retrieval_age,     file.path(CHECK_DIR, "0a_removed_comment_retrieval_age.csv"))


# -- 7b. Reading sample for manual coding --

# 30 awry paths: 15 from 2015-2017 (no post IDs) and 15 from 2018-2022, in case
# the data were assembled differently across the two periods.
# Speakers are relabelled A, B, C... within each path (usernames not written out).
#
# How to code: read each path top to bottom against CMV Rule 2 ("Don't be rude or
# hostile to other users" - check current wording on the CMV wiki), then fill in the
# coding sheet. The question is whether the FINAL comment is the obvious Rule 2 violation.

set.seed(2026)

# Uses the pair-level period flag created in section 4b
sample_ids <- conversations %>%
  filter(has_removed_comment) %>%
  group_by(period) %>%
  slice_sample(n = 15) %>%
  ungroup() %>%
  select(conversation_id, period)

validation_paths <- comments_enriched %>%
  semi_join(sample_ids, by = "conversation_id") %>%
  arrange(period, conversation_id, timestamp) %>%
  group_by(conversation_id) %>%
  mutate(
    position      = row_number(),
    speaker_label = LETTERS[match(speaker, unique(speaker))]
  ) %>%
  ungroup() %>%
  select(period, conversation_id, post_slug, position, thread_depth,
         speaker_label, is_removed_comment, score, word_count, text)

coding_sheet <- sample_ids %>%
  arrange(period, conversation_id) %>%
  mutate(
    final_comment_violates_rule2 = "",   # Y / N / Unsure
    earlier_comment_worse        = "",   # Y / N - is an earlier comment a clearer violation?
    earlier_comment_position     = "",   # position number of that earlier comment, if any
    notes                        = ""
  )

write_csv(validation_paths, file.path(CHECK_DIR, "0a_validation_sample_paths.csv"))
write_csv(coding_sheet,     file.path(CHECK_DIR, "0a_validation_coding_sheet.csv"))

cat(sprintf("\nValidation sample written: %d paths, %d comments\n",
            nrow(sample_ids), nrow(validation_paths)))
cat("  -> descriptive_results/data_checks/0a_validation_sample_paths.csv\n")
cat("  -> descriptive_results/data_checks/0a_validation_coding_sheet.csv\n")


# -- 8. Sample comparison (step 0b): full corpus vs period vs post-ID sample --

# Post IDs (from permalinks) are only available from partway through 2017. Analyses
# that group by post/topic must use the subset with post IDs. This table checks whether
# that subset - and the two periods - look similar on basic structural measures, so the
# restriction can be justified (or its consequences described).
#
# Columns:
#   Full corpus  | 2015-2017 | 2018-2022 | Has post ID (2017 partial + 2018-2022)
#
# Measures use no text coding: counts, path structure, participation, scores, retrieval.
# Scores use non-final comments only (the final comment's score is cut short by removal
# in awry paths; the on-track final comment is excluded too, for symmetry).

cat("\n=== STEP 0b: SAMPLE COMPARISON ===\n")

f1 <- function(x, d = 1) formatC(x, format = "f", digits = d, big.mark = ",")
f0 <- function(x)        formatC(x, format = "d", big.mark = ",")

# Flag the final comment in every path (awry and on-track)
ce_s8 <- comments_enriched %>%
  group_by(conversation_id) %>%
  mutate(is_final_in_path = thread_depth == max(thread_depth)) %>%
  ungroup()

summarise_sample <- function(ce_s, label) {
  
  conv_s <- ce_s %>%
    group_by(conversation_id, has_removed_comment) %>%
    summarise(
      n_comments = n(),
      n_speakers = n_distinct(speaker),
      .groups = "drop"
    )
  
  users <- ce_s %>%
    filter(speaker != "[deleted]") %>%
    count(speaker, name = "n_comments")
  
  top1_cut  <- quantile(users$n_comments, 0.99)
  non_final <- ce_s %>% filter(!is_final_in_path)
  
  tibble(
    Metric = c(
      "Comments", "Conversations (paths)", "Pairs", "Posts (threads)",
      "Unique users (excl. [deleted])", "Date range",
      "Mean comments per path", "Median comments per path",
      "Mean comments per path - awry", "Mean comments per path - on-track",
      "Mean speakers per path", "% paths with exactly 2 speakers",
      "% comments by top 1% of users", "% users with one comment",
      "% comments by [deleted]",
      "Median word count", "% comments with delta flair",
      "Median score (non-final comments)", "Mean score (non-final comments)",
      "Median age at retrieval (days)", "% retrieved within 1 day"
    ),
    !!label := c(
      f0(nrow(ce_s)),
      f0(n_distinct(ce_s$conversation_id)),
      f0(n_distinct(ce_s$pair_key)),
      f0(n_distinct(na.omit(ce_s$post_id))),
      f0(nrow(users)),
      paste(format(min(ce_s$timestamp_dt), "%b %Y"), "-",
            format(max(ce_s$timestamp_dt), "%b %Y")),
      f1(mean(conv_s$n_comments)),
      f1(median(conv_s$n_comments)),
      f1(mean(conv_s$n_comments[conv_s$has_removed_comment])),
      f1(mean(conv_s$n_comments[!conv_s$has_removed_comment])),
      f1(mean(conv_s$n_speakers)),
      f1(100 * mean(conv_s$n_speakers == 2)),
      f1(100 * sum(users$n_comments[users$n_comments >= top1_cut]) / nrow(ce_s)),
      f1(100 * mean(users$n_comments == 1)),
      f1(100 * mean(ce_s$speaker == "[deleted]")),
      f1(median(ce_s$word_count, na.rm = TRUE)),
      f1(100 * mean(!is.na(ce_s$author_flair_text))),
      f1(median(non_final$score, na.rm = TRUE)),
      f1(mean(non_final$score, na.rm = TRUE)),
      f1(median(ce_s$age_at_retrieval_days, na.rm = TRUE)),
      f1(100 * mean(ce_s$age_at_retrieval_days < 1, na.rm = TRUE))
    )
  )
}

sample_comparison <- list(
  summarise_sample(ce_s8,                                     "Full corpus"),
  summarise_sample(ce_s8 %>% filter(period == "2015-2017"),   "2015-2017"),
  summarise_sample(ce_s8 %>% filter(period == "2018-2022"),   "2018-2022"),
  summarise_sample(ce_s8 %>% filter(has_post_id),             "Has post ID")
) %>%
  reduce(left_join, by = "Metric")

print(sample_comparison, n = Inf, width = Inf)

write_csv(sample_comparison, file.path(CHECK_DIR, "0b_sample_comparison.csv"))
cat("  -> descriptive_results/data_checks/0b_sample_comparison.csv\n")