# 3b. descriptives_removal_authors.R for Conversations Gone Awry (CMV) - Political Comments Analysis
#
# Who writes removed comments, once the number of chances to be removed is accounted for?
#
# Definition (opportunity-adjusted):
#   In this dataset a comment can only be removed if it is the FINAL comment of a path.
#   Every path ends with one final comment: in awry paths it is the removed comment, in
#   on-track paths it is where the sampled path stops. For each user:
#     n_endings     = paths whose final comment they wrote (awry + on-track)
#     n_removed     = of those, how many were removed (awry endings)
#     removed_share = n_removed / n_endings   (50% expected by design: one of each per pair)
#   A user's z-score compares n_removed with the binomial(n_endings, 0.5) expectation:
#     z = (n_removed - n_endings / 2) / sqrt(n_endings / 4)
#   Labels (users with at least MIN_ENDINGS endings):
#     "Removed more often than chance"  z >= +Z_THRESHOLD
#     "Within chance"                   -Z_THRESHOLD < z < +Z_THRESHOLD
#     "Removed less often than chance"  z <= -Z_THRESHOLD
#   With many users, some will pass +/-2 by luck alone; section 2 reports how many to
#   expect by chance next to how many are observed.
#
# Caveats:
#   - On-track endings are where the SAMPLED path stops (dataset construction), not
#     necessarily where the real conversation ended.
#   - A user who ends both paths of the same pair gets one removed and one not, which
#     pulls their share towards 50%.
#   - [deleted] is excluded (it is not one user).
#
# Sections (output files are numbered to match):
#   1. Path endings: who writes the final comment, and how often it is removed
#   2. Removed share per user vs. chance; z-scores and labels
#   3. Repeat removal authors (counts of removed comments) alongside the labels
#   4. Characteristics of each label group (activity, role, style, context, reputation)
#   5. Save user-level labels
#
# Percentage axes run 0-100%; count axes put the tallest bar at ~80% of the axis.
#
# Run after 1_data_prep.R and 1b_pseudonymize_users.R (independent of script 3).


# === SECTION 0: PREP ===

# -- 0. Packages --
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggpubr, patchwork, scales)

# -- 1. Settings --
# TRUE  = paths with a post ID only (Aug 2017 onward, ~83% of pairs). Default.
# FALSE = full corpus.
USE_POST_ID_SAMPLE <- TRUE
MIN_ENDINGS        <- 5     # minimum path endings for a user to be labelled
Z_THRESHOLD        <- 2     # |z| at or beyond this = more / less often than chance

SAMPLE_TAG   <- if (USE_POST_ID_SAMPLE) "postid" else "full"
SAMPLE_LABEL <- if (USE_POST_ID_SAMPLE) "Paths with post ID (Aug 2017-Dec 2022)" else "Full corpus (Aug 2015-Dec 2022)"

# -- 2. Paths and helpers --
PREPPED          <- "prepped_data_anonymized"
SHARED_USER_DIR  <- "input_data_anonymized"
PRIVATE_USER_DIR <- "input_data_from_python"
KEY_FILE         <- file.path(PRIVATE_USER_DIR, "PRIVATE_user_id_key.csv")
DESC             <- file.path("descriptive_results", "removal_authors")
FIGS             <- file.path("graphical_output", "removal_authors")
dir.create(DESC, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGS, recursive = TRUE, showWarnings = FALSE)

fmt <- function(x) format(round(x, 2), big.mark = ",")

save_csv <- function(df, name) {
  filename <- paste0(name, "_", SAMPLE_TAG, ".csv")
  write_csv(df, file.path(DESC, filename))
  cat(sprintf("  -> saved: %s\n", filename))
  print(df, n = 50, width = Inf)
  invisible(df)
}

save_fig <- function(p, name, width = 9, height = 5) {
  filename <- paste0(name, "_", SAMPLE_TAG, ".png")
  ggsave(file.path(FIGS, filename), p, width = width, height = height, dpi = 300, bg = "white")
  cat(sprintf("  -> saved: %s\n", filename))
  invisible(p)
}

base_theme <- theme_pubr() +
  theme(
    plot.title    = element_text(face = "bold"),
    axis.title    = element_text(face = "bold"),
    plot.caption  = element_text(hjust = 0, colour = "#666666", size = 8),
    legend.title  = element_blank()
  )

col_neutral  <- "#5C85D6"
col_expected <- "#BDBDBD"

LABEL_MORE   <- "Removed more often than chance"
LABEL_WITHIN <- "Within chance"
LABEL_LESS   <- "Removed less often than chance"
LABEL_FEW    <- paste0("Fewer than ", MIN_ENDINGS, " path endings")
LABEL_LEVELS <- c(LABEL_MORE, LABEL_WITHIN, LABEL_LESS, LABEL_FEW)

col_labels <- setNames(c("#EF5350", "#9E9E9E", "#81C784", "#E0E0E0"), LABEL_LEVELS)

# Count axes: tallest bar at ~80% of the axis
scale_y_80 <- function(...) scale_y_continuous(..., expand = expansion(mult = c(0, 0.25)))

# Percentage axes: always the full 0-100% scale (max_val accepted but not used)
scale_y_pct <- function(max_val = NULL, ...) {
  list(
    scale_y_continuous(..., limits = c(0, 100), breaks = seq(0, 100, 20), expand = c(0, 0)),
    coord_cartesian(clip = "off")
  )
}

# Full (uncapped) breakdown of one count variable: n, %, cumulative %
uncapped_count <- function(df, key, n_name = "n_users") {
  df %>%
    count({{ key }}, name = "n") %>%
    arrange({{ key }}) %>%
    mutate(
      pct     = round(100 * n / sum(n), 2),
      cum_pct = round(100 * cumsum(n) / sum(n), 2)
    ) %>%
    rename(!!n_name := n)
}

# -- 3. Load data and apply sample --
message("Loading prepped data...")
ce <- readRDS(file.path(PREPPED, "comments_enriched.rds"))

if (USE_POST_ID_SAMPLE) {
  ce <- ce %>% filter(has_post_id)
}

# Per-comment fields needed below. Paths are linear (verified in 1_data_prep.R), so the
# previous comment in depth order is the parent.
ce <- ce %>%
  mutate(delta_count = as.numeric(str_extract(author_flair_text, "\\d+"))) %>%
  arrange(conversation_id, thread_depth) %>%
  group_by(conversation_id) %>%
  mutate(
    is_final        = thread_depth == max(thread_depth),
    reply_latency_h = (timestamp - lag(timestamp)) / 3600,
    n_speakers_path = n_distinct(speaker)
  ) %>%
  ungroup()

ce_known <- ce %>% filter(speaker != "[deleted]")

cap <- paste0("Sample: ", SAMPLE_LABEL, ". [deleted] excluded. ")

cat(sprintf("\nSample: %s | %s comments | %s identifiable users\n",
            SAMPLE_LABEL, fmt(nrow(ce)), fmt(n_distinct(ce_known$speaker))))


# === USER TABLE (built here, saved in section 5) ===

cat("\n=== USER TABLE ===\n")

users <- ce_known %>%
  group_by(speaker) %>%
  summarise(
    n_comments             = n(),
    n_paths                = n_distinct(conversation_id),
    n_threads              = n_distinct(post_id, na.rm = TRUE),
    n_first_replies        = sum(thread_depth == 0),
    n_endings              = sum(is_final),
    n_removed              = sum(is_removed_comment),
    n_removed_threads      = n_distinct(post_id[is_removed_comment], na.rm = TRUE),
    median_word_count      = median(word_count, na.rm = TRUE),
    median_reply_latency_h = median(reply_latency_h[thread_depth > 0], na.rm = TRUE),
    # Excludes the user's own path endings: endings are the deepest comment in a path,
    # and removed endings sit in (longer) awry paths, so including them would build the
    # label into the measure. NA if every comment the user wrote ends a path.
    mean_depth             = if (any(!is_final)) mean(thread_depth[!is_final]) else NA_real_,
    first_ts               = min(timestamp),
    last_ts                = max(timestamp),
    has_delta_flair        = any(!is.na(author_flair_text)),
    max_delta_flair        = suppressWarnings(max(delta_count, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(max_delta_flair = if_else(is.infinite(max_delta_flair), NA_real_, max_delta_flair))

# Path-level context for each user
path_stats <- ce_known %>%
  group_by(speaker, conversation_id) %>%
  summarise(
    n_in_path    = n(),
    ended        = any(is_final),
    awry         = first(has_removed_comment),
    two_speakers = first(n_speakers_path) == 2,
    .groups = "drop"
  ) %>%
  group_by(speaker) %>%
  summarise(
    share_paths_returned       = mean(n_in_path > 1),
    share_paths_two_speakers   = mean(two_speakers),
    # Excludes paths the user ended, so their own removals do not drive this measure
    share_awry_paths_not_ended = if (any(!ended)) mean(awry[!ended]) else NA_real_,
    .groups = "drop"
  )

# Activity groups: same definitions as script 3
q99 <- quantile(users$n_comments, 0.99)
q95 <- quantile(users$n_comments, 0.95)

users <- users %>%
  left_join(path_stats, by = "speaker") %>%
  mutate(
    active_days               = (last_ts - first_ts) / 86400,
    comments_per_active_month = n_comments / pmax(active_days / 30.44, 1),
    share_first_replies       = n_first_replies / n_comments,
    share_endings             = n_endings / n_comments,
    removed_share             = if_else(n_endings > 0, n_removed / n_endings, NA_real_),
    z                         = if_else(n_endings > 0,
                                        (n_removed - n_endings / 2) / sqrt(n_endings / 4),
                                        NA_real_),
    removal_label = factor(case_when(
      n_endings < MIN_ENDINGS ~ LABEL_FEW,
      z >=  Z_THRESHOLD       ~ LABEL_MORE,
      z <= -Z_THRESHOLD       ~ LABEL_LESS,
      TRUE                    ~ LABEL_WITHIN
    ), levels = LABEL_LEVELS),
    removal_count_band = factor(case_when(
      n_removed == 0 ~ "0",
      n_removed == 1 ~ "1",
      n_removed <= 4 ~ "2-4",
      TRUE           ~ "5+"
    ), levels = c("0", "1", "2-4", "5+")),
    activity_group = factor(case_when(
      n_comments >= q99 ~ "Top 1%",
      n_comments >= q95 ~ "Top 5% (excl. top 1%)",
      n_comments == 1   ~ "One comment",
      TRUE              ~ "Other (2+ comments, bottom 95%)"
    ), levels = c("One comment", "Other (2+ comments, bottom 95%)",
                  "Top 5% (excl. top 1%)", "Top 1%"))
  ) %>%
  select(-first_ts, -last_ts)


# === SECTION 1: PATH ENDINGS ===

cat("\n=== SECTION 1: PATH ENDINGS ===\n")

enders <- users %>% filter(n_endings > 0)

endings_summary <- tibble(
  Metric = c("Identifiable users", "Users who end at least one path",
             "% of users who end at least one path",
             "Path endings by identifiable users", "Of which removed (awry endings)",
             "Overall removed share of endings (50% by design)",
             paste0("Users with ", MIN_ENDINGS, "+ endings (eligible for a label)"),
             paste0("% of endings written by users with ", MIN_ENDINGS, "+ endings")),
  Value = c(
    fmt(nrow(users)),
    fmt(nrow(enders)),
    sprintf("%.1f%%", 100 * nrow(enders) / nrow(users)),
    fmt(sum(users$n_endings)),
    fmt(sum(users$n_removed)),
    sprintf("%.1f%%", 100 * sum(users$n_removed) / sum(users$n_endings)),
    fmt(sum(users$n_endings >= MIN_ENDINGS)),
    sprintf("%.1f%%", 100 * sum(users$n_endings[users$n_endings >= MIN_ENDINGS]) / sum(users$n_endings))
  )
)

save_csv(endings_summary, "1_path_endings_summary")
save_csv(uncapped_count(enders, n_endings), "1_endings_per_user_uncapped")

ending_bins   <- c(0, 1, 2, 4, 9, 19, Inf)
ending_labels <- c("1", "2", "3-4", "5-9", "10-19", "20+")

endings_dist <- enders %>%
  mutate(bin = cut(n_endings, breaks = ending_bins, labels = ending_labels)) %>%
  count(bin, name = "n_users") %>%
  mutate(pct = round(100 * n_users / sum(n_users), 1))

save_csv(endings_dist, "1_endings_per_user")

p_endings <- endings_dist %>%
  ggplot(aes(x = bin, y = pct)) +
  geom_col(fill = col_neutral, width = 0.7) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.4, size = 3.2) +
  scale_y_pct(max(endings_dist$pct)) +
  labs(title = "Path endings per user",
       subtitle = "Paths whose final comment the user wrote (awry or on-track)",
       x = "Path endings written", y = "% of users who end at least one path",
       caption = paste0(cap, "Only users with ", MIN_ENDINGS, "+ endings are labelled in section 2.")) +
  base_theme

save_fig(p_endings, "1_endings_per_user")


# === SECTION 2: REMOVED SHARE VS CHANCE ===

cat("\n=== SECTION 2: REMOVED SHARE VS CHANCE ===\n")

eligible <- users %>% filter(n_endings >= MIN_ENDINGS)

# -- 2a. Distribution of removed share vs binomial expectation --
share_breaks <- seq(0, 1, 0.1)
share_labels <- paste0(seq(0, 90, 10), "-", seq(10, 100, 10), "%")
bin_share <- function(s) cut(s, breaks = share_breaks, labels = share_labels,
                             include.lowest = TRUE, right = FALSE)

observed_share <- eligible %>%
  mutate(share_bin = bin_share(removed_share)) %>%
  count(share_bin, name = "n_users") %>%
  mutate(type = "Observed")

expected_share <- eligible %>%
  select(speaker, n_endings) %>%
  mutate(dist = map(n_endings, ~ tibble(x = 0:.x, p = dbinom(0:.x, .x, 0.5)))) %>%
  unnest(dist) %>%
  mutate(share_bin = bin_share(x / n_endings)) %>%
  group_by(share_bin) %>%
  summarise(n_users = sum(p), .groups = "drop") %>%
  mutate(type = "Expected by chance")

share_dist <- bind_rows(observed_share, expected_share) %>%
  group_by(type) %>%
  mutate(pct = round(100 * n_users / sum(n_users), 1)) %>%
  ungroup() %>%
  complete(type, share_bin, fill = list(n_users = 0, pct = 0)) %>%
  mutate(type = factor(type, levels = c("Observed", "Expected by chance")))

save_csv(share_dist, "2a_removed_share_distribution")

p_share <- share_dist %>%
  ggplot(aes(x = share_bin, y = pct, fill = type)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  scale_fill_manual(values = c("Observed" = col_neutral, "Expected by chance" = col_expected)) +
  scale_y_pct(max(share_dist$pct)) +
  labs(title = "Share of each user's path endings that were removed",
       subtitle = paste0("Users with ", MIN_ENDINGS, "+ path endings. 50% expected by design."),
       x = "% of the user's path endings that were removed", y = "% of users",
       caption = paste0(cap, "Expected = binomial(n endings, 0.5) for each user, summed across users.")) +
  base_theme +
  theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1))

save_fig(p_share, "2a_removed_share_distribution")

# -- 2b. z-scores, labels, and how many to expect by chance --
# Under chance, the probability that a user with n endings reaches z >= +Z or z <= -Z,
# summed over eligible users, gives the expected number of labelled users.
n_e     <- eligible$n_endings
x_hi    <- ceiling(n_e / 2 + Z_THRESHOLD * sqrt(n_e) / 2)
x_lo    <- floor(n_e / 2 - Z_THRESHOLD * sqrt(n_e) / 2)
exp_hi  <- sum(pbinom(x_hi - 1, n_e, 0.5, lower.tail = FALSE))
exp_lo  <- sum(pbinom(x_lo, n_e, 0.5))

label_counts <- users %>%
  count(removal_label, name = "n_users", .drop = FALSE) %>%
  mutate(
    pct_of_all_users      = round(100 * n_users / sum(n_users), 1),
    pct_of_eligible       = if_else(removal_label == LABEL_FEW, NA_real_,
                                    round(100 * n_users / nrow(eligible), 1)),
    expected_by_chance    = case_when(removal_label == LABEL_MORE ~ round(exp_hi, 1),
                                      removal_label == LABEL_LESS ~ round(exp_lo, 1),
                                      TRUE ~ NA_real_)
  )

save_csv(label_counts, "2b_removal_labels")

z_summary <- eligible %>%
  summarise(
    n_users          = n(),
    mean_z           = round(mean(z), 2),
    sd_z_observed    = round(sd(z), 2),
    sd_z_if_chance   = 1,
    pct_z_ge_2       = round(100 * mean(z >= Z_THRESHOLD), 1),
    pct_z_le_minus_2 = round(100 * mean(z <= -Z_THRESHOLD), 1),
    max_z            = round(max(z), 2)
  )

save_csv(z_summary, "2b_z_score_summary")

p_z <- eligible %>%
  mutate(removal_label = droplevels(removal_label)) %>%
  ggplot(aes(x = z, fill = removal_label)) +
  geom_histogram(binwidth = 0.5, boundary = 0, colour = "white") +
  geom_vline(xintercept = c(-Z_THRESHOLD, Z_THRESHOLD), linetype = "dashed", colour = "#333333") +
  scale_fill_manual(values = col_labels) +
  scale_y_80(labels = comma) +
  labs(title = "How far is each user from the 50% expected by chance?",
       subtitle = paste0("z = (removed - endings/2) / sqrt(endings/4). Dashed lines at +/-", Z_THRESHOLD, "."),
       x = "z-score", y = "Number of users",
       caption = paste0(cap, "Users with ", MIN_ENDINGS, "+ path endings. Under chance about 2.3% fall beyond each line; ",
                        "see 2b_removal_labels for expected counts.")) +
  base_theme +
  theme(legend.position = "top")

save_fig(p_z, "2b_z_score_distribution")

label_by_group <- users %>%
  count(activity_group, removal_label, name = "n_users") %>%
  group_by(activity_group) %>%
  mutate(pct = round(100 * n_users / sum(n_users), 1)) %>%
  ungroup()

save_csv(label_by_group, "2c_removal_labels_by_activity_group")


# === SECTION 3: REPEAT REMOVAL AUTHORS ===

cat("\n=== SECTION 3: REPEAT REMOVAL AUTHORS ===\n")

# Raw counts of removed comments (not adjusted for opportunity), cross-tabulated with the
# opportunity-adjusted labels: repeat authors are not necessarily removed more than chance.
repeat_by_label <- users %>%
  count(removal_count_band, removal_label, name = "n_users") %>%
  group_by(removal_count_band) %>%
  mutate(pct_of_band = round(100 * n_users / sum(n_users), 1)) %>%
  ungroup()

save_csv(repeat_by_label, "3_repeat_removal_authors_by_label")

repeat_summary <- users %>%
  group_by(removal_count_band) %>%
  summarise(
    n_users              = n(),
    total_removed        = sum(n_removed),
    median_comments      = median(n_comments),
    median_endings       = median(n_endings),
    median_removed_share = round(100 * median(removed_share, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  mutate(pct_of_removals = round(100 * total_removed / sum(total_removed), 1))

save_csv(repeat_summary, "3_repeat_removal_authors_summary")

p_repeat <- repeat_by_label %>%
  filter(removal_count_band != "0") %>%
  ggplot(aes(x = removal_count_band, y = pct_of_band, fill = removal_label)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = col_labels) +
  scale_y_continuous(limits = c(0, 101), breaks = seq(0, 100, 20), expand = c(0, 0)) +   # 101: rounding headroom
  labs(title = "Repeat removal authors: removed more often than chance?",
       x = "Removed comments written", y = "% of users in band",
       caption = paste0(cap, "Bands use raw counts; labels adjust for how many paths each user ended.")) +
  base_theme +
  theme(legend.position = "top") +
  guides(fill = guide_legend(nrow = 2))

save_fig(p_repeat, "3_repeat_removal_authors_by_label")


# === SECTION 4: CHARACTERISTICS BY LABEL ===

cat("\n=== SECTION 4: CHARACTERISTICS BY LABEL ===\n")

# Groups compared: the three labels among users with MIN_ENDINGS+ endings, so all groups
# are users who end paths regularly (a rough control for activity). Users with fewer
# endings are kept in the table for reference only.
characteristics <- tribble(
  ~var,                          ~label,                                          ~scale,
  "n_comments",                  "Comments",                                      1,
  "comments_per_active_month",   "Comments per active month",                     1,
  "active_days",                 "Time on forum (days)",                          1,
  "share_first_replies",         "% of comments that are first replies",          100,
  "share_endings",               "% of comments that end a path",                 100,
  "share_paths_returned",        "% of paths where they comment more than once",  100,
  "median_word_count",           "Median words per comment",                      1,
  "median_reply_latency_h",      "Median reply time (hours)",                     1,
  "mean_depth",                  "Mean depth (excluding their path endings)",     1,
  "share_paths_two_speakers",    "% of their paths with only two speakers",       100,
  "share_awry_paths_not_ended",  "% of paths they did not end that went awry",    100,
  "max_delta_flair",             "Career deltas (users with flair)",              1
)

char_long <- users %>%
  select(speaker, removal_label, all_of(characteristics$var)) %>%
  pivot_longer(-c(speaker, removal_label), names_to = "var", values_to = "value") %>%
  left_join(characteristics, by = "var") %>%
  mutate(value = value * scale)

char_table <- char_long %>%
  group_by(label, removal_label) %>%
  summarise(
    n_users = sum(!is.na(value)),
    median  = round(median(value, na.rm = TRUE), 2),
    p25     = round(quantile(value, 0.25, na.rm = TRUE), 2),
    p75     = round(quantile(value, 0.75, na.rm = TRUE), 2),
    mean    = round(mean(value, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(label = factor(label, levels = characteristics$label)) %>%
  arrange(label, removal_label)

flair_share <- users %>%
  group_by(removal_label) %>%
  summarise(n_users = n(), pct_with_delta_flair = round(100 * mean(has_delta_flair), 1),
            .groups = "drop")

save_csv(char_table, "4_characteristics_by_label")
save_csv(flair_share, "4_delta_flair_share_by_label")

p_char <- char_table %>%
  filter(removal_label != LABEL_FEW) %>%
  ggplot(aes(x = removal_label, y = median, fill = removal_label)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = p25, ymax = p75), width = 0.2, colour = "#333333") +
  facet_wrap(~ label, scales = "free_y", ncol = 4, labeller = label_wrap_gen(28)) +
  scale_fill_manual(values = col_labels) +
  scale_y_80() +
  labs(title = "Characteristics of users by removal label",
       subtitle = "Bars = median; lines = 25th-75th percentile",
       x = NULL, y = NULL,
       caption = paste0(cap, "Users with ", MIN_ENDINGS, "+ path endings. ",
                        "Time on forum (first to last comment) is measured within this dataset. ",
                        "Delta flair share by label is in 4_delta_flair_share_by_label.")) +
  base_theme +
  theme(legend.position = "top",
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text   = element_text(face = "bold", size = 8)) +
  guides(fill = guide_legend(nrow = 1))

save_fig(p_char, "4_characteristics_by_label", width = 13, height = 9)

# Removal spread: are a user's removals concentrated in one thread? (users with 2+ removals)
spread <- users %>%
  filter(n_removed >= 2) %>%
  mutate(share_removals_distinct_threads = n_removed_threads / n_removed) %>%
  group_by(removal_label) %>%
  summarise(
    n_users                        = n(),
    median_removed                 = median(n_removed),
    median_share_distinct_threads  = round(100 * median(share_removals_distinct_threads, na.rm = TRUE), 1),
    pct_all_removals_same_thread   = round(100 * mean(n_removed_threads == 1, na.rm = TRUE), 1),
    .groups = "drop"
  )

save_csv(spread, "4_removal_spread_across_threads")


# === SECTION 5: SAVE USER-LEVEL LABELS ===

label_cols <- c("speaker", "activity_group", "n_comments", "n_endings", "n_removed",
                "removed_share", "z", "removal_label", "removal_count_band")

shared_file <- file.path(SHARED_USER_DIR, paste0("removal_author_labels_", SAMPLE_TAG, ".csv"))
write_csv(users %>% select(all_of(label_cols), everything()), shared_file)
cat(sprintf("\n  -> labels (IDs) saved: %s (%s users)\n", shared_file, fmt(nrow(users))))

if (file.exists(KEY_FILE)) {
  key <- read_csv(KEY_FILE, col_types = cols(.default = col_character()), na = "")
  users_identified <- users %>%
    left_join(key %>% select(user_id, username = speaker), by = c("speaker" = "user_id")) %>%
    mutate(speaker = coalesce(username, speaker)) %>%
    select(-username) %>%
    select(all_of(label_cols), everything())
  private_file <- file.path(PRIVATE_USER_DIR, paste0("removal_author_labels_", SAMPLE_TAG, ".csv"))
  write_csv(users_identified, private_file)
  cat(sprintf("  -> labels (usernames) saved: %s\n", private_file))
} else {
  cat("  Private key not found - username version not written.\n")
}

cat("\nDone: removal-author descriptives saved to", DESC, "and", FIGS, "\n")