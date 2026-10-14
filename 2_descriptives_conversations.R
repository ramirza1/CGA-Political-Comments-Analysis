# 2. descriptives_conversations.R for Conversations Gone Awry (CMV) - Political Comments Analysis
#
# Conversation-level descriptives: how threads, pairs, and paths are structured, and
# what the awry/on-track pairing does and does not hold constant. No text coding.
#
# Taxonomy (ConvoKit term in brackets):
#   Thread       - an original post and all comments beneath it            [post_id]
#   First reply  - a comment replying directly to the original post        [thread_depth == 0]
#   Challenger   - author of the first reply
#   Path         - a linear chain of comments from a first reply downwards [conversation]
#   Pair         - two paths from different branches of the same thread,
#                  one ending in a removed comment (awry), one not (on-track) [pair_key]
#
# Sections:
#   1. Nesting overview + by-construction confirmations
#   2. Depth, two ways: comments by depth position; paths by length
#   3. Threads: pairs per thread
#   4. Within-pair comparison: do the two paths in a pair run to the same length?
#   5. Relative timing: which path in a pair started first; position within thread
#   6. Participation: speakers per path; two-person paths; the challenger's role
#   7. Final comment: who wrote it and who it replies to (awry = removed comment)
#      (Depth of the removed comment is covered by path length in section 2b.)
#
# Figures group long tails (e.g. "6+") for readability; every grouped figure has a
# matching *_uncapped or *_detailed table with the full breakdown for precise statistics.
#
# Outputs:
#   descriptive_results/conversations/*_<sample>.csv
#   graphical_output/conversations/*_<sample>.png
#
# Run after 1_data_prep.R and 1b_pseudonymize_users.R


# === SECTION 0: PREP ===

# -- 0. Packages --
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggpubr, patchwork, scales)

# -- 1. Sample switch --
# TRUE  = paths with a post ID only (Aug 2017 onward, ~83% of pairs). Default.
# FALSE = full corpus. Thread-level sections (3, 5b) always use paths with a post ID.
USE_POST_ID_SAMPLE <- TRUE

SAMPLE_TAG   <- if (USE_POST_ID_SAMPLE) "postid" else "full"
SAMPLE_LABEL <- if (USE_POST_ID_SAMPLE) "Paths with post ID (Aug 2017-Dec 2022)" else "Full corpus (Aug 2015-Dec 2022)"

# -- 2. Paths and helpers --
PREPPED <- "prepped_data_anonymized"   # shareable copy with user IDs (from 1b_pseudonymize_users.R)
DESC    <- file.path("descriptive_results", "conversations")
FIGS    <- file.path("graphical_output", "conversations")
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

col_awry    <- "#EF5350"
col_ontrack <- "#81C784"
col_neutral <- "#5C85D6"
col_outcome <- c("Awry" = col_awry, "On-track" = col_ontrack)

# Y axis for bar charts: the tallest bar sits at ~80% of the axis height
scale_y_80 <- function(...) scale_y_continuous(..., expand = expansion(mult = c(0, 0.25)))

# Full (uncapped) breakdown of a count variable by outcome, one row per value:
#   n_awry, n_ontrack, pct_*, cum_pct_* (percentages within outcome)
uncapped_by_outcome <- function(df, key) {
  df %>%
    mutate(o = if_else(outcome == "Awry", "awry", "ontrack")) %>%
    count(o, {{ key }}, name = "n") %>%
    complete(o, {{ key }}, fill = list(n = 0L)) %>%
    arrange(o, {{ key }}) %>%
    group_by(o) %>%
    mutate(
      pct     = round(100 * n / sum(n), 2),
      cum_pct = round(100 * cumsum(n) / sum(n), 2)
    ) %>%
    ungroup() %>%
    pivot_wider(names_from = o, values_from = c(n, pct, cum_pct)) %>%
    arrange({{ key }})
}

# Hour bins used for timing figures (section 5): grouped after 1 day
HOUR_BREAKS <- c(0, 1, 3, 6, 12, 24, Inf)
HOUR_LABELS <- c("0-1 hr", "1-3 hrs", "3-6 hrs", "6-12 hrs", "12-24 hrs", ">1 day")

# Finer bins for the timing tables
FINE_BREAKS <- c(0, 5/60, 15/60, 30/60, 1, 3, 6, 12, 24, 72, 168, Inf)
FINE_LABELS <- c("<5 min", "5-15 min", "15-30 min", "30-60 min", "1-3 hrs", "3-6 hrs",
                 "6-12 hrs", "12-24 hrs", "1-3 days", "3-7 days", ">1 week")

# -- 3. Load data and apply sample --
message("Loading prepped data...")
ce   <- readRDS(file.path(PREPPED, "comments_enriched.rds"))
conv <- readRDS(file.path(PREPPED, "conversations.rds"))

if (USE_POST_ID_SAMPLE) {
  ce   <- ce   %>% filter(has_post_id)
  conv <- conv %>% filter(has_post_id)
}

ce <- ce %>%
  mutate(outcome = factor(if_else(has_removed_comment, "Awry", "On-track"),
                          levels = c("Awry", "On-track")))

cat(sprintf("\nSample: %s | %s comments | %s paths\n",
            SAMPLE_LABEL, fmt(nrow(ce)), fmt(nrow(conv))))

# -- 4. Path table (one row per path) --
# Paths are strictly linear (verified in 1_data_prep.R, section 7a), so ordering
# by thread_depth gives the reply order.
paths <- ce %>%
  arrange(conversation_id, thread_depth) %>%
  group_by(conversation_id) %>%
  summarise(
    pair_key              = first(pair_key),
    post_id               = first(post_id),
    post_slug             = first(post_slug),
    period                = first(period),
    outcome               = first(outcome),
    n_comments            = n(),
    max_depth             = max(thread_depth),
    n_depth0              = sum(thread_depth == 0),
    n_speakers            = n_distinct(speaker),
    has_deleted_speaker   = any(speaker == "[deleted]"),
    challenger            = speaker[thread_depth == 0][1],
    start_ts              = timestamp[thread_depth == 0][1],
    end_ts                = max(timestamp),
    challenger_n_comments = sum(speaker == challenger),
    .groups = "drop"
  ) %>%
  mutate(
    challenger_returns = challenger_n_comments > 1,
    challenger_share   = challenger_n_comments / n_comments
  )

# Caption used on every figure
cap <- paste0("Sample: ", SAMPLE_LABEL, ". ")


# === SECTION 1: NESTING OVERVIEW + BY-CONSTRUCTION CONFIRMATIONS ===

cat("\n=== SECTION 1: NESTING OVERVIEW ===\n")

overview <- tibble(
  Level = c("Comments", "Paths (ConvoKit conversations)", "Pairs",
            "Threads (posts)", "Unique users (excl. [deleted])"),
  N = c(
    nrow(ce),
    nrow(paths),
    n_distinct(paths$pair_key),
    n_distinct(na.omit(paths$post_id)),
    n_distinct(ce$speaker[ce$speaker != "[deleted]"])
  )
) %>%
  mutate(N_formatted = fmt(N))

save_csv(overview, "1_nesting_overview")

# Features that hold by design - confirmed, not plotted
pair_checks <- paths %>%
  group_by(pair_key) %>%
  summarise(
    n_paths   = n(),
    n_awry    = sum(outcome == "Awry"),
    n_threads = n_distinct(post_id, na.rm = TRUE),
    .groups = "drop"
  )

confirmations <- tibble(
  Check = c(
    "Every pair has exactly 2 paths",
    "Every pair has exactly 1 awry path",
    "Every path has exactly 1 first reply (depth 0)",
    "Every path is linear (length = max depth + 1)",
    "No pair spans more than one thread (pairs with post ID)",
    "Awry share of paths"
  ),
  Result = c(
    sprintf("%s of %s pairs", fmt(sum(pair_checks$n_paths == 2)), fmt(nrow(pair_checks))),
    sprintf("%s of %s pairs", fmt(sum(pair_checks$n_awry == 1)),  fmt(nrow(pair_checks))),
    sprintf("%s of %s paths", fmt(sum(paths$n_depth0 == 1)),       fmt(nrow(paths))),
    sprintf("%s of %s paths", fmt(sum(paths$n_comments == paths$max_depth + 1)), fmt(nrow(paths))),
    sprintf("%s pairs span >1 thread", fmt(sum(pair_checks$n_threads > 1))),
    sprintf("%.1f%%", 100 * mean(paths$outcome == "Awry"))
  )
)

save_csv(confirmations, "1_by_construction_checks")


# === SECTION 2: DEPTH, TWO WAYS ===

cat("\n=== SECTION 2: DEPTH ===\n")

# -- 2a. Comments by depth position --
# Depth 0 = first reply (replies to the post); depth 1 = reply to the first reply; etc.
DEPTH_CAP <- 15

comments_by_depth <- ce %>%
  mutate(depth_bin = pmin(thread_depth, DEPTH_CAP)) %>%
  count(outcome, depth_bin, name = "n_comments") %>%
  group_by(outcome) %>%
  mutate(pct = round(100 * n_comments / sum(n_comments), 1)) %>%
  ungroup()

save_csv(comments_by_depth, "2a_comments_by_depth")
save_csv(uncapped_by_outcome(ce, thread_depth), "2a_comments_by_depth_uncapped")

p_depth <- comments_by_depth %>%
  ggplot(aes(x = depth_bin, y = n_comments, fill = outcome)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  scale_fill_manual(values = col_outcome) +
  scale_x_continuous(breaks = 0:DEPTH_CAP,
                     labels = c(0:(DEPTH_CAP - 1), paste0(DEPTH_CAP, "+"))) +
  scale_y_80(labels = comma) +
  labs(title = "Comments by depth position in their path",
       x = "Depth (0 = first reply to the post)", y = "Number of comments",
       caption = paste0(cap, "Depth 0 and 1 are equal in size by construction: every path has one first reply and at least one reply to it.")) +
  base_theme

save_fig(p_depth, "2a_comments_by_depth")

# -- 2b. Paths by length (how many comments deep each path runs) --
LENGTH_CAP <- 16

path_length_dist <- paths %>%
  mutate(length_bin = pmin(n_comments, LENGTH_CAP)) %>%
  count(outcome, length_bin, name = "n_paths") %>%
  group_by(outcome) %>%
  mutate(pct = round(100 * n_paths / sum(n_paths), 1)) %>%
  ungroup()

save_csv(path_length_dist, "2b_path_length_distribution")
save_csv(uncapped_by_outcome(paths, n_comments), "2b_path_length_uncapped")

path_length_summary <- paths %>%
  group_by(outcome) %>%
  summarise(
    n_paths = n(),
    mean    = round(mean(n_comments), 2),
    median  = median(n_comments),
    p25     = quantile(n_comments, 0.25),
    p75     = quantile(n_comments, 0.75),
    p90     = quantile(n_comments, 0.90),
    max     = max(n_comments),
    .groups = "drop"
  )

save_csv(path_length_summary, "2b_path_length_summary")

p_length <- path_length_dist %>%
  ggplot(aes(x = length_bin, y = pct, fill = outcome)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  scale_fill_manual(values = col_outcome) +
  scale_x_continuous(breaks = 2:LENGTH_CAP,
                     labels = c(2:(LENGTH_CAP - 1), paste0(LENGTH_CAP, "+"))) +
  scale_y_80() +
  labs(title = "Path length: number of comments in each path",
       x = "Comments in path", y = "% of paths (within outcome)",
       caption = cap) +
  base_theme

save_fig(p_length, "2b_path_length_distribution")


# === SECTION 3: THREADS ===

cat("\n=== SECTION 3: THREADS ===\n")

# Always restricted to paths with a post ID
paths_thread <- paths %>% filter(!is.na(post_id))

thread_sizes <- paths_thread %>%
  group_by(post_id, post_slug) %>%
  summarise(
    n_pairs = n_distinct(pair_key),
    n_paths = n(),
    .groups = "drop"
  )

PAIRS_CAP <- 6

pairs_per_thread <- thread_sizes %>%
  mutate(n_pairs_bin = pmin(n_pairs, PAIRS_CAP)) %>%
  group_by(n_pairs_bin) %>%
  summarise(
    n_threads = n(),
    n_pairs   = sum(n_pairs),
    .groups = "drop"
  ) %>%
  mutate(
    pct_threads = round(100 * n_threads / sum(n_threads), 1),
    pct_pairs   = round(100 * n_pairs / sum(n_pairs), 1),
    n_pairs_label = if_else(n_pairs_bin == PAIRS_CAP, paste0(PAIRS_CAP, "+"), as.character(n_pairs_bin))
  )

save_csv(pairs_per_thread, "3_pairs_per_thread")

pairs_per_thread_uncapped <- thread_sizes %>%
  count(n_pairs, name = "n_threads") %>%
  mutate(
    pct_threads     = round(100 * n_threads / sum(n_threads), 2),
    cum_pct_threads = round(100 * cumsum(n_threads) / sum(n_threads), 2),
    pairs_in_bin    = n_pairs * n_threads,
    pct_pairs       = round(100 * pairs_in_bin / sum(pairs_in_bin), 2)
  )

save_csv(pairs_per_thread_uncapped, "3_pairs_per_thread_uncapped")

thread_summary <- tibble(
  Metric = c("Threads", "Pairs", "Mean pairs per thread", "Median pairs per thread",
             "Max pairs in one thread", "% of threads with >1 pair",
             "% of pairs in threads with >1 pair"),
  Value = c(
    fmt(nrow(thread_sizes)),
    fmt(sum(thread_sizes$n_pairs)),
    fmt(mean(thread_sizes$n_pairs)),
    fmt(median(thread_sizes$n_pairs)),
    fmt(max(thread_sizes$n_pairs)),
    sprintf("%.1f%%", 100 * mean(thread_sizes$n_pairs > 1)),
    sprintf("%.1f%%", 100 * sum(thread_sizes$n_pairs[thread_sizes$n_pairs > 1]) / sum(thread_sizes$n_pairs))
  )
)

save_csv(thread_summary, "3_thread_summary")

# Largest threads (internal reference - post titles from permalinks)
top_threads <- thread_sizes %>%
  arrange(desc(n_pairs)) %>%
  slice_head(n = 20)

save_csv(top_threads, "3_top20_threads_by_pairs")

p_threads <- pairs_per_thread %>%
  ggplot(aes(x = factor(n_pairs_label, levels = n_pairs_label), y = n_threads)) +
  geom_col(fill = col_neutral, width = 0.7) +
  geom_text(aes(label = paste0(comma(n_threads), "\n(", pct_threads, "%)")),
            vjust = -0.3, size = 3.2) +
  scale_y_80(labels = comma) +
  labs(title = "Pairs per thread",
       x = "Number of pairs drawn from the thread", y = "Number of threads",
       caption = "Paths with post ID only (Aug 2017-Dec 2022). Pairs from the same thread share a topic and an original poster.") +
  base_theme

save_fig(p_threads, "3_pairs_per_thread")


# === SECTION 4: WITHIN-PAIR COMPARISON ===

cat("\n=== SECTION 4: WITHIN-PAIR COMPARISON ===\n")

pairs_wide <- paths %>%
  mutate(o = if_else(outcome == "Awry", "awry", "ontrack")) %>%
  select(pair_key, post_id, o, n_comments, n_speakers, start_ts) %>%
  pivot_wider(names_from = o, values_from = c(n_comments, n_speakers, start_ts)) %>%
  mutate(
    length_diff     = n_comments_awry - n_comments_ontrack,
    start_gap_hours = (start_ts_awry - start_ts_ontrack) / 3600
  )

# -- 4a. Length difference: awry minus on-track --
length_diff_summary <- pairs_wide %>%
  summarise(
    n_pairs              = n(),
    pct_awry_longer      = round(100 * mean(length_diff > 0), 1),
    pct_same_length      = round(100 * mean(length_diff == 0), 1),
    pct_awry_shorter     = round(100 * mean(length_diff < 0), 1),
    mean_diff            = round(mean(length_diff), 2),
    median_diff          = median(length_diff),
    pct_within_1_comment = round(100 * mean(abs(length_diff) <= 1), 1)
  )

save_csv(length_diff_summary, "4a_pair_length_difference_summary")

DIFF_CAP <- 8

length_diff_dist <- pairs_wide %>%
  mutate(diff_bin = pmax(pmin(length_diff, DIFF_CAP), -DIFF_CAP)) %>%
  count(diff_bin, name = "n_pairs") %>%
  mutate(
    pct       = round(100 * n_pairs / sum(n_pairs), 1),
    direction = case_when(diff_bin > 0 ~ "Awry longer",
                          diff_bin < 0 ~ "Awry shorter",
                          TRUE         ~ "Same length")
  )

save_csv(length_diff_dist, "4a_pair_length_difference_distribution")

length_diff_uncapped <- pairs_wide %>%
  count(length_diff, name = "n_pairs") %>%
  mutate(
    pct     = round(100 * n_pairs / sum(n_pairs), 2),
    cum_pct = round(100 * cumsum(n_pairs) / sum(n_pairs), 2)
  )

save_csv(length_diff_uncapped, "4a_pair_length_difference_uncapped")

p_diff <- length_diff_dist %>%
  ggplot(aes(x = diff_bin, y = pct, fill = direction)) +
  geom_col(width = 0.8) +
  scale_fill_manual(values = c("Awry longer" = col_awry, "Awry shorter" = col_ontrack,
                               "Same length" = "#9E9E9E")) +
  scale_x_continuous(breaks = -DIFF_CAP:DIFF_CAP,
                     labels = c(paste0("<=", -DIFF_CAP), (-DIFF_CAP + 1):(DIFF_CAP - 1),
                                paste0(">=", DIFF_CAP))) +
  scale_y_80() +
  labs(title = "Do the two paths in a pair run to the same length?",
       subtitle = "Length difference within each pair (awry minus on-track, in comments)",
       x = "Awry path length minus on-track path length", y = "% of pairs",
       caption = cap) +
  base_theme +
  theme(legend.position = "top")

save_fig(p_diff, "4a_pair_length_difference")

# -- 4c. Cumulative closeness: share of pairs within k comments of each other --
# Ignores direction (awry longer or shorter): absolute length difference only.
WITHIN_MAX <- DIFF_CAP - 1   # bars: same length, within 1..7, then 8+ (matches the 4a cap)

# Final bar "8+" closes the cumulative series at 100% (all pairs, any difference);
# n_pairs_this_step shows how many pairs each step adds (for 8+: pairs differing by 8 or more).
length_within_k <- tibble(k = 0:WITHIN_MAX) %>%
  mutate(
    n_pairs_within = map_int(k, ~ sum(abs(pairs_wide$length_diff) <= .x)),
    label          = if_else(k == 0, "Same length", paste0("Within ", k))
  ) %>%
  bind_rows(tibble(k = WITHIN_MAX + 1, n_pairs_within = nrow(pairs_wide),
                   label = paste0(DIFF_CAP, "+"))) %>%
  mutate(
    pct_within        = round(100 * n_pairs_within / nrow(pairs_wide), 2),
    n_pairs_this_step = n_pairs_within - lag(n_pairs_within, default = 0L)
  )

save_csv(length_within_k, "4c_pair_length_within_k")

p_within <- length_within_k %>%
  ggplot(aes(x = factor(label, levels = label), y = pct_within)) +
  geom_col(fill = col_neutral, width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", pct_within)), vjust = -0.4, size = 3.2) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  labs(title = "How close in length are the two paths in a pair?",
       subtitle = "Cumulative share of pairs whose paths differ by at most k comments (either direction)",
       x = "Length difference between the two paths (comments)", y = "% of pairs",
       caption = cap) +
  base_theme

save_fig(p_within, "4c_pair_length_within_k")

# -- 4b. Length cross-tab: awry length vs on-track length --
XT_CAP <- 12

length_crosstab <- pairs_wide %>%
  mutate(
    awry_bin    = pmin(n_comments_awry, XT_CAP),
    ontrack_bin = pmin(n_comments_ontrack, XT_CAP)
  ) %>%
  count(awry_bin, ontrack_bin, name = "n_pairs")

save_csv(length_crosstab, "4b_pair_length_crosstab")
save_csv(pairs_wide %>% count(n_comments_awry, n_comments_ontrack, name = "n_pairs"),
         "4b_pair_length_crosstab_uncapped")

p_crosstab <- length_crosstab %>%
  ggplot(aes(x = ontrack_bin, y = awry_bin, fill = n_pairs)) +
  geom_tile(colour = "white") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "#333333") +
  scale_fill_gradient(low = "#E3F2FD", high = "#0D47A1", trans = "log10", labels = comma,
                      name = "Pairs") +
  scale_x_continuous(breaks = 2:XT_CAP, labels = c(2:(XT_CAP - 1), paste0(XT_CAP, "+"))) +
  scale_y_continuous(breaks = 2:XT_CAP, labels = c(2:(XT_CAP - 1), paste0(XT_CAP, "+"))) +
  coord_equal() +
  labs(title = "Path length within pairs",
       subtitle = "Dashed line = both paths the same length",
       x = "On-track path length (comments)", y = "Awry path length (comments)",
       caption = cap) +
  base_theme +
  theme(legend.position = "right", legend.title = element_text(face = "bold"))

save_fig(p_crosstab, "4b_pair_length_crosstab", width = 7, height = 6.5)


# === SECTION 5: RELATIVE TIMING ===

cat("\n=== SECTION 5: RELATIVE TIMING ===\n")

# Thread (post) creation time is not in the data, so timing is measured relative to
# the other path in the pair (5a) and to the other sampled paths in the thread (5b).
# "Start" = timestamp of the path's first reply.

# -- 5a. Within pair: which path started first, and by how much? --
# Figure: absolute gap in hour bins (grouped after 1 day), split by which path started first.
# Table: finer bins (minutes to weeks) for precise statistics.
first_levels <- c("Awry path first", "On-track path first", "Same time")

pairs_gap <- pairs_wide %>%
  mutate(
    abs_gap_hours = abs(start_gap_hours),
    first = factor(case_when(start_gap_hours < 0 ~ "Awry path first",
                             start_gap_hours > 0 ~ "On-track path first",
                             TRUE                ~ "Same time"),
                   levels = first_levels),
    gap_bin  = cut(abs_gap_hours, breaks = HOUR_BREAKS, labels = HOUR_LABELS, right = FALSE),
    gap_fine = cut(abs_gap_hours, breaks = FINE_BREAKS, labels = FINE_LABELS, right = FALSE)
  )

start_gap_dist <- pairs_gap %>%
  count(gap_bin, first, name = "n_pairs") %>%
  mutate(pct = round(100 * n_pairs / sum(n_pairs), 1))

save_csv(start_gap_dist, "5a_pair_start_gap_distribution")

start_gap_detailed <- pairs_gap %>%
  count(gap_fine, first, name = "n_pairs") %>%
  pivot_wider(names_from = first, values_from = n_pairs, values_fill = 0L) %>%
  arrange(gap_fine) %>%
  mutate(
    total   = rowSums(across(any_of(first_levels))),
    pct     = round(100 * total / sum(total), 2),
    cum_pct = round(100 * cumsum(total) / sum(total), 2)
  )

save_csv(start_gap_detailed, "5a_pair_start_gap_detailed")

start_gap_summary <- pairs_wide %>%
  summarise(
    n_pairs                   = n(),
    pct_awry_started_first    = round(100 * mean(start_gap_hours < 0), 1),
    pct_ontrack_started_first = round(100 * mean(start_gap_hours > 0), 1),
    pct_same_time             = round(100 * mean(start_gap_hours == 0), 1),
    median_abs_gap_hours      = round(median(abs(start_gap_hours)), 2),
    pct_within_1_hour         = round(100 * mean(abs(start_gap_hours) < 1), 1),
    pct_within_1_day          = round(100 * mean(abs(start_gap_hours) < 24), 1)
  )

save_csv(start_gap_summary, "5a_pair_start_gap_summary")

p_gap <- start_gap_dist %>%
  ggplot(aes(x = gap_bin, y = pct, fill = first)) +
  geom_col(position = position_dodge(width = 0.85, preserve = "single"), width = 0.8) +
  geom_text(aes(label = paste0(pct, "%")),
            position = position_dodge(width = 0.85, preserve = "single"),
            vjust = -0.4, size = 3) +
  scale_fill_manual(values = c("Awry path first" = col_awry, "On-track path first" = col_ontrack,
                               "Same time" = "#9E9E9E")) +
  scale_y_80() +
  labs(title = "How far apart did the two paths in a pair start?",
       subtitle = "Gap between the first replies of the awry and on-track paths",
       x = "Gap between start times", y = "% of pairs",
       caption = paste0(cap, "Gaps over 1 day are grouped; see the detailed table for the full breakdown.")) +
  base_theme +
  theme(legend.position = "top")

save_fig(p_gap, "5a_pair_start_gap")

# -- 5b. Within thread: how early did each path start, relative to other sampled paths? --
# Proxy for thread start = earliest first reply among the SAMPLED paths in that thread
# (the true post time and unsampled comments are not in the data).
# Normalised rank: 0 = earliest sampled path in thread, 1 = latest.
paths_position <- paths_thread %>%
  group_by(post_id) %>%
  mutate(
    n_paths_in_thread        = n(),
    hours_after_first_path   = (start_ts - min(start_ts)) / 3600,
    start_rank               = rank(start_ts, ties.method = "min"),
    start_rank_norm          = (rank(start_ts, ties.method = "average") - 1) / (n() - 1),
    is_earliest_in_thread    = start_ts == min(start_ts)
  ) %>%
  ungroup()

position_summary <- paths_position %>%
  group_by(outcome) %>%
  summarise(
    n_paths                       = n(),
    pct_earliest_in_thread        = round(100 * mean(is_earliest_in_thread), 1),
    mean_norm_rank                = round(mean(start_rank_norm), 3),
    median_hours_after_first_path = round(median(hours_after_first_path), 2),
    pct_within_1_hr_of_first      = round(100 * mean(hours_after_first_path < 1), 1),
    pct_within_1_day_of_first     = round(100 * mean(hours_after_first_path < 24), 1),
    .groups = "drop"
  )

save_csv(position_summary, "5b_path_start_position_in_thread")

position_dist <- paths_position %>%
  mutate(
    position_bin = if_else(
      is_earliest_in_thread, "Earliest sampled path",
      as.character(cut(hours_after_first_path, breaks = HOUR_BREAKS,
                       labels = HOUR_LABELS, right = FALSE))
    ),
    position_bin = factor(position_bin, levels = c("Earliest sampled path", HOUR_LABELS))
  ) %>%
  count(outcome, position_bin, name = "n_paths") %>%
  group_by(outcome) %>%
  mutate(pct = round(100 * n_paths / sum(n_paths), 1)) %>%
  ungroup()

save_csv(position_dist, "5b_path_start_position_distribution")

position_detailed <- paths_position %>%
  mutate(
    position_fine = if_else(
      is_earliest_in_thread, "Earliest sampled path",
      as.character(cut(hours_after_first_path, breaks = FINE_BREAKS,
                       labels = FINE_LABELS, right = FALSE))
    ),
    position_fine = factor(position_fine, levels = c("Earliest sampled path", FINE_LABELS))
  ) %>%
  uncapped_by_outcome(position_fine)

save_csv(position_detailed, "5b_path_start_position_detailed")

p_position <- position_dist %>%
  ggplot(aes(x = position_bin, y = pct, fill = outcome)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  geom_text(aes(label = paste0(pct, "%")), position = position_dodge(width = 0.85),
            vjust = -0.4, size = 3) +
  scale_fill_manual(values = col_outcome) +
  scale_y_80() +
  labs(title = "How long after the thread's earliest sampled path did each path start?",
       x = "Time after the earliest sampled first reply in the thread",
       y = "% of paths (within outcome)",
       caption = paste0("Paths with post ID only. Thread start is approximated by the earliest sampled first reply. ",
                        "Gaps over 1 day are grouped.")) +
  base_theme +
  theme(legend.position = "top")

save_fig(p_position, "5b_path_start_position_in_thread")


# === SECTION 6: PARTICIPATION ===

cat("\n=== SECTION 6: PARTICIPATION ===\n")

# Note: [deleted] is counted as one speaker even though it may hide several accounts.

# -- 6a. Speakers per path --
SPK_CAP <- 6

speakers_dist <- paths %>%
  mutate(speaker_bin = pmin(n_speakers, SPK_CAP)) %>%
  count(outcome, speaker_bin, name = "n_paths") %>%
  group_by(outcome) %>%
  mutate(pct = round(100 * n_paths / sum(n_paths), 1)) %>%
  ungroup()

save_csv(speakers_dist, "6a_speakers_per_path")
save_csv(uncapped_by_outcome(paths, n_speakers), "6a_speakers_per_path_uncapped")

participation_summary <- paths %>%
  group_by(outcome) %>%
  summarise(
    n_paths                   = n(),
    mean_speakers             = round(mean(n_speakers), 2),
    median_speakers           = median(n_speakers),
    pct_two_speakers          = round(100 * mean(n_speakers == 2), 1),
    pct_three_plus_speakers   = round(100 * mean(n_speakers >= 3), 1),
    pct_with_deleted_speaker  = round(100 * mean(has_deleted_speaker), 1),
    .groups = "drop"
  )

save_csv(participation_summary, "6a_participation_summary")

p_speakers <- speakers_dist %>%
  ggplot(aes(x = speaker_bin, y = pct, fill = outcome)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  scale_fill_manual(values = col_outcome) +
  scale_x_continuous(breaks = 1:SPK_CAP, labels = c(1:(SPK_CAP - 1), paste0(SPK_CAP, "+"))) +
  scale_y_80() +
  labs(title = "Speakers per path",
       x = "Unique speakers in path", y = "% of paths (within outcome)",
       caption = paste0(cap, "[deleted] counted as one speaker.")) +
  base_theme +
  theme(legend.position = "top")

save_fig(p_speakers, "6a_speakers_per_path")

# -- 6b. The challenger: do they stay, and how much of the path do they write? --
# Excludes paths whose first reply is by [deleted] (challenger not identifiable).
challenger_paths <- paths %>% filter(challenger != "[deleted]")

challenger_summary <- challenger_paths %>%
  group_by(outcome) %>%
  summarise(
    n_paths                 = n(),
    pct_challenger_returns  = round(100 * mean(challenger_returns), 1),
    mean_challenger_share   = round(100 * mean(challenger_share), 1),
    median_challenger_share = round(100 * median(challenger_share), 1),
    .groups = "drop"
  )

save_csv(challenger_summary, "6b_challenger_summary")

# Does the challenger come back after the first reply?
challenger_returns_dist <- challenger_paths %>%
  mutate(returns = factor(if_else(challenger_returns, "Challenger returns",
                                  "Challenger does not return"),
                          levels = c("Challenger returns", "Challenger does not return"))) %>%
  count(outcome, returns, name = "n_paths") %>%
  group_by(outcome) %>%
  mutate(pct = round(100 * n_paths / sum(n_paths), 1)) %>%
  ungroup()

save_csv(challenger_returns_dist, "6b_challenger_returns")

# Awry paths run longer, and longer paths give the challenger more chances to return
# (in a 2-comment path they cannot). Compare at the same path length.
challenger_by_length <- challenger_paths %>%
  mutate(length_bin = pmin(n_comments, 10)) %>%   # 10 = 10+ comments
  group_by(outcome, length_bin) %>%
  summarise(
    n_paths                = n(),
    pct_challenger_returns = round(100 * mean(challenger_returns), 1),
    mean_challenger_share  = round(100 * mean(challenger_share), 1),
    .groups = "drop"
  ) %>%
  arrange(length_bin, outcome)

save_csv(challenger_by_length, "6b_challenger_by_path_length")

p_returns <- challenger_returns_dist %>%
  ggplot(aes(x = returns, y = pct, fill = outcome)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  geom_text(aes(label = paste0(pct, "%")), position = position_dodge(width = 0.85),
            vjust = -0.4, size = 3.2) +
  scale_fill_manual(values = col_outcome) +
  scale_y_80() +
  labs(title = "Does the challenger return after the first reply?",
       x = NULL, y = "% of paths (within outcome)",
       caption = paste0(cap, "Challenger = author of the first reply. Excludes paths with a [deleted] challenger.")) +
  base_theme +
  theme(legend.position = "top")

save_fig(p_returns, "6b_challenger_returns", width = 7, height = 5)

challenger_share_dist <- challenger_paths %>%
  mutate(share_bin = cut(challenger_share,
                         breaks = c(0, 0.2, 0.3, 0.4, 0.5, 0.6, 1),
                         labels = c("<20%", "20-30%", "30-40%", "40-50%", "50-60%", "60%+"),
                         include.lowest = TRUE, right = FALSE)) %>%
  count(outcome, share_bin, name = "n_paths") %>%
  group_by(outcome) %>%
  mutate(pct = round(100 * n_paths / sum(n_paths), 1)) %>%
  ungroup()

save_csv(challenger_share_dist, "6b_challenger_share_distribution")

p_challenger <- challenger_share_dist %>%
  ggplot(aes(x = share_bin, y = pct, fill = outcome)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  scale_fill_manual(values = col_outcome) +
  scale_y_80() +
  labs(title = "Share of the path's comments written by the challenger",
       x = "Challenger's share of comments in path", y = "% of paths (within outcome)",
       caption = paste0(cap, "Challenger = author of the first reply. Excludes paths with a [deleted] challenger.")) +
  base_theme +
  theme(legend.position = "top")

save_fig(p_challenger, "6b_challenger_share")


# === SECTION 7: FINAL COMMENT ===

cat("\n=== SECTION 7: FINAL COMMENT ===\n")

# Final comment = deepest comment in each path. In awry paths this is the removed
# comment (verified in 1_data_prep.R, section 7a); in on-track paths it is simply where
# the sampled path ends. Comparing the two shows what is distinctive about removal.
#
# Author role:
#   Challenger            - wrote the path's first reply
#   Returning participant - posted earlier in the path, but is not the challenger
#   New to path           - first appearance in the path
# Reply target: who wrote the comment the final comment replies to.

final_comments <- ce %>%
  arrange(conversation_id, thread_depth) %>%
  group_by(conversation_id) %>%
  mutate(
    challenger     = speaker[thread_depth == 0][1],
    parent_speaker = lag(speaker),
    seen_before    = duplicated(speaker)
  ) %>%
  filter(thread_depth == max(thread_depth)) %>%
  ungroup() %>%
  mutate(
    author_role = case_when(
      speaker == "[deleted]"  ~ "Unknown ([deleted])",
      speaker == challenger   ~ "Challenger",
      seen_before             ~ "Returning participant",
      TRUE                    ~ "New to path"
    ),
    reply_target = case_when(
      speaker == "[deleted]" | parent_speaker == "[deleted]" ~ "Unknown ([deleted])",
      parent_speaker == speaker                             ~ "Replies to self",
      parent_speaker == challenger                          ~ "Replies to challenger",
      TRUE                                                  ~ "Replies to another participant"
    ),
    author_role  = factor(author_role, levels = c("Challenger", "Returning participant",
                                                  "New to path", "Unknown ([deleted])")),
    reply_target = factor(reply_target, levels = c("Replies to challenger",
                                                   "Replies to another participant",
                                                   "Replies to self", "Unknown ([deleted])"))
  )

final_author_role <- final_comments %>%
  count(outcome, author_role, name = "n_paths", .drop = FALSE) %>%
  group_by(outcome) %>%
  mutate(pct = round(100 * n_paths / sum(n_paths), 1)) %>%
  ungroup()

save_csv(final_author_role, "7a_final_comment_author_role")

final_reply_target <- final_comments %>%
  count(outcome, reply_target, name = "n_paths", .drop = FALSE) %>%
  group_by(outcome) %>%
  mutate(pct = round(100 * n_paths / sum(n_paths), 1)) %>%
  ungroup()

save_csv(final_reply_target, "7b_final_comment_reply_target")


p_role <- final_author_role %>%
  ggplot(aes(x = author_role, y = pct, fill = outcome)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  geom_text(aes(label = paste0(pct, "%")), position = position_dodge(width = 0.85),
            vjust = -0.4, size = 3) +
  scale_fill_manual(values = col_outcome,
                    labels = c("Awry (removed comment)", "On-track (final comment)")) +
  scale_y_80() +
  labs(title = "Who wrote the final comment?",
       x = NULL, y = "% of paths (within outcome)", caption = cap) +
  base_theme +
  theme(legend.position = "top")

p_target <- final_reply_target %>%
  ggplot(aes(x = reply_target, y = pct, fill = outcome)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  geom_text(aes(label = paste0(pct, "%")), position = position_dodge(width = 0.85),
            vjust = -0.4, size = 3) +
  scale_fill_manual(values = col_outcome,
                    labels = c("Awry (removed comment)", "On-track (final comment)")) +
  scale_y_80() +
  labs(title = "Who does the final comment reply to?",
       x = NULL, y = "% of paths (within outcome)", caption = cap) +
  base_theme +
  theme(legend.position = "top")

p_final <- (p_role / p_target) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

save_fig(p_final, "7_final_comment_role_and_target", width = 10, height = 9)

cat("\nDone: conversation descriptives saved to", DESC, "and", FIGS, "\n")