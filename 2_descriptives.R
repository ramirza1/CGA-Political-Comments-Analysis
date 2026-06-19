# 2. descriptives.R for Conversations Gone Awry (CMV) - Political Comments Analysis
# Generates descriptive statistics and figures across 8 themes.
# Outputs: 
#   descriptive_results/<theme>/*.csv
#   graphical_output/<theme>/*.png
# Run after 1. data_prep.R

# === SECTION 0: PREP ===

# -- 0. Packages --
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggpubr, patchwork, scales)

# -- 1. Helpers --


PREPPED <- "prepped_data"
DESC    <- "descriptive_results"
FIGS    <- "graphical_output"

#Add commas to large numbers

fmt <- function(x) format(round(x, 2), big.mark = ",")

# File saving path

save_csv <- function(df, subdir, filename) {
  write_csv(df, file.path(DESC, subdir, filename))
  cat(sprintf("  -> saved: %s\n", filename))
  print(df)
  invisible(df)
}

# Figure saving path

save_fig <- function(p, subdir, filename, width = 9, height = 5) {
  ggsave(file.path(FIGS, subdir, filename), p,
         width = width, height = height, dpi = 300, bg = "white")
  cat(sprintf("  -> saved: %s\n", filename))
  invisible(p)
}

# Plot themes

base_theme <- theme_pubr() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

col_awry    <- "#EF5350"
col_ontrack <- "#81C784"
col_neutral <- "#5C85D6"


# -- 2. Load data --
message("Loading prepped data...")
ce   <- readRDS(file.path(PREPPED, "comments_enriched.rds"))
conv <- readRDS(file.path(PREPPED, "conversations.rds"))
spkr <- readRDS(file.path(PREPPED, "speakers.rds"))

# Parse delta flair ("28∆" -> 28); NA where no flair
ce <- ce %>%
  mutate(delta_count = as.numeric(str_extract(author_flair_text, "\\d+")))


# === SECTION 1: HEADLINE STATS ===

cat("\n=== HEADLINE STATS ===\n")


n_awry    <- sum(conv$has_removed_comment)
n_ontrack <- sum(!conv$has_removed_comment)
n_pairs   <- n_distinct(conv$pair_id)

headline <- tibble(
  Metric = c(
    "Total comments", "Total conversations", "Unique conversation pairs",
    "Unique speakers", "Date range start", "Date range end",
    "Awry conversations", "On-track conversations", "Awry rate",
    "Train / Val / Test", "Mean comment score", "Median comment score"
  ),
  Value = c(
    fmt(nrow(ce)), fmt(nrow(conv)), fmt(n_pairs),
    fmt(n_distinct(ce$speaker)),
    format(min(ce$timestamp_dt), "%b %Y"),
    format(max(ce$timestamp_dt), "%b %Y"),
    fmt(n_awry), fmt(n_ontrack),
    sprintf("%.1f%%", 100 * n_awry / nrow(conv)),
    paste(table(conv$split), collapse = " / "),
    fmt(mean(ce$score, na.rm = TRUE)),
    fmt(median(ce$score, na.rm = TRUE))
  )
)

save_csv(headline, "headline_stats", "headline_stats.csv")


# === SECTION 2: TEMPORAL ===

cat("\n=== TEMPORAL ===\n")

# Group comments and conversations and users by month
temporal_monthly <- ce %>%
  group_by(year, month) %>%
  summarise(
    n_comments      = n(),
    n_conversations = n_distinct(conversation_id),
    unique_users    = n_distinct(speaker),
    .groups = "drop"
  ) %>%
  mutate(year_month = as.Date(sprintf("%d-%02d-01", year, month)))

save_csv(temporal_monthly, "temporal", "temporal_monthly.csv")

users_by_year <- ce %>%
  group_by(year) %>%
  summarise(unique_users = n_distinct(speaker), .groups = "drop")

save_csv(users_by_year, "temporal", "unique_users_by_year.csv")

# Plot temporal data - monthly comments

p_comments <- temporal_monthly %>%
  ggplot(aes(x = year_month, y = n_comments)) +
  geom_col(fill = col_neutral) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  labs(title = "Comments by month", x = NULL, y = "Number of comments") +
  base_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_fig(p_comments, "temporal", "comments_by_month.png")


# Plot temporal data - monthly conversations

p_conversations <- temporal_monthly %>%
  ggplot(aes(x = year_month, y = n_conversations)) +
  geom_col(fill = col_neutral) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  labs(title = "Conversations by month", x = NULL, y = "Number of conversations") +
  base_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_fig(p_conversations, "temporal", "conversations_by_month.png")

# Combined conversations and comments plot
p_temporal_combined <- p_comments / p_conversations
save_fig(p_temporal_combined, "temporal", "temporal_comments_conversations.png", width = 9, height = 8)


# Plot annual user
p_users_year <- users_by_year %>%
  ggplot(aes(x = factor(year), y = unique_users)) +
  geom_col(fill = col_neutral) +
  labs(title = "Unique users by year", x = "Year", y = "Unique speakers") +
  base_theme

save_fig(p_users_year, "temporal", "unique_users_by_year.png")


# === SECTION 3: USER PARTICIPATION ===

cat("\n=== USER PARTICIPATION ===\n")

# Define user stats, summarizing comment, average score, median score, and delta count
# by speaker
user_stats <- ce %>%
  filter(speaker != "[deleted]") %>%
  group_by(speaker) %>%
  summarise(
    n_comments   = n(),
    mean_score   = round(mean(score, na.rm = TRUE), 2),
    median_score = median(score, na.rm = TRUE),
    max_delta    = suppressWarnings(max(delta_count, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(max_delta = ifelse(is.infinite(max_delta), NA_real_, max_delta)) %>%
  arrange(desc(n_comments))

q99 <- quantile(user_stats$n_comments, 0.99)
q95 <- quantile(user_stats$n_comments, 0.95)
q90 <- quantile(user_stats$n_comments, 0.90)

# Volume summary: one-time commenters and top-% share
n_known_comments <- sum(ce$speaker != "[deleted]")
pct_comments <- function(users_subset) round(100 * sum(users_subset) / n_known_comments, 1)
pct_users    <- function(n)            round(100 * n / nrow(user_stats), 1)


volume_summary <- tibble(
  Group = c("All users", "One-time commenters",
            "Top 1% by volume", "Top 5%", "Top 10%"),
  N_users = c(
    nrow(user_stats),
    sum(user_stats$n_comments == 1),
    sum(user_stats$n_comments >= q99),
    sum(user_stats$n_comments >= q95),
    sum(user_stats$n_comments >= q90)
  ),
  Pct_of_all_users = c(
    100,
    pct_users(sum(user_stats$n_comments == 1)),
    pct_users(sum(user_stats$n_comments >= q99)),
    pct_users(sum(user_stats$n_comments >= q95)),
    pct_users(sum(user_stats$n_comments >= q90))
  ),
  Pct_of_all_comments = c(
    100,
    pct_comments(user_stats$n_comments[user_stats$n_comments == 1]),
    pct_comments(user_stats$n_comments[user_stats$n_comments >= q99]),
    pct_comments(user_stats$n_comments[user_stats$n_comments >= q95]),
    pct_comments(user_stats$n_comments[user_stats$n_comments >= q90])
  )
)

save_csv(volume_summary, "user_participation", "user_volume_summary.csv")

# Delta flair summary - summarize number and percentage of users with deltas
delta_summary <- user_stats %>%
  summarise(
    users_with_delta  = sum(!is.na(max_delta)),
    pct_with_delta    = round(100 * users_with_delta / n(), 1),
    mean_delta        = round(mean(max_delta, na.rm = TRUE), 1),
    median_delta      = median(max_delta, na.rm = TRUE),
    max_delta_overall = max(max_delta, na.rm = TRUE)
  )

save_csv(delta_summary, "user_participation", "delta_flair_summary.csv")

# Comments per user histogram (trim top 1% for readability)
p_user_dist <- user_stats %>%
  filter(n_comments <= q99) %>%
  ggplot(aes(x = n_comments)) +
  geom_histogram(bins = 40, fill = col_neutral, color = "white") +
  labs(title = "Comments per user (excluding top 1%)",
       x = "Number of comments", y = "Number of users") +
  base_theme

save_fig(p_user_dist, "user_participation", "comments_per_user.png")

# Comments per user histogram (top 1%)
top1pct_users <- user_stats %>%
  filter(n_comments >= q99) %>%
  arrange(desc(n_comments)) %>%
  mutate(rank = factor(row_number(), levels = rev(seq_len(sum(n_comments >= q99)))))

mean_top1 <- round(mean(top1pct_users$n_comments), 0)

top_user <- top1pct_users %>% mutate(rank = row_number()) %>% filter(rank == 1)

n_top1 <- nrow(top1pct_users)

p_top1pct <- top1pct_users %>%
  mutate(rank = row_number()) %>%
  ggplot(aes(x = rank, y = n_comments)) +
  geom_col(fill = col_neutral, width = 0.8) +
  geom_text(data = top_user,
            aes(label = paste0("#1: ", scales::comma(n_comments), " comments")),
            vjust = -0.4, hjust = 0, size = 3.5, fontface = "bold") +
  geom_hline(yintercept = mean_top1, linetype = "dashed",
             color = "darkred", linewidth = 0.8) +
  annotate("text",
           x = n_top1 * 0.94, y = mean_top1 * 2,
           label = paste0("Mean: ", fmt(mean_top1)),
           color = "darkred", size = 3.5, hjust = 1) +
  scale_y_log10(labels = scales::comma,
                breaks = c(1, 10, 100, 1000),
                limits = c(NA, 1000)) +
  scale_x_continuous(breaks = c(1, seq(50, 200, by = 50), n_top1),
                     limits = c(0.5, n_top1 + 0.5)) +
  labs(title = paste0("Top 1% users by comment volume (n = ", n_top1, ", log scale)"),
       x = "Rank", y = "Number of comments (log scale)") +
  base_theme

save_fig(p_top1pct, "user_participation", "top1pct_users.png", width = 10, height = 6)

# Top 50 users by comment volume — [deleted] included deliberately here to
# show the artefact. It is NOT a single user, so it
# is labelled explicitly rather than anonymised by rank like everyone else.
user_stats_with_deleted <- ce %>%
  group_by(speaker) %>%
  summarise(n_comments = n(), .groups = "drop")

top50_with_deleted <- user_stats_with_deleted %>%
  slice_max(n_comments, n = 50, with_ties = FALSE) %>%
  mutate(
    rank           = row_number(),
    is_deleted     = speaker == "[deleted]",
    display_label  = ifelse(is_deleted, "[deleted]", paste0("#", rank))
  )

label_levels <- top50_with_deleted %>% arrange(rank) %>% pull(display_label) %>% rev()
top50_with_deleted <- top50_with_deleted %>%
  mutate(display_label = factor(display_label, levels = label_levels))

p_top_users <- top50_with_deleted %>%
  ggplot(aes(x = display_label, y = n_comments, fill = is_deleted)) +
  geom_col() +
  geom_text(aes(label = scales::comma(n_comments)),
            hjust = -0.15, size = 2.5) +
  scale_fill_manual(values = c("FALSE" = col_neutral, "TRUE" = col_awry), guide = "none") +
  scale_y_log10(labels = scales::comma,
                limits = c(NA, max(top50_with_deleted$n_comments) * 4)) +
  coord_flip() +
  labs(title    = "Top 50 users by comment volume (log scale)",
       subtitle = "[deleted] = Reddit's placeholder for removed/banned accounts, not one user.\nOther users anonymised by rank.",
       x = NULL, y = "Number of comments (log scale)") +
  base_theme

save_fig(p_top_users, "user_participation", "top50_users.png", width = 7, height = 8)


# Score by user activity quartile
user_quartile <- user_stats %>%
  mutate(activity_q = paste0("Q", ntile(n_comments, 4)))

score_by_activity <- ce %>%
  left_join(user_quartile %>% select(speaker, activity_q), by = "speaker") %>%
  filter(!is.na(activity_q)) %>%
  group_by(activity_q) %>%
  summarise(
    n_comments   = n(),
    mean_score   = round(mean(score, na.rm = TRUE), 2),
    median_score = median(score, na.rm = TRUE),
    .groups = "drop"
  )

save_csv(score_by_activity, "user_participation", "score_by_activity_quartile.csv")

p_score_activity <- score_by_activity %>%
  ggplot(aes(x = activity_q, y = mean_score, fill = activity_q)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f", mean_score)),
            vjust = -0.4, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Q1" = "#B3CDE3", "Q2" = "#6BAED6",
                               "Q3" = "#2171B5", "Q4" = "#084594")) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(title = "Mean score by user activity quartile",
       subtitle = "Quartiles based on number of comments per user. Q1 = fewest comments, Q4 = most",
       x = "Activity quartile", y = "Mean score") +
  base_theme +
  theme(legend.position = "none")

save_fig(p_score_activity, "user_participation", "score_by_activity_quartile.png")

# same plot with average number of comments per quartile:

score_by_activity_enhanced <- score_by_activity %>%
  left_join(
    user_stats %>%
      mutate(activity_q = paste0("Q", ntile(n_comments, 4))) %>%
      group_by(activity_q) %>%
      summarise(avg_comments = round(mean(n_comments), 1), .groups = "drop"),
    by = "activity_q"
  )

p_score_activity <- score_by_activity_enhanced %>%
  ggplot(aes(x = activity_q, y = mean_score, fill = activity_q)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f", mean_score)),
            vjust = -0.4, size = 4, fontface = "bold") +
  geom_text(aes(y = -6, label = sprintf("%.1f", avg_comments)), size = 3.5) +
  annotate("text", x = 0.35, y = -6,
           label = "Avg comments\nper user:", hjust = 1, size = 3) +
  scale_fill_manual(values = c("Q1" = "#B3CDE3", "Q2" = "#6BAED6",
                               "Q3" = "#2171B5", "Q4" = "#084594")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  coord_cartesian(ylim = c(0, 50), clip = "off") +
  labs(title = "Mean score by user activity quartile",
       subtitle = "Quartiles based on number of comments per user. Q1 = fewest, Q4 = most",
       x = NULL, y = "Mean score") +
  base_theme +
  theme(legend.position = "none",
        plot.margin = margin(10, 10, 50, 80))

save_fig(p_score_activity, "user_participation", "score_by_activity_quartile_with_comment_number.png",
         width = 8, height = 6)

# === SECTION 4: THREAD STRUCTURE ===

cat("\n=== THREAD STRUCTURE ===\n")

# Compare threads that are awry vs not awry on number of comments, top level vs nested
thread_stats <- ce %>%
  group_by(conversation_id, has_removed_comment) %>%
  summarise(
    n_comments  = n(),
    n_top_level = sum(comment_type == "top_level_reply"),
    n_nested    = sum(comment_type == "nested_reply"),
    .groups = "drop"
  ) %>%
  mutate(
    pct_top_level = round(100 * n_top_level / n_comments, 1),
    pct_nested    = round(100 * n_nested    / n_comments, 1)
  )

# Comment type breakdown by user group
one_time_speakers <- user_stats %>% filter(n_comments == 1) %>% pull(speaker)

ct_by_group <- function(data, group_label) {
  data %>%
    count(comment_type) %>%
    mutate(
      pct   = round(100 * n / sum(n), 1),
      group = group_label
    ) %>%
    select(group, comment_type, n, pct)
}

comment_type_by_user_group <- bind_rows(
  ct_by_group(ce, "All users"),
  ct_by_group(ce %>% filter(speaker %in% top1pct_users$speaker), "Top 1%"),
  ct_by_group(ce %>% filter(speaker %in% one_time_speakers),     "One-time commenters")
)

save_csv(comment_type_by_user_group, "thread_structure", "comment_type_by_user_group.csv")


# Compare awry vs non-awry for number of conversations, average number of comments per conversation, nested vs top-level


thread_summary <- thread_stats %>%
  mutate(outcome = ifelse(has_removed_comment, "Awry", "On-track")) %>%
  group_by(outcome) %>%
  summarise(
    n_conversations    = n(),
    mean_comments      = round(mean(n_comments), 1),
    median_comments    = median(n_comments),
    max_comments       = max(n_comments),
    mean_pct_top_level = round(mean(pct_top_level), 1),
    mean_pct_nested    = round(mean(pct_nested), 1),
    .groups = "drop"
  )

save_csv(thread_summary, "thread_structure", "thread_summary_by_outcome.csv")


# Count root vs nested comments by awry vs non-awry
comment_type_dist <- ce %>%
  count(comment_type, has_removed_comment) %>%
  group_by(has_removed_comment) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  mutate(outcome = ifelse(has_removed_comment, "Awry", "On-track"))

save_csv(comment_type_dist, "thread_structure", "comment_type_distribution.csv")

# plot root vs nested comments by awry vs non-awry

p_comment_type <- comment_type_dist %>%
  ggplot(aes(x = comment_type, y = pct, fill = outcome)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Awry" = col_awry, "On-track" = col_ontrack)) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Comment type by conversation outcome",
       x = "Comment type", y = "% of comments", fill = "Outcome") +
  base_theme

save_fig(p_comment_type, "thread_structure", "comment_type_by_outcome.png")


# === SECTION 5: SCORES ===

cat("\n=== SCORES ===\n")

# summary of scores across all comments
score_summary <- ce %>%
  summarise(
    mean_score        = round(mean(score, na.rm = TRUE), 2),
    median_score      = median(score, na.rm = TRUE),
    min_score         = min(score, na.rm = TRUE),
    max_score         = max(score, na.rm = TRUE),
    pct_zero_or_below = round(100 * mean(score <= 0, na.rm = TRUE), 1)
  )

save_csv(score_summary, "scores", "score_summary.csv")

# score distribution - quintiles

score_quintiles <- ce %>%
  filter(!is.na(score)) %>%
  mutate(quintile = ntile(score, 5)) %>%
  group_by(quintile) %>%
  summarise(
    n          = n(),
    min_score  = min(score),
    max_score  = max(score),
    mean_score = round(mean(score), 1),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0("Q", quintile, "\n(", min_score, " to ", max_score, ")")
  )

save_csv(score_quintiles %>% select(-label), "scores", "score_quintiles.csv")

p_score_quintiles <- score_quintiles %>%
  ggplot(aes(x = label, y = mean_score)) +
  geom_col(fill = col_neutral) +
  geom_text(aes(label = sprintf("%.1f", mean_score)),
            vjust = -0.4, size = 3.5, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Mean score by quintile",
       subtitle = "Each quintile contains ~20% of comments. Range shows min to max score within quintile.",
       x = NULL, y = "Mean score") +
  base_theme

save_fig(p_score_quintiles, "scores", "score_distribution.png")

# score distribution - discrete buckets

score_buckets <- ce %>%
  filter(!is.na(score)) %>%
  mutate(bucket = case_when(
    score <= 0  ~ "≤ 0",
    score == 1  ~ "1",
    score <= 10 ~ "2–10",
    TRUE        ~ "11+"
  )) %>%
  mutate(bucket = factor(bucket, levels = c("≤ 0", "1", "2–10", "11+"))) %>%
  count(bucket) %>%
  mutate(pct = round(100 * n / sum(n), 1))

save_csv(score_buckets, "scores", "score_buckets.csv")

p_score_buckets <- score_buckets %>%
  ggplot(aes(x = bucket, y = n)) +
  geom_col(fill = col_neutral) +
  geom_text(aes(label = paste0(fmt(n), "\n(", pct, "%)")),
            vjust = -0.4, size = 3.5, fontface = "bold") +
  scale_y_continuous(limits = c(0, 50000),
                     breaks = seq(0, 50000, by = 10000),
                     labels = scales::comma) +
  labs(title = "Comment count by score bucket",
       x = "Score range", y = "Number of comments") +
  base_theme

save_fig(p_score_buckets, "scores", "score_buckets.png")

# scores over time

score_by_year <- ce %>%
  group_by(year) %>%
  summarise(
    mean_score   = round(mean(score, na.rm = TRUE), 2),
    median_score = median(score, na.rm = TRUE),
    .groups = "drop"
  )

save_csv(score_by_year, "scores", "score_by_year.csv")

# plot of scores over time
p_score_year <- score_by_year %>%
  ggplot(aes(x = factor(year), y = mean_score)) +
  geom_col(fill = col_neutral) +
  labs(title = "Mean score by year", x = "Year", y = "Mean score") +
  base_theme

save_fig(p_score_year, "scores", "score_by_year.png")

# Scores for awry + non-awry

score_by_outcome <- ce %>%
  mutate(outcome = ifelse(has_removed_comment, "Awry", "On-track")) %>%
  group_by(outcome) %>%
  summarise(
    n_comments   = n(),
    mean_score   = round(mean(score, na.rm = TRUE), 2),
    median_score = median(score, na.rm = TRUE),
    .groups = "drop"
  )

save_csv(score_by_outcome, "scores", "score_by_outcome.csv")

# Plot awry vs non-awry scores

p_score_outcome <- score_by_outcome %>%
  ggplot(aes(x = outcome, y = mean_score, fill = outcome)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = sprintf("%.1f", mean_score)), vjust = -0.4, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Awry" = col_awry, "On-track" = col_ontrack)) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(title = "Mean score by conversation outcome",
       x = NULL, y = "Mean score") +
  base_theme +
  theme(legend.position = "none")

save_fig(p_score_outcome, "scores", "score_by_outcome.png", width = 6, height = 5)

# and median for comparison

p_score_outcome_median <- score_by_outcome %>%
  ggplot(aes(x = outcome, y = median_score, fill = outcome)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = sprintf("%.0f", median_score)),
            vjust = -0.4, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Awry" = col_awry, "On-track" = col_ontrack)) +
  scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
  labs(title = "Median score by conversation outcome",
       x = NULL, y = "Median score") +
  base_theme +
  theme(legend.position = "none")

save_fig(p_score_outcome_median, "scores", "score_by_outcome_median.png", width = 6, height = 5)

# Combined mean + median side by side, useful as a single paper exhibit
p_score_outcome_combined <- p_score_outcome | p_score_outcome_median
save_fig(p_score_outcome_combined, "scores", "score_by_outcome_mean_median.png", width = 11, height = 5)


# === SECTION 6: TEXT LENGTH ===

cat("\n=== TEXT LENGTH ===\n")


# summary of text length for awry vs on-track
text_summary <- ce %>%
  mutate(outcome = ifelse(has_removed_comment, "Awry", "On-track")) %>%
  group_by(comment_type, outcome) %>%
  summarise(
    mean_words   = round(mean(word_count, na.rm = TRUE), 1),
    median_words = median(word_count, na.rm = TRUE),
    mean_chars   = round(mean(char_count, na.rm = TRUE), 0),
    median_chars = median(char_count, na.rm = TRUE),
    .groups = "drop"
  )

save_csv(text_summary, "text_length", "text_length_by_type_and_outcome.csv")

# plot word counts for awry vs non-track, removing outliers

bin_width <- 10

word_count_bins <- ce %>%
  filter(!is.na(word_count),
         word_count <= quantile(word_count, 0.99, na.rm = TRUE)) %>%
  mutate(
    outcome = ifelse(has_removed_comment, "Awry", "On-track"),
    bin = floor(word_count / bin_width) * bin_width
  ) %>%
  count(bin, outcome) %>%
  pivot_wider(names_from = outcome, values_from = n, values_fill = 0)

p_wordcount <- word_count_bins %>%
  ggplot(aes(x = bin)) +
  geom_col(aes(y = `On-track`, fill = "On-track"), width = bin_width * 0.9) +
  geom_point(aes(y = Awry, colour = "Awry"), size = 1.5) +
  scale_y_continuous(limits = c(0, 8000),
                     breaks = seq(0, 8000, by = 2000),
                     labels = scales::comma) +
  scale_fill_manual(values   = c("On-track" = "#1565C0"), name = "Outcome") +
  scale_colour_manual(values = c("Awry"     = "#E53935"), name = "Outcome") +
  labs(title    = "Word count distribution by outcome (trimmed at 99th percentile)",
       x = "Word count", y = "Count") +
  base_theme +
  theme(legend.position = "top")

save_fig(p_wordcount, "text_length", "word_count_by_outcome.png")

# Compare top 1% (n=246) users vs the rest on verbosity

top1pct_speakers <- user_stats %>%
  filter(n_comments >= q99) %>%
  pull(speaker)

wordcount_by_user_type <- ce %>%
  filter(speaker != "[deleted]") %>%
  mutate(user_type = ifelse(speaker %in% top1pct_speakers, "Top 1%", "Bottom 99%")) %>%
  group_by(user_type) %>%
  summarise(
    n_comments   = n(),
    mean_words   = round(mean(word_count, na.rm = TRUE), 1),
    median_words = median(word_count, na.rm = TRUE),
    .groups = "drop"
  )

save_csv(wordcount_by_user_type, "text_length", "wordcount_by_user_type.csv")

# Plot top 1% vs rest

p_wordcount_users <- wordcount_by_user_type %>%
  ggplot(aes(x = user_type, y = mean_words, fill = user_type)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = sprintf("%.1f", mean_words)), vjust = -0.4, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Top 1%" = "#084594", "Bottom 99%" = "#90CAF9")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Mean word count by user type",
       subtitle = "User type based on comment volume (top 1% = 246 users)",
       x = NULL, y = "Mean word count") +
  base_theme +
  theme(legend.position = "none")

save_fig(p_wordcount_users, "text_length", "wordcount_by_user_type.png", width = 6, height = 5)

# === SECTION 7: MISSINGNESS ===

cat("\n=== MISSINGNESS ===\n")


# See number of missing values across all variables
missingness <- ce %>%
  select(comment_id, conversation_id, speaker, text, score) %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_missing") %>%
  mutate(pct_missing = round(100 * n_missing / nrow(ce), 2))

n_dupes <- sum(duplicated(ce$comment_id))
cat(sprintf("  Duplicate comment_ids: %d\n", n_dupes))

save_csv(missingness, "missingness", "missingness_summary.csv")

# === SECTION 8: REMOVED CONTENT ===

cat("\n=== REMOVED CONTENT ===\n")

# Get conversation start year from earliest comment
conv_year <- ce %>%
  group_by(conversation_id) %>%
  summarise(year = min(year), .groups = "drop")

# See how many comments were removed by year
removed_by_year <- conv %>%
  left_join(conv_year, by = "conversation_id") %>%
  group_by(year) %>%
  summarise(
    n_conversations = n(),
    n_awry          = sum(has_removed_comment),
    pct_awry        = round(100 * n_awry / n_conversations, 1),
    .groups = "drop"
  )

save_csv(removed_by_year, "removed_content", "removed_by_year.csv")

# plot removed content by year

p_removed_year <- removed_by_year %>%
  ggplot(aes(x = factor(year), y = n_awry)) +
  geom_col(fill = col_awry) +
  labs(title = "Awry conversations by year",
       x = "Year", y = "Number of removed conversations") +
  base_theme

save_fig(p_removed_year, "removed_content", "awry_conversations_by_year.png")