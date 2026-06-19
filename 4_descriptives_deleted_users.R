# 4. descriptives_deleted_users.R for Conversations Gone Awry (CMV) - Deleted Accounts Analysis
#
# [deleted] is Reddit's placeholder for any account that has been deleted,
# suspended, or banned. Every such comment shares this single speaker value,
# so it is NOT one prolific user — it is an unknown number of distinct
# accounts collapsed into one label. For this reason [deleted] is excluded
# from all user-level analyses in 2. descriptives.R and 3. descriptives_power_users.R
# and is analysed separately here.
#
# Whether this block reflects voluntary account deletion, platform bans, or
# both cannot be determined from this data alone. The awry-rate and score
# comparisons below are intended to inform — not settle — that question.
#
# Run after 1. data_prep.R

# === SECTION 0: PREP ===

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggpubr, patchwork, scales)

PREPPED <- "prepped_data"
DESC    <- "descriptive_results"
FIGS    <- "graphical_output"

fmt <- function(x) format(round(x, 2), big.mark = ",")

save_csv <- function(df, subdir, filename) {
  write_csv(df, file.path(DESC, subdir, filename))
  cat(sprintf("  -> saved: %s\n", filename))
  print(df)
  invisible(df)
}

save_fig <- function(p, subdir, filename, width = 9, height = 5) {
  ggsave(file.path(FIGS, subdir, filename), p,
         width = width, height = height, dpi = 300, bg = "white")
  cat(sprintf("  -> saved: %s\n", filename))
  invisible(p)
}

base_theme <- theme_pubr() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

col_awry    <- "#EF5350"
col_ontrack <- "#81C784"
col_neutral <- "#5C85D6"

# -- 1. Load data and isolate [deleted] --
message("Loading prepped data...")
ce   <- readRDS(file.path(PREPPED, "comments_enriched.rds"))
conv <- readRDS(file.path(PREPPED, "conversations.rds"))

ce_del <- ce %>% filter(speaker == "[deleted]")

cat(sprintf(
  "\n[deleted] comments: %s of %s total (%.1f%% of corpus)\n",
  fmt(nrow(ce_del)), fmt(nrow(ce)), 100 * nrow(ce_del) / nrow(ce)
))

# Conversations [deleted] participated in (at least one comment)
conv_del <- conv %>% filter(conversation_id %in% unique(ce_del$conversation_id))

cat(sprintf("Conversations [deleted] appears in: %s of %s total\n",
            fmt(nrow(conv_del)), fmt(nrow(conv))))


# === SECTION 1: HEADLINE STATS ===

cat("\n=== HEADLINE STATS ([deleted]) ===\n")

n_awry_del    <- sum(conv_del$has_removed_comment)
n_ontrack_del <- sum(!conv_del$has_removed_comment)

headline_del <- tibble(
  Metric = c(
    "Comments by [deleted]",
    "% of full corpus",
    "Conversations [deleted] appears in",
    "Date range start",
    "Date range end",
    "Awry conversations (participated in)",
    "On-track conversations (participated in)",
    "Awry rate (of conversations participated in)",
    "Mean comment score",
    "Median comment score"
  ),
  Value = c(
    fmt(nrow(ce_del)),
    sprintf("%.1f%%", 100 * nrow(ce_del) / nrow(ce)),
    fmt(nrow(conv_del)),
    format(min(ce_del$timestamp_dt), "%b %Y"),
    format(max(ce_del$timestamp_dt), "%b %Y"),
    fmt(n_awry_del),
    fmt(n_ontrack_del),
    sprintf("%.1f%%", 100 * n_awry_del / nrow(conv_del)),
    fmt(mean(ce_del$score, na.rm = TRUE)),
    fmt(median(ce_del$score, na.rm = TRUE))
  )
)

save_csv(headline_del, "headline_stats", "deleted_headline_stats.csv")

cat(sprintf(
  "\n  Awry rate for [deleted]-participated conversations: %.1f%% (corpus baseline: 50.0%%)\n",
  100 * n_awry_del / nrow(conv_del)
))


# === SECTION 2: TEMPORAL ===

cat("\n=== TEMPORAL ([deleted]) ===\n")

temporal_del_monthly <- ce_del %>%
  group_by(year, month) %>%
  summarise(
    n_comments      = n(),
    n_conversations = n_distinct(conversation_id),
    .groups = "drop"
  ) %>%
  mutate(year_month = as.Date(sprintf("%d-%02d-01", year, month)))

save_csv(temporal_del_monthly, "temporal", "deleted_temporal_monthly.csv")

p_del_comments <- temporal_del_monthly %>%
  ggplot(aes(x = year_month, y = n_comments)) +
  geom_col(fill = col_neutral) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  labs(title = "[deleted]: comments by month", x = NULL, y = "Number of comments") +
  base_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_del_conversations <- temporal_del_monthly %>%
  ggplot(aes(x = year_month, y = n_conversations)) +
  geom_col(fill = col_neutral) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  labs(title = "[deleted]: conversations participated in by month",
       x = NULL, y = "Number of conversations") +
  base_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_del_temporal_combined <- p_del_comments / p_del_conversations
save_fig(p_del_temporal_combined, "temporal", "deleted_temporal_combined.png",
         width = 9, height = 8)


# === SECTION 3: SCORES ===

cat("\n=== SCORES ([deleted]) ===\n")

score_summary_del <- ce_del %>%
  summarise(
    mean_score        = round(mean(score, na.rm = TRUE), 2),
    median_score      = median(score, na.rm = TRUE),
    min_score         = min(score, na.rm = TRUE),
    max_score         = max(score, na.rm = TRUE),
    pct_zero_or_below = round(100 * mean(score <= 0, na.rm = TRUE), 1)
  )

save_csv(score_summary_del, "scores", "deleted_score_summary.csv")

# Mean score by conversation outcome — the key comparison.
# If [deleted] comments disproportionately cluster in awry conversations
# and/or carry markedly different scores than the corpus baseline
# (full sample: Awry 22.3 / On-track 15.5), that's consistent with a
# moderation/ban-driven explanation rather than voluntary departure.

score_by_outcome_del <- ce_del %>%
  mutate(outcome = ifelse(has_removed_comment, "Awry", "On-track")) %>%
  group_by(outcome) %>%
  summarise(
    n_comments   = n(),
    mean_score   = round(mean(score, na.rm = TRUE), 2),
    median_score = median(score, na.rm = TRUE),
    .groups = "drop"
  )

save_csv(score_by_outcome_del, "scores", "deleted_score_by_outcome.csv")

p_del_score_outcome <- score_by_outcome_del %>%
  ggplot(aes(x = outcome, y = mean_score, fill = outcome)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = sprintf("%.1f", mean_score)),
            vjust = -0.4, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Awry" = col_awry, "On-track" = col_ontrack)) +
  scale_y_continuous(limits = c(0, 50)) +  # matched to full-sample/power-user charts
  labs(title    = "[deleted]: mean score by conversation outcome",
       subtitle = "Compare to full sample: Awry 22.3 / On-track 15.5",
       x = NULL, y = "Mean score") +
  base_theme +
  theme(legend.position = "none")

save_fig(p_del_score_outcome, "scores", "deleted_score_by_outcome.png", width = 6, height = 5)

# and Median score

p_del_score_outcome_median <- score_by_outcome_del %>%
  ggplot(aes(x = outcome, y = median_score, fill = outcome)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = sprintf("%.0f", median_score)),
            vjust = -0.4, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Awry" = col_awry, "On-track" = col_ontrack)) +
  scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
  labs(title    = "[deleted]: median score by conversation outcome",
       subtitle = "Compare to full sample: Awry 1 / On-track 2",
       x = NULL, y = "Median score") +
  base_theme +
  theme(legend.position = "none")

save_fig(p_del_score_outcome_median, "scores", "deleted_score_by_outcome_median.png", width = 6, height = 5)

p_del_score_outcome_combined <- p_del_score_outcome | p_del_score_outcome_median
save_fig(p_del_score_outcome_combined, "scores", "deleted_score_by_outcome_mean_median.png", width = 11, height = 5)