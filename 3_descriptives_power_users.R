# 3. descriptives_power_users.R for Conversations Gone Awry (CMV) - Power Users Analysis
# Generates descriptive statistics and figures for the top 1% ("power users").
#
# Filtering: comment-level — only rows in ce where speaker is a power user.
# Conversation scope: conversations those users participated in (>= 1 comment).
#
# Run after 1. data_prep.R and 2. descriptives.R


# === SECTION 0: PREP ===

# -- 0. Packages --
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggpubr, patchwork, scales)

# -- 1. Helpers --

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
    plot.title    = element_text(face = "bold"),
    axis.title    = element_text(face = "bold")
  )

col_awry    <- "#EF5350"
col_ontrack <- "#81C784"
col_neutral <- "#5C85D6"

# -- 2. Load data --
message("Loading prepped data...")
ce   <- readRDS(file.path(PREPPED, "comments_enriched.rds"))
conv <- readRDS(file.path(PREPPED, "conversations.rds"))

# Exclude [deleted] — see 4. descriptives_deleted_users.R for that analysis
ce <- ce %>% filter(speaker != "[deleted]")

# Parse delta flair ("28∆" -> 28); NA where no flair
ce <- ce %>%
  mutate(delta_count = as.numeric(str_extract(author_flair_text, "\\d+")))


# -- 3. Define power users --
# Full user_stats is needed to compute the q99 threshold consistently

user_stats <- ce %>%
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

power_users <- user_stats %>% filter(n_comments >= q99)
top1pct_speakers <- power_users$speaker
n_power <- nrow(power_users)

cat(sprintf(
  "\nPower users: n = %d (top 1%% threshold: >= %.0f comments per user)\n",
  nrow(power_users), q99
))

# -- 4. Filter comments and conversations --

# Comment-level filter: only power user rows
ce_pu <- ce %>% filter(speaker %in% top1pct_speakers)

# Conversations these users participated in (at least one comment)
conv_pu <- conv %>% filter(conversation_id %in% unique(ce_pu$conversation_id))

cat(sprintf(
  "Power user comments : %s of %s total (%.1f%%)\n",
  fmt(nrow(ce_pu)), fmt(nrow(ce)),
  100 * nrow(ce_pu) / nrow(ce)
))
cat(sprintf(
  "Conversations participated in: %s of %s total\n",
  fmt(nrow(conv_pu)), fmt(nrow(conv))
))


# === SECTION 1: HEADLINE STATS ===

cat("\n=== HEADLINE STATS (POWER USERS) ===\n")

n_awry_pu    <- sum(conv_pu$has_removed_comment)
n_ontrack_pu <- sum(!conv_pu$has_removed_comment)

headline_pu <- tibble(
  Metric = c(
    "Power users (top 1%)",
    "Total comments by power users",
    "Conversations participated in",
    "Date range start",
    "Date range end",
    "Awry conversations (participated in)",
    "On-track conversations (participated in)",
    "Awry rate (of conversations participated in)",
    "Mean comment score",
    "Median comment score",
    "Mean comments per power user",
    "Median comments per power user"
  ),
  Value = c(
    fmt(nrow(power_users)),
    fmt(nrow(ce_pu)),
    fmt(nrow(conv_pu)),
    format(min(ce_pu$timestamp_dt), "%b %Y"),
    format(max(ce_pu$timestamp_dt), "%b %Y"),
    fmt(n_awry_pu),
    fmt(n_ontrack_pu),
    sprintf("%.1f%%", 100 * n_awry_pu / nrow(conv_pu)),
    fmt(mean(ce_pu$score, na.rm = TRUE)),
    fmt(median(ce_pu$score, na.rm = TRUE)),
    fmt(mean(power_users$n_comments)),
    fmt(median(power_users$n_comments))
  )
)

save_csv(headline_pu, "headline_stats", "top1pct_headline_stats.csv")


# === SECTION 2: TEMPORAL ===

cat("\n=== TEMPORAL (POWER USERS) ===\n")

# Monthly comments and distinct conversations by power users
temporal_top1pct_monthly <- ce_pu %>%
  group_by(year, month) %>%
  summarise(
    n_comments      = n(),
    n_conversations = n_distinct(conversation_id),
    .groups = "drop"
  ) %>%
  mutate(year_month = as.Date(sprintf("%d-%02d-01", year, month)))

save_csv(temporal_top1pct_monthly, "temporal", "top1pct_temporal_monthly.csv")

# How many of the 246 power users were active in each year
top1pct_users_by_year <- ce_pu %>%
  group_by(year) %>%
  summarise(unique_users = n_distinct(speaker), .groups = "drop")

save_csv(top1pct_users_by_year, "temporal", "top1pct_unique_users_by_year.csv")

# -- Plots --

p_top1pct_comments <- temporal_top1pct_monthly %>%
  ggplot(aes(x = year_month, y = n_comments)) +
  geom_col(fill = col_neutral) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  labs(title    = "Power users: comments by month",
       subtitle = paste0("Top 1% users (n = ", n_power, ") only"),
       x = NULL, y = "Number of comments") +
  base_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_fig(p_top1pct_comments, "temporal", "top1pct_comments_by_month.png")

p_top1pct_conversations <- temporal_top1pct_monthly %>%
  ggplot(aes(x = year_month, y = n_conversations)) +
  geom_col(fill = col_neutral) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  labs(title    = "Power users: conversations participated in by month",
       subtitle = paste0("Top 1% users (n = ", n_power, ") only"),
       x = NULL, y = "Number of conversations") +
  base_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_fig(p_top1pct_conversations, "temporal", "top1pct_conversations_by_month.png")

p_top1pct_temporal_combined <- p_top1pct_comments / p_top1pct_conversations
save_fig(p_top1pct_temporal_combined, "temporal", "top1pct_temporal_combined.png",
         width = 9, height = 8)

p_top1pct_users_year <- top1pct_users_by_year %>%
  ggplot(aes(x = factor(year), y = unique_users)) +
  geom_col(fill = col_neutral) +
  labs(title    = "Power users active by year",
       subtitle = "A user appears once per year they commented. Not additive across years.",
       x = "Year", y = "Active power users") +
  base_theme

save_fig(p_top1pct_users_year, "temporal", "top1pct_users_by_year.png")


# === SECTION 3: USER PARTICIPATION ===

cat("\n=== USER PARTICIPATION (POWER USERS) ===\n")

# -- 3a. Delta flair summary --

delta_summary_pu <- power_users %>%
  summarise(
    users_with_delta  = sum(!is.na(max_delta)),
    pct_with_delta    = round(100 * users_with_delta / n(), 1),
    mean_delta        = round(mean(max_delta, na.rm = TRUE), 1),
    median_delta      = median(max_delta, na.rm = TRUE),
    max_delta_overall = max(max_delta, na.rm = TRUE)
  )

save_csv(delta_summary_pu, "user_participation", "top1pct_delta_flair_summary.csv")

# -- 3b. Awry participation rate per power user --
# For each of the 246 users: what % of their OWN comments land in awry conversations?
# This reveals whether the group is homogeneous or whether there are structurally
# "civil" vs "derailment-prone" power users.

top1pct_awry_rate <- ce_pu %>%
  group_by(speaker) %>%
  summarise(
    n_comments      = n(),
    n_awry_comments = sum(has_removed_comment),
    pct_awry        = round(100 * n_awry_comments / n_comments, 1),
    .groups = "drop"
  )

top1pct_awry_rate_summary <- top1pct_awry_rate %>%
  summarise(
    mean_pct_awry   = round(mean(pct_awry), 1),
    median_pct_awry = round(median(pct_awry), 1),
    sd_pct_awry     = round(sd(pct_awry), 1),
    min_pct_awry    = min(pct_awry),
    max_pct_awry    = max(pct_awry),
    pct_users_above_50 = round(100 * mean(pct_awry > 50), 1),
    pct_users_below_50 = round(100 * mean(pct_awry < 50), 1)
  )

save_csv(top1pct_awry_rate,         "user_participation", "top1pct_awry_participation_rate.csv")
save_csv(top1pct_awry_rate_summary, "user_participation", "top1pct_awry_participation_summary.csv")

mean_awry_pu <- top1pct_awry_rate_summary$mean_pct_awry

p_top1pct_awry_hist <- top1pct_awry_rate %>%
  ggplot(aes(x = pct_awry)) +
  geom_histogram(bins = 20, fill = col_neutral, colour = "white") +
  geom_vline(xintercept = 50, linetype = "dashed",
             colour = "darkred", linewidth = 0.8) +
  geom_vline(xintercept = mean_awry_pu, linetype = "dotted",
             colour = "gray30", linewidth = 0.8) +
  annotate("text", x = 51.5, y = Inf,
           label = "Corpus avg (50%)", hjust = 0, vjust = 2,
           colour = "darkred", size = 3.2) +
  annotate("text", x = mean_awry_pu + 1.5, y = Inf,
           label = sprintf("Group mean (%.0f%%)", mean_awry_pu),
           hjust = 0, vjust = 3.8,
           colour = "gray30", size = 3.2) +
  labs(
    title    = "Awry participation rate: distribution across power users",
    subtitle = paste0("% of each power user's own comments that fall in awry conversations (n = ", n_power, " users)"),
    x        = "% of own comments in awry conversations",
    y        = "Number of power users"
  ) +
  base_theme

save_fig(p_top1pct_awry_hist, "user_participation", "top1pct_awry_participation_rate.png")


# === SECTION 4: SCORES ===

cat("\n=== SCORES (POWER USERS) ===\n")

# -- 4a. Score summary table --

score_summary_pu <- ce_pu %>%
  summarise(
    mean_score        = round(mean(score, na.rm = TRUE), 2),
    median_score      = median(score, na.rm = TRUE),
    min_score         = min(score, na.rm = TRUE),
    max_score         = max(score, na.rm = TRUE),
    pct_zero_or_below = round(100 * mean(score <= 0, na.rm = TRUE), 1)
  )

save_csv(score_summary_pu, "scores", "top1pct_score_summary.csv")

# -- 4b. Mean score by quintile --

score_quintiles_pu <- ce_pu %>%
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
  mutate(label = paste0("Q", quintile, "\n(", min_score, " to ", max_score, ")"))

save_csv(score_quintiles_pu %>% select(-label), "scores", "top1pct_score_quintiles.csv")

p_top1pct_score_quintiles <- score_quintiles_pu %>%
  ggplot(aes(x = label, y = mean_score)) +
  geom_col(fill = col_neutral) +
  geom_text(aes(label = sprintf("%.1f", mean_score)),
            vjust = -0.4, size = 3.5, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Power users: mean score by quintile",
    subtitle = "Each quintile ~20% of power user comments. Range shows min to max within quintile.",
    x = NULL, y = "Mean score"
  ) +
  base_theme

save_fig(p_top1pct_score_quintiles, "scores", "top1pct_score_distribution.png")

# -- 4c. Mean score by conversation outcome --

score_by_outcome_pu <- ce_pu %>%
  mutate(outcome = ifelse(has_removed_comment, "Awry", "On-track")) %>%
  group_by(outcome) %>%
  summarise(
    n_comments   = n(),
    mean_score   = round(mean(score, na.rm = TRUE), 2),
    median_score = median(score, na.rm = TRUE),
    .groups = "drop"
  )

save_csv(score_by_outcome_pu, "scores", "top1pct_score_by_outcome.csv")

p_top1pct_score_outcome <- score_by_outcome_pu %>%
  ggplot(aes(x = outcome, y = mean_score, fill = outcome)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = sprintf("%.1f", mean_score)),
            vjust = -0.4, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Awry" = col_awry, "On-track" = col_ontrack)) +
  scale_y_continuous(limits = c(0, 50)) +  # Matched to full-sample chart for comparability
  labs(
    title    = "Power users: mean score by conversation outcome",
    subtitle = paste0("Top 1% users (n = ", n_power, ") only"),
    x = NULL, y = "Mean score"
  ) +
  base_theme +
  theme(legend.position = "none")

save_fig(p_top1pct_score_outcome, "scores", "top1pct_score_by_outcome.png", width = 6, height = 5)

# -- 4d. Median score by conversation outcome --
p_top1pct_score_outcome_median <- score_by_outcome_pu %>%
  ggplot(aes(x = outcome, y = median_score, fill = outcome)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = sprintf("%.0f", median_score)),
            vjust = -0.4, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Awry" = col_awry, "On-track" = col_ontrack)) +
  scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
  labs(
    title    = "Power users: median score by conversation outcome",
    subtitle = paste0("Top 1% users (n = ", n_power, ") only"),
    x = NULL, y = "Median score"
  ) +
  base_theme +
  theme(legend.position = "none")

save_fig(p_top1pct_score_outcome_median, "scores", "top1pct_score_by_outcome_median.png", width = 6, height = 5)

p_top1pct_score_outcome_combined <- p_top1pct_score_outcome | p_top1pct_score_outcome_median
save_fig(p_top1pct_score_outcome_combined, "scores", "top1pct_score_by_outcome_mean_median.png", width = 11, height = 5)


# -- 4e. Mean score by year --

score_by_year_pu <- ce_pu %>%
  group_by(year) %>%
  summarise(
    mean_score   = round(mean(score, na.rm = TRUE), 2),
    median_score = median(score, na.rm = TRUE),
    .groups = "drop"
  )

save_csv(score_by_year_pu, "scores", "top1pct_score_by_year.csv")

p_top1pct_score_year <- score_by_year_pu %>%
  ggplot(aes(x = factor(year), y = mean_score)) +
  geom_col(fill = col_neutral) +
  labs(
    title    = "Power users: mean score by year",
    subtitle = paste0("Top 1% users (n = ", n_power, ") only"),
    x = "Year", y = "Mean score"
  ) +
  base_theme

save_fig(p_top1pct_score_year, "scores", "top1pct_score_by_year.png")


# === SECTION 5: TEXT LENGTH ===

cat("\n=== TEXT LENGTH (POWER USERS) ===\n")

# Summary table by comment type × outcome
text_summary_pu <- ce_pu %>%
  mutate(outcome = ifelse(has_removed_comment, "Awry", "On-track")) %>%
  group_by(comment_type, outcome) %>%
  summarise(
    mean_words   = round(mean(word_count, na.rm = TRUE), 1),
    median_words = median(word_count, na.rm = TRUE),
    mean_chars   = round(mean(char_count, na.rm = TRUE), 0),
    median_chars = median(char_count, na.rm = TRUE),
    .groups = "drop"
  )

save_csv(text_summary_pu, "text_length", "top1pct_text_length_by_type_and_outcome.csv")

# Word count histogram by outcome (trimmed at 99th percentile; style mirrors full sample)
bin_width <- 10

word_count_bins_pu <- ce_pu %>%
  filter(!is.na(word_count),
         word_count <= quantile(word_count, 0.99, na.rm = TRUE)) %>%
  mutate(
    outcome = ifelse(has_removed_comment, "Awry", "On-track"),
    bin     = floor(word_count / bin_width) * bin_width
  ) %>%
  count(bin, outcome) %>%
  pivot_wider(names_from = outcome, values_from = n, values_fill = 0)

p_top1pct_wordcount <- word_count_bins_pu %>%
  ggplot(aes(x = bin)) +
  geom_col(aes(y = `On-track`, fill = "On-track"), width = bin_width * 0.9) +
  geom_point(aes(y = Awry, colour = "Awry"), size = 1.5) +
  scale_fill_manual(values   = c("On-track" = "#1565C0"), name = "Outcome") +
  scale_colour_manual(values = c("Awry"     = "#E53935"), name = "Outcome") +
  labs(
    title    = "Power users: word count distribution by outcome (trimmed at 99th percentile)",
    subtitle = paste0("Top 1% users (n = ", n_power, ") only"),
    x = "Word count", y = "Count"
  ) +
  base_theme +
  theme(legend.position = "top")

save_fig(p_top1pct_wordcount, "text_length", "top1pct_word_count_by_outcome.png")


# === SECTION 6: DELTA RATE SCATTER ===

cat("\n=== DELTA RATE SCATTER (POWER USERS) ===\n")

# y = max_delta (career flair) / n_comments (in this dataset)
# Interpretation: approximates persuasion rate per comment.
# Assumption: a user's share of activity captured in this dataset is roughly
# constant across users, so in-sample comment count is a fair denominator.
# Imperfect — career deltas can predate or extend beyond the dataset window —
# but the best available normalization given data constraints.

delta_scatter <- power_users %>%
  mutate(delta_rate = ifelse(!is.na(max_delta), round(max_delta / n_comments, 4), NA_real_))

n_no_flair <- sum(is.na(delta_scatter$delta_rate))
cat(sprintf("  Users excluded from scatter (no delta flair): %d\n", n_no_flair))

save_csv(
  delta_scatter %>% select(speaker, n_comments, max_delta, delta_rate),
  "user_participation", "top1pct_delta_scatter_data.csv"
)

# Annotate the #1 poster
top_commenter <- delta_scatter %>% arrange(desc(n_comments)) %>% slice(1)

p_top1pct_delta_scatter <- delta_scatter %>%
  filter(!is.na(delta_rate)) %>%
  ggplot(aes(x = n_comments, y = delta_rate)) +
  geom_point(colour = col_neutral, alpha = 0.7, size = 2.5) +
  geom_point(data = filter(delta_scatter, !is.na(delta_rate)) %>%
               arrange(desc(n_comments)) %>% slice(1),
             colour = col_awry, size = 3.5) +
  geom_text(data = filter(delta_scatter, !is.na(delta_rate)) %>%
              arrange(desc(n_comments)) %>% slice(1),
            aes(label = paste0("#1: ", scales::comma(n_comments), " comments")),
            hjust = -0.08, size = 3.2, colour = col_awry) +
  scale_x_log10(
    labels = scales::comma,
    breaks = c(50, 100, 250, 500, 1000, 3000)
  ) +
  scale_y_continuous(labels = function(x) sprintf("%.2f", x)) +
  labs(
    title    = "Power users (top 1%): comment volume vs delta rate",
    subtitle = paste0(
      "x-axis log-scaled. y = career deltas (from flair) / comments in this dataset.\n",
      "Approximates persuasion rate per comment; assumes in-sample activity ~ overall activity.\n",
      if (n_no_flair > 0) sprintf("%d users with no delta flair excluded.", n_no_flair) else ""
    ),
    x = "Number of comments (log scale)",
    y = "Delta rate (career deltas per in-sample comment)"
  ) +
  base_theme

save_fig(p_top1pct_delta_scatter, "user_participation", "top1pct_delta_scatter.png",
         width = 9, height = 6)


# === SECTION 7: SCORE VS VOLUME SCATTER ===

cat("\n=== SCORE VS VOLUME SCATTER (POWER USERS) ===\n")

# Per-user: mean score and awry participation rate (computed in Section 3b)
top1pct_score_vol <- power_users %>%
  left_join(
    top1pct_awry_rate %>% select(speaker, pct_awry),
    by = "speaker"
  )

save_csv(
  top1pct_score_vol %>% select(speaker, n_comments, mean_score, pct_awry),
  "scores", "top1pct_score_vs_volume_data.csv"
)

# Annotate the #1 poster
top_commenter_sv <- top1pct_score_vol %>% arrange(desc(n_comments)) %>% slice(1)

top1pct_score_vol <- top1pct_score_vol %>%
  mutate(high_awry = ifelse(pct_awry >= 75, "≥75% awry", "<75% awry"))

p_top1pct_score_vol <- top1pct_score_vol %>%
  ggplot(aes(x = n_comments, y = mean_score, colour = high_awry)) +
  geom_point(alpha = 0.8, size = 2.5) +
  geom_point(data = top_commenter_sv,
             size = 3.8, shape = 21,
             colour = "black", fill = NA, stroke = 1) +
  geom_text(data = top_commenter_sv,
            aes(label = paste0("#1: ", scales::comma(n_comments))),
            hjust = 1.1, vjust = -1, size = 3.2, colour = "black") +
  scale_x_log10(
    labels = scales::comma,
    breaks = c(50, 100, 250, 500, 1000, 3000),
    expand = expansion(mult = c(0.05, 0.12))
  ) +
  scale_colour_manual(
    values = c("≥75% awry" = col_awry, "<75% awry" = col_neutral),
    name   = "Participation in 'awry' threads"
  ) +
  labs(
    title    = "Power users (top 1%): comment volume vs mean score",
    subtitle = "x-axis log-scaled. Red = users whose own comments are in awry conversations ≥75% of the time.",
    x        = "Number of comments (log scale)",
    y        = "Mean comment score"
  ) +
  base_theme +
  theme(
    legend.position    = c(0.78, 0.85),
    legend.background  = element_rect(fill = "white", colour = "grey70", linewidth = 0.4),
    legend.title       = element_text(size = 9),
    legend.text        = element_text(size = 8),
    legend.box.margin  = margin(4, 6, 4, 6)
  )

save_fig(p_top1pct_score_vol, "scores", "top1pct_score_vs_volume.png", width = 8, height = 6)