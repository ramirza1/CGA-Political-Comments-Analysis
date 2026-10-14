# 3. descriptives_users.R for Conversations Gone Awry (CMV) - Political Comments Analysis
#
# User-level descriptives: who participates, how concentrated activity and removals are,
# and how delta flair and [deleted] accounts are distributed. No text coding.
#
# Taxonomy (as in script 2):
#   Thread / first reply / challenger / path / pair. A "user" is a Reddit account (speaker).
#   [deleted] is NOT a user: it is Reddit's placeholder for any account deleted or banned
#   before collection, so it is excluded from sections 1-7 and described in section 8.
#
# Sections (output files are numbered to match):
#   -  User table (one row per identifiable user) - built first, saved at the end
#   1. Activity concentration: quintiles and top groups, comments per user,
#      concentration curve, top 1% / top 5% users by volume, participation profile
#      (comment length, reply time, time span, posting frequency)
#   2. Challengers: how many users ever write a first reply
#   3. Removal concentration among identifiable users
#   4. Each user's share of awry paths vs. the 50% expected by design
#   5. Removal rate by activity group
#      5b. Removal rate per user, in bins (users with MIN_COMMENTS_RATE+ comments)
#      5c. Removal rate vs comment volume (same users)
#      5d. High-removal user groups compared (3+ removed / top X% by rate / above chance)
#   6. Delta flair: distribution, comments vs deltas, by activity group,
#      removed-comment authors vs others
#   7. [deleted] accounts
#
# Percentage axes run 0-100%; count axes put the tallest bar at ~80% of the axis.
#
# Figures group long tails for readability; every grouped figure has a matching
# *_uncapped table with the full breakdown.
#
# Outputs:
#   input_data_anonymized/user_table_<sample>.csv    (user IDs - shareable)
#   input_data_from_python/user_table_<sample>.csv   (usernames, via the private key - local only)
#   descriptive_results/users/*_<sample>.csv
#   graphical_output/users/*_<sample>.png
#
# Run after 1_data_prep.R and 1b_pseudonymize_users.R


# === SECTION 0: PREP ===

# -- 0. Packages --
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggpubr, patchwork, scales)

# -- 1. Sample switch --
# TRUE  = paths with a post ID only (Aug 2017 onward, ~83% of pairs). Default.
# FALSE = full corpus.
USE_POST_ID_SAMPLE <- TRUE

# Removal-rate settings (section 5b-5d)
# MIN_COMMENTS_RATE: at 25 comments, a user whose true rate is the average has about a 5% chance
# of showing double the average by chance (the script prints the exact figure).
MIN_COMMENTS_RATE <- 25   # users need this many comments for their removal rate to be shown
MIN_ENDINGS       <- 5    # ...and this many path endings for the removed share of endings (as in 3b)
Z_THRESHOLD       <- 2    # removed share of endings above 50% by z >= this = "above chance" (as in 3b)
REPEAT_MIN        <- 3    # repeat removal author = this many removed comments or more
TOP_RATE_PCT      <- 10   # "top X%" of users by removal rate (among users with MIN_COMMENTS_RATE+ comments)

SAMPLE_TAG   <- if (USE_POST_ID_SAMPLE) "postid" else "full"
SAMPLE_LABEL <- if (USE_POST_ID_SAMPLE) "Paths with post ID (Aug 2017-Dec 2022)" else "Full corpus (Aug 2015-Dec 2022)"

# -- 2. Paths and helpers --
PREPPED   <- "prepped_data_anonymized"   # shareable copy with user IDs (from 1b_pseudonymize_users.R)
SHARED_USER_DIR  <- "input_data_anonymized"                                # user table with IDs
PRIVATE_USER_DIR <- "input_data_from_python"                               # user table with usernames (local)
KEY_FILE         <- file.path(PRIVATE_USER_DIR, "PRIVATE_user_id_key.csv")
DESC      <- file.path("descriptive_results", "users")
FIGS      <- file.path("graphical_output", "users")
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

col_awry     <- "#EF5350"
col_ontrack  <- "#81C784"
col_neutral  <- "#5C85D6"
col_expected <- "#BDBDBD"
col_deleted  <- "#757575"
col_groups   <- c("One comment"                     = "#B3CDE3",
                  "Other (2+ comments, bottom 95%)" = "#6BAED6",
                  "Top 5% (excl. top 1%)"           = "#CD7F32",
                  "Top 1%"                          = "#B8860B")

# Y axis for count bar charts: the tallest bar sits at ~80% of the axis height
scale_y_80 <- function(...) scale_y_continuous(..., expand = expansion(mult = c(0, 0.25)))

# Y axis for percentage bar charts: always the full 0-100% scale.
# (max_val is accepted for compatibility but not used.) clip = "off" keeps value labels
# visible when bars are close to 100%.
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

# Concentration curve: users sorted from most to least on x; cumulative share of the total
concentration_curve <- function(x, label) {
  x <- sort(x, decreasing = TRUE)
  tibble(
    measure       = label,
    cum_pct_users = c(0, 100 * seq_along(x) / length(x)),
    cum_pct_total = c(0, 100 * cumsum(x) / sum(x))
  )
}

ACTIVITY_LEVELS <- names(col_groups)

# -- 3. Load data and apply sample --
message("Loading prepped data...")
ce <- readRDS(file.path(PREPPED, "comments_enriched.rds"))

if (USE_POST_ID_SAMPLE) {
  ce <- ce %>% filter(has_post_id)
}

# Delta flair: author_flair_text holds the user's career delta count (e.g. "28" + delta sign),
# as displayed when the comment was collected. NA = no flair shown.
ce <- ce %>%
  mutate(
    outcome     = factor(if_else(has_removed_comment, "Awry", "On-track"),
                         levels = c("Awry", "On-track")),
    delta_count = as.numeric(str_extract(author_flair_text, "\\d+"))
  ) %>%
  arrange(conversation_id, thread_depth) %>%
  group_by(conversation_id) %>%
  # Reply time = hours since the comment being replied to (paths are linear, so this is
  # the previous comment). NA for first replies: the post itself is not in the data.
  mutate(reply_latency_h = (timestamp - lag(timestamp)) / 3600,
         # Final comment of the path: the only comment that can be removed in this dataset
         is_final_comment = thread_depth == max(thread_depth)) %>%
  ungroup()

ce_known <- ce %>% filter(speaker != "[deleted]")

cap <- paste0("Sample: ", SAMPLE_LABEL, ". [deleted] excluded (see section 7). ")

cat(sprintf("\nSample: %s | %s comments | %s identifiable users\n",
            SAMPLE_LABEL, fmt(nrow(ce)), fmt(n_distinct(ce_known$speaker))))


# === USER TABLE (built here, saved in section 8) ===

cat("\n=== USER TABLE (built here, saved in section 8) ===\n")

users <- ce_known %>%
  group_by(speaker) %>%
  summarise(
    n_comments         = n(),
    n_paths            = n_distinct(conversation_id),
    n_pairs            = n_distinct(pair_key),
    n_threads          = n_distinct(post_id, na.rm = TRUE),
    n_awry_paths       = n_distinct(conversation_id[has_removed_comment]),
    n_ontrack_paths    = n_distinct(conversation_id[!has_removed_comment]),
    n_first_replies    = sum(thread_depth == 0),
    n_removed_comments = sum(is_removed_comment),
    n_endings          = sum(is_final_comment),   # paths whose final comment the user wrote
    has_delta_flair    = any(!is.na(author_flair_text)),
    max_delta_flair    = suppressWarnings(max(delta_count, na.rm = TRUE)),
    n_flair_values     = n_distinct(delta_count, na.rm = TRUE),
    median_word_count  = median(word_count, na.rm = TRUE),
    median_reply_latency_h = median(reply_latency_h[thread_depth > 0], na.rm = TRUE),
    n_active_months    = n_distinct(format(timestamp_dt, "%Y-%m")),
    first_comment      = min(timestamp_dt),
    last_comment       = max(timestamp_dt),
    .groups = "drop"
  ) %>%
  mutate(
    # Span and frequency are measured within this dataset (sampled paths only), not
    # across the user's full Reddit history
    active_days               = as.numeric(difftime(last_comment, first_comment, units = "days")),
    comments_per_active_month = n_comments / n_active_months,
    max_delta_flair  = if_else(is.infinite(max_delta_flair), NA_real_, max_delta_flair),
    share_paths_awry = n_awry_paths / n_paths,
    removal_rate     = n_removed_comments / n_comments,
    # Removed share of the user's path endings (50% expected by design: one awry, one on-track per pair)
    removed_share_endings = if_else(n_endings > 0, n_removed_comments / n_endings, NA_real_),
    is_challenger    = n_first_replies > 0,
    ever_removed     = n_removed_comments > 0
  )

# Activity groups (mutually exclusive), by number of comments
q99 <- quantile(users$n_comments, 0.99)
q95 <- quantile(users$n_comments, 0.95)
q90 <- quantile(users$n_comments, 0.90)

users <- users %>%
  mutate(activity_group = factor(case_when(
    n_comments >= q99 ~ "Top 1%",
    n_comments >= q95 ~ "Top 5% (excl. top 1%)",
    n_comments == 1   ~ "One comment",
    TRUE              ~ "Other (2+ comments, bottom 95%)"
  ), levels = ACTIVITY_LEVELS)) %>%
  relocate(speaker, activity_group)

activity_thresholds <- tibble(
  Threshold = c("Top 1% (99th percentile)", "Top 5% (95th percentile)", "Top 10% (90th percentile)"),
  Min_comments = c(q99, q95, q90)
)

save_csv(activity_thresholds, "1a_activity_group_thresholds")

# The user table is saved at the end of the script (section 9), once the role, flair,
# and removal-status columns from later sections have been added.


# === SECTION 1: ACTIVITY CONCENTRATION ===

cat("\n=== SECTION 1: ACTIVITY CONCENTRATION ===\n")

n_comments_known <- sum(users$n_comments)

# -- 1a. Volume summary: quintiles, top groups, one-time commenters, [deleted] --
# Percentages of comments use ALL comments as the base (including [deleted]), so
# "All identifiable users" + "[deleted] accounts" = 100%, as in the earlier analysis.
# Quintiles: users ranked by comments and split into five equal-sized groups. Most users
# have only 1-2 comments, so tied users are split across the lower quintiles arbitrarily.
# Top 10/5/1% use the percentile thresholds (>=), so they overlap Q5 and each other.
n_comments_all   <- nrow(ce)
n_comments_known <- sum(users$n_comments)

users <- users %>%
  mutate(activity_quintile = factor(paste0("Q", ntile(n_comments, 5)),
                                    levels = paste0("Q", 1:5)))

volume_group <- function(label, keep) {
  tibble(
    Group               = label,
    N_users             = sum(keep),
    Pct_of_all_users    = round(100 * mean(keep), 1),
    Mean_comments       = round(mean(users$n_comments[keep]), 1),
    Median_comments     = median(users$n_comments[keep]),
    Pct_of_all_comments = round(100 * sum(users$n_comments[keep]) / n_comments_all, 1)
  )
}

VOLUME_LEVELS <- c("All identifiable users", "One-time commenters",
                   "Q1 (least active)", "Q2", "Q3", "Q4", "Q5 (most active)",
                   "Top 10%", "Top 5%", "Top 1%", "[deleted] accounts")

volume_summary <- bind_rows(
  volume_group("All identifiable users", rep(TRUE, nrow(users))),
  volume_group("One-time commenters",    users$n_comments == 1),
  volume_group("Q1 (least active)",      users$activity_quintile == "Q1"),
  volume_group("Q2",                     users$activity_quintile == "Q2"),
  volume_group("Q3",                     users$activity_quintile == "Q3"),
  volume_group("Q4",                     users$activity_quintile == "Q4"),
  volume_group("Q5 (most active)",       users$activity_quintile == "Q5"),
  volume_group("Top 10%",                users$n_comments >= q90),
  volume_group("Top 5%",                 users$n_comments >= q95),
  volume_group("Top 1%",                 users$n_comments >= q99),
  tibble(Group = "[deleted] accounts", N_users = NA, Pct_of_all_users = NA,
         Mean_comments = NA, Median_comments = NA,
         Pct_of_all_comments = round(100 * sum(ce$speaker == "[deleted]") / n_comments_all, 1))
)

save_csv(volume_summary, "1a_activity_volume_summary")

# Figure: share of all comments and mean comments per user, quintiles then top groups
col_volume <- c("Q1 (least active)" = "#C6DBEF", "Q2" = "#9ECAE1", "Q3" = "#6BAED6",
                "Q4" = "#3182BD", "Q5 (most active)" = "#08519C",
                "Top 10%" = "#D9A441", "Top 5%" = "#CD7F32", "Top 1%" = "#B8860B")

volume_plot_data <- volume_summary %>%
  filter(Group %in% names(col_volume)) %>%
  mutate(Group = factor(Group, levels = names(col_volume)))

volume_figure <- function(groups, title, note) {
  d <- volume_plot_data %>% filter(Group %in% groups) %>% droplevels()
  
  p_share <- d %>%
    ggplot(aes(x = Group, y = Pct_of_all_comments, fill = Group)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = paste0(Pct_of_all_comments, "%")), vjust = -0.4, size = 3.4) +
    scale_fill_manual(values = col_volume) +
    scale_y_pct() +
    scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
    labs(title = "Share of all comments written", x = NULL, y = "% of all comments") +
    base_theme +
    theme(legend.position = "none", plot.title = element_text(size = 12))
  
  p_mean <- d %>%
    ggplot(aes(x = Group, y = Mean_comments, fill = Group)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = Mean_comments), vjust = -0.4, size = 3.4) +
    scale_fill_manual(values = col_volume) +
    scale_y_80() +
    scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
    labs(title = "Mean comments per user", x = NULL, y = "Comments per user") +
    base_theme +
    theme(legend.position = "none", plot.title = element_text(size = 12))
  
  (p_share | p_mean) +
    plot_annotation(
      title   = title,
      caption = paste0(cap, note),
      theme   = theme(plot.title   = element_text(face = "bold", size = 14),
                      plot.caption = element_text(hjust = 0, colour = "#666666", size = 8))
    )
}

save_fig(volume_figure(c("Q1 (least active)", "Q2", "Q3", "Q4", "Q5 (most active)"),
                       "Activity by quintile of users",
                       "Quintiles = five equal-sized groups by comments; ties split arbitrarily (most users write 1-2 comments)."),
         "1a_activity_quintiles", width = 12, height = 5.5)

save_fig(volume_figure(c("Top 10%", "Top 5%", "Top 1%"),
                       "Activity of the most active users",
                       "Groups overlap: the top 1% is part of the top 5%, which is part of the top 10%."),
         "1a_activity_top_groups", width = 10, height = 5.5)

# Mutually exclusive groups (used in sections 5 and 6)
activity_groups <- users %>%
  group_by(activity_group) %>%
  summarise(
    n_users         = n(),
    median_comments = median(n_comments),
    total_comments  = sum(n_comments),
    .groups = "drop"
  ) %>%
  mutate(
    pct_users    = round(100 * n_users / sum(n_users), 1),
    pct_comments = round(100 * total_comments / sum(total_comments), 1)
  )

save_csv(activity_groups, "1a_activity_groups")

# -- 1b. Comments per user --
comment_bins <- c(0, 1, 2, 3, 4, 5, 10, 20, 50, 100, Inf)
comment_labels <- c("1", "2", "3", "4", "5", "6-10", "11-20", "21-50", "51-100", "100+")

comments_per_user <- users %>%
  mutate(bin = cut(n_comments, breaks = comment_bins, labels = comment_labels)) %>%
  count(bin, name = "n_users") %>%
  mutate(pct = round(100 * n_users / sum(n_users), 1))

save_csv(comments_per_user, "1b_comments_per_user")
save_csv(uncapped_count(users, n_comments), "1b_comments_per_user_uncapped")

p_comments_per_user <- comments_per_user %>%
  ggplot(aes(x = bin, y = pct)) +
  geom_col(fill = col_neutral, width = 0.75) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.4, size = 3.2) +
  scale_y_pct(max(comments_per_user$pct)) +
  labs(title = "Comments per user",
       x = "Comments written by the user", y = "% of users", caption = cap) +
  base_theme

save_fig(p_comments_per_user, "1b_comments_per_user")

# -- 1c. Paths per user (how many paths each user appears in) --
save_csv(uncapped_count(users, n_paths), "1c_paths_per_user_uncapped")

# -- 1d. Concentration curve: share of comments written by the most active users --
curve_comments <- concentration_curve(users$n_comments, "Comments")

p_concentration <- curve_comments %>%
  ggplot(aes(x = cum_pct_users, y = cum_pct_total)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "#9E9E9E") +
  geom_line(colour = col_neutral, linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 100, 10), labels = function(x) paste0(x, "%")) +
  scale_y_continuous(breaks = seq(0, 100, 20), labels = function(x) paste0(x, "%")) +
  labs(title = "How concentrated is activity?",
       subtitle = "Users sorted from most to least active. Dashed line = every user writes the same amount.",
       x = "Cumulative % of users (most active first)", y = "Cumulative % of comments",
       caption = cap) +
  base_theme

save_fig(p_concentration, "1d_activity_concentration_curve", width = 7, height = 6)

# -- 1e. Top 1% and top 5% users by comment volume (ranked, log scale) --
plot_top_users <- function(threshold, label, colour) {
  top <- users %>%
    filter(n_comments >= threshold) %>%
    arrange(desc(n_comments), speaker) %>%
    mutate(rank = row_number())
  n_top    <- nrow(top)
  mean_top <- mean(top$n_comments)
  x_step   <- if (n_top > 600) 300 else 50
  x_max    <- ceiling(n_top / x_step) * x_step
  
  top %>%
    ggplot(aes(x = rank, y = n_comments)) +
    geom_col(fill = colour, width = 0.8) +
    annotate("text", x = 1, y = top$n_comments[1],
             label = paste0("#1: ", comma(top$n_comments[1]), " comments"),
             vjust = -0.6, hjust = 0, size = 3.5, fontface = "bold") +
    geom_hline(yintercept = mean_top, linetype = "dashed", colour = "darkred", linewidth = 0.8) +
    annotate("text", x = x_max * 0.97, y = mean_top,
             label = paste0("Mean: ", round(mean_top)),
             vjust = -0.6, hjust = 1, colour = "darkred", size = 3.5) +
    scale_y_log10(labels = comma, breaks = c(1, 10, 100, 1000), limits = c(NA, 1000)) +
    scale_x_continuous(breaks = c(1, seq(x_step, x_max, by = x_step)),
                       limits = c(0.5, x_max + 0.5)) +
    labs(title = paste0(label, " users by comment volume (n = ", comma(n_top), ", log scale)"),
         x = "Rank", y = "Number of comments (log scale)",
         caption = paste0(cap, "Users ranked by number of comments; ties broken by user ID.")) +
    base_theme
}

save_fig(plot_top_users(q99, "Top 1%", col_groups[["Top 1%"]]),
         "1e_top1pct_users_by_volume", width = 10, height = 6)
save_fig(plot_top_users(q95, "Top 5%", col_groups[["Top 5% (excl. top 1%)"]]),
         "1f_top5pct_users_by_volume", width = 10, height = 6)

# -- 1g. Participation profile: comment length, reply time, time span, frequency --
# All identifiable users, then the bottom 90% and the (overlapping) top 10/5/1%.
# Time span and frequency are measured within this dataset (sampled paths), so a user
# with one comment has a span of 0 days and 1 active month.
profile_metrics <- tribble(
  ~var,                         ~label,
  "median_word_count",          "Median words per comment",
  "median_reply_latency_h",     "Median reply time (hours)",
  "active_days",                "Time on forum (days)",
  "n_active_months",            "Months with at least one comment",
  "comments_per_active_month",  "Comments per active month"
)

PROFILE_LEVELS <- c("All identifiable users", "Bottom 90%", "Top 10%", "Top 5%", "Top 1%")
profile_groups <- list(
  "All identifiable users" = rep(TRUE, nrow(users)),
  "Bottom 90%"             = users$n_comments <  q90,
  "Top 10%"                = users$n_comments >= q90,
  "Top 5%"                 = users$n_comments >= q95,
  "Top 1%"                 = users$n_comments >= q99
)

participation_profile <- imap_dfr(profile_groups, function(keep, g) {
  users[keep, ] %>%
    select(all_of(profile_metrics$var)) %>%
    mutate(across(everything(), as.numeric)) %>%
    pivot_longer(everything(), names_to = "var", values_to = "value") %>%
    mutate(group = g)
}) %>%
  group_by(group, var) %>%
  summarise(
    n_users = sum(!is.na(value)),
    mean    = mean(value, na.rm = TRUE),
    sd      = sd(value, na.rm = TRUE),
    median  = median(value, na.rm = TRUE),
    p25     = quantile(value, 0.25, na.rm = TRUE),
    p75     = quantile(value, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # 95% CI of the mean (t-based; undefined for groups with fewer than 2 users)
    se         = sd / sqrt(n_users),
    t95        = if_else(n_users > 1, qt(0.975, df = n_users - 1), NA_real_),
    ci95_lower = mean - t95 * se,
    ci95_upper = mean + t95 * se
  ) %>%
  left_join(profile_metrics, by = "var") %>%
  mutate(
    group = factor(group, levels = PROFILE_LEVELS),
    label = factor(label, levels = profile_metrics$label),
    across(c(mean, sd, se, ci95_lower, ci95_upper, median, p25, p75), ~ round(.x, 2))
  ) %>%
  arrange(label, group) %>%
  select(label, group, n_users, mean, ci95_lower, ci95_upper, sd, se, median, p25, p75, var)

save_csv(participation_profile, "1g_participation_profile")

col_profile <- c("All identifiable users" = col_neutral, "Bottom 90%" = "#9ECAE1",
                 "Top 10%" = "#D9A441", "Top 5%" = "#CD7F32", "Top 1%" = "#B8860B")

p_profile <- participation_profile %>%
  ggplot(aes(x = group, y = mean, fill = group)) +
  geom_col(width = 0.7) +
  # all measures are non-negative, so the lower CI is floored at 0 for display only
  geom_errorbar(aes(ymin = pmax(ci95_lower, 0), ymax = ci95_upper), width = 0.2, colour = "#333333") +
  geom_text(aes(label = round(mean, 1), y = coalesce(ci95_upper, mean)), vjust = -0.6, size = 2.8) +
  facet_wrap(~ label, scales = "free_y", ncol = 3, labeller = label_wrap_gen(30)) +
  scale_fill_manual(values = col_profile) +
  scale_y_80() +
  labs(title = "Participation profile: all users vs the most active",
       subtitle = "Bars = mean across users; lines = 95% confidence interval of the mean",
       x = NULL, y = NULL,
       caption = paste0(cap, "Top groups overlap (top 1% is within top 5% and top 10%), so their intervals are not independent comparisons. ",
                        "Time on forum (first to last comment) and frequency are measured within this dataset.\n",
                        "Reply time = hours since the comment being replied to (first replies excluded: the post is not in the data). ",
                        "Word count and reply time are each user's median, averaged across users. Medians and IQRs are in the CSV.")) +
  base_theme +
  theme(legend.position = "top",
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text   = element_text(face = "bold", size = 9)) +
  guides(fill = guide_legend(nrow = 1))

save_fig(p_profile, "1g_participation_profile", width = 12, height = 8)


# === SECTION 2: CHALLENGERS ===

cat("\n=== SECTION 2: CHALLENGERS ===\n")

# Challenger = author of a first reply. No attempt is made to identify original posters.
users <- users %>%
  mutate(role = factor(case_when(
    n_first_replies > 0 & n_first_replies == n_comments ~ "Only writes first replies",
    n_first_replies > 0                                 ~ "Writes first replies and deeper replies",
    TRUE                                                ~ "Only writes deeper replies"
  ), levels = c("Only writes first replies", "Writes first replies and deeper replies",
                "Only writes deeper replies")))

n_first_replies_known <- sum(users$n_first_replies)

challenger_summary <- tibble(
  Metric = c("Identifiable users", "Users who wrote >=1 first reply (challengers)",
             "% of users who are challengers", "First replies by identifiable users",
             "Median first replies per challenger", "Max first replies by one challenger",
             "% of first replies written by top 1% users"),
  Value = c(
    fmt(nrow(users)),
    fmt(sum(users$is_challenger)),
    sprintf("%.1f%%", 100 * mean(users$is_challenger)),
    fmt(n_first_replies_known),
    fmt(median(users$n_first_replies[users$is_challenger])),
    fmt(max(users$n_first_replies)),
    sprintf("%.1f%%", 100 * sum(users$n_first_replies[users$activity_group == "Top 1%"]) /
              n_first_replies_known)
  )
)

save_csv(challenger_summary, "2a_challenger_summary")

role_dist <- users %>%
  count(role, name = "n_users") %>%
  mutate(pct = round(100 * n_users / sum(n_users), 1))

save_csv(role_dist, "2b_user_roles")

role_by_group <- users %>%
  count(activity_group, role, name = "n_users") %>%
  group_by(activity_group) %>%
  mutate(pct = round(100 * n_users / sum(n_users), 1)) %>%
  ungroup()

save_csv(role_by_group, "2b_user_roles_by_activity_group")

first_reply_bins   <- c(0, 1, 2, 3, 5, 10, Inf)
first_reply_labels <- c("1", "2", "3", "4-5", "6-10", "11+")

first_replies_per_challenger <- users %>%
  filter(is_challenger) %>%
  mutate(bin = cut(n_first_replies, breaks = first_reply_bins, labels = first_reply_labels)) %>%
  count(bin, name = "n_users") %>%
  mutate(pct = round(100 * n_users / sum(n_users), 1))

save_csv(first_replies_per_challenger, "2c_first_replies_per_challenger")
save_csv(uncapped_count(users %>% filter(is_challenger), n_first_replies),
         "2c_first_replies_per_challenger_uncapped")

p_roles <- role_dist %>%
  ggplot(aes(x = role, y = pct)) +
  geom_col(fill = col_neutral, width = 0.6) +
  geom_text(aes(label = paste0(pct, "%\n(", comma(n_users), ")")), vjust = -0.3, size = 3.2) +
  scale_y_pct(max(role_dist$pct)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 22)) +
  labs(title = "What roles do users play?",
       x = NULL, y = "% of users", caption = paste0(cap, "First reply = a comment replying directly to the post.")) +
  base_theme

p_first_replies <- first_replies_per_challenger %>%
  ggplot(aes(x = bin, y = pct)) +
  geom_col(fill = col_neutral, width = 0.7) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.4, size = 3.2) +
  scale_y_pct(max(first_replies_per_challenger$pct)) +
  labs(title = "First replies per challenger",
       x = "First replies written", y = "% of challengers", caption = cap) +
  base_theme

save_fig(p_roles, "2b_user_roles", width = 8, height = 5.5)
save_fig(p_first_replies, "2c_first_replies_per_challenger", width = 8, height = 5.5)


# === SECTION 3: REMOVAL CONCENTRATION ===

cat("\n=== SECTION 3: REMOVAL CONCENTRATION ===\n")

# Removed comment = final comment of an awry path (one per awry path, by construction).
# Removals of comments outside the sampled paths are not observed.
removal_authors <- users %>% filter(ever_removed)
n_removed_known <- sum(users$n_removed_comments)

removal_summary <- tibble(
  Metric = c("Removed comments by identifiable users",
             "Distinct authors of removed comments",
             "% of identifiable users with >=1 removed comment",
             "Authors with exactly 1 removed comment",
             "Authors with 2+ removed comments",
             "% of removed comments written by repeat (2+) authors",
             "Max removed comments by one author",
             "% of removed comments written by top 1% users",
             "% of all comments written by top 1% users"),
  Value = c(
    fmt(n_removed_known),
    fmt(nrow(removal_authors)),
    sprintf("%.1f%%", 100 * mean(users$ever_removed)),
    fmt(sum(removal_authors$n_removed_comments == 1)),
    fmt(sum(removal_authors$n_removed_comments >= 2)),
    sprintf("%.1f%%", 100 * sum(removal_authors$n_removed_comments[removal_authors$n_removed_comments >= 2]) /
              n_removed_known),
    fmt(max(users$n_removed_comments)),
    sprintf("%.1f%%", 100 * sum(users$n_removed_comments[users$activity_group == "Top 1%"]) / n_removed_known),
    sprintf("%.1f%%", 100 * sum(users$n_comments[users$activity_group == "Top 1%"]) / n_comments_known)
  )
)

save_csv(removal_summary, "3a_removal_concentration_summary")

removal_bins   <- c(0, 1, 2, 3, 5, 10, Inf)
removal_labels <- c("1", "2", "3", "4-5", "6-10", "11+")

removals_per_author <- removal_authors %>%
  mutate(bin = cut(n_removed_comments, breaks = removal_bins, labels = removal_labels)) %>%
  group_by(bin) %>%
  summarise(n_authors = n(), n_removed = sum(n_removed_comments), .groups = "drop") %>%
  mutate(
    pct_authors  = round(100 * n_authors / sum(n_authors), 1),
    pct_removals = round(100 * n_removed / sum(n_removed), 1)
  )

save_csv(removals_per_author, "3b_removals_per_author")
save_csv(uncapped_count(removal_authors, n_removed_comments, n_name = "n_authors"),
         "3b_removals_per_author_uncapped")

p_removals_per_author <- removals_per_author %>%
  select(bin, `% of authors` = pct_authors, `% of removed comments` = pct_removals) %>%
  pivot_longer(-bin, names_to = "measure", values_to = "pct") %>%
  ggplot(aes(x = bin, y = pct, fill = measure)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  geom_text(aes(label = paste0(pct, "%")), position = position_dodge(width = 0.85),
            vjust = -0.4, size = 3) +
  scale_fill_manual(values = c("% of authors" = col_neutral, "% of removed comments" = col_awry)) +
  scale_y_pct(max(c(removals_per_author$pct_authors, removals_per_author$pct_removals))) +
  labs(title = "Removed comments per author",
       subtitle = "Authors with at least one removed comment",
       x = "Removed comments written by the author", y = "%", caption = cap) +
  base_theme +
  theme(legend.position = "top")

save_fig(p_removals_per_author, "3b_removals_per_author")

# Concentration of removals vs concentration of activity
curves <- bind_rows(
  concentration_curve(users$n_comments,         "Comments"),
  concentration_curve(users$n_removed_comments, "Removed comments")
)

p_removal_curve <- curves %>%
  ggplot(aes(x = cum_pct_users, y = cum_pct_total, colour = measure)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "#9E9E9E") +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c("Comments" = col_neutral, "Removed comments" = col_awry)) +
  scale_x_continuous(breaks = seq(0, 100, 10), labels = function(x) paste0(x, "%")) +
  scale_y_continuous(breaks = seq(0, 100, 20), labels = function(x) paste0(x, "%")) +
  labs(title = "Are removals more concentrated than activity?",
       subtitle = "Each curve sorts users from most to least on that measure",
       x = "Cumulative % of users", y = "Cumulative % of total",
       caption = cap) +
  base_theme +
  theme(legend.position = "top")

save_fig(p_removal_curve, "3c_removal_vs_activity_concentration", width = 7, height = 6)


# === SECTION 4: SHARE OF AWRY PATHS PER USER ===

cat("\n=== SECTION 4: SHARE OF AWRY PATHS PER USER ===\n")

# By design half of all paths are awry, so a user who joins paths at random should
# have ~50% awry. The expected spread around 50% depends on how many paths a user is in,
# so observed shares are compared with a binomial(n_paths, 0.5) benchmark.
# Users in fewer than MIN_PATHS paths are excluded (their shares are too coarse).
# Note: a user in both paths of the same pair contributes one awry and one on-track
# path, which pulls their share towards 50%.
MIN_PATHS <- 5

share_breaks <- seq(0, 1, 0.1)
share_labels <- paste0(seq(0, 90, 10), "-", seq(10, 100, 10), "%")
bin_share <- function(s) cut(s, breaks = share_breaks, labels = share_labels,
                             include.lowest = TRUE, right = FALSE)

awry_users <- users %>% filter(n_paths >= MIN_PATHS)

observed_share <- awry_users %>%
  mutate(share_bin = bin_share(share_paths_awry)) %>%
  count(share_bin, name = "n_users") %>%
  mutate(type = "Observed")

expected_share <- awry_users %>%
  select(speaker, n_paths) %>%
  mutate(dist = map(n_paths, ~ tibble(x = 0:.x, p = dbinom(0:.x, .x, 0.5)))) %>%
  unnest(dist) %>%
  mutate(share_bin = bin_share(x / n_paths)) %>%
  group_by(share_bin) %>%
  summarise(n_users = sum(p), .groups = "drop") %>%
  mutate(type = "Expected if paths were joined at random")

share_dist <- bind_rows(observed_share, expected_share) %>%
  group_by(type) %>%
  mutate(pct = round(100 * n_users / sum(n_users), 1)) %>%
  ungroup() %>%
  complete(type, share_bin, fill = list(n_users = 0, pct = 0)) %>%
  mutate(type = factor(type, levels = c("Observed", "Expected if paths were joined at random")))

save_csv(share_dist, "4_share_awry_paths_distribution")

share_summary <- awry_users %>%
  summarise(
    n_users                  = n(),
    min_paths                = MIN_PATHS,
    mean_share_awry          = round(100 * mean(share_paths_awry), 1),
    sd_share_awry_observed   = round(100 * sd(share_paths_awry), 1),
    sd_share_awry_expected   = round(100 * sqrt(mean(0.25 / n_paths)), 1),
    pct_users_70plus_awry    = round(100 * mean(share_paths_awry >= 0.7), 1),
    pct_users_30minus_awry   = round(100 * mean(share_paths_awry <= 0.3), 1),
    pct_users_in_both_halves_of_a_pair = round(100 * mean(n_pairs < n_paths), 1)
  )

save_csv(share_summary, "4_share_awry_paths_summary")

share_by_group <- awry_users %>%
  group_by(activity_group) %>%
  summarise(
    n_users         = n(),
    mean_share_awry = round(100 * mean(share_paths_awry), 1),
    sd_share_awry   = round(100 * sd(share_paths_awry), 1),
    .groups = "drop"
  )

save_csv(share_by_group, "4_share_awry_paths_by_activity_group")

p_share <- share_dist %>%
  ggplot(aes(x = share_bin, y = pct, fill = type)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  scale_fill_manual(values = c("Observed" = col_neutral,
                               "Expected if paths were joined at random" = col_expected)) +
  scale_y_pct(max(share_dist$pct)) +
  labs(title = "Share of each user's paths that went awry",
       subtitle = paste0("Users in ", MIN_PATHS, "+ paths. Half of all paths are awry by design."),
       x = "% of the user's paths that went awry", y = "% of users",
       caption = paste0(cap, "Expected = binomial(n paths, 0.5) for each user, summed across users.")) +
  base_theme +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 30, hjust = 1))

save_fig(p_share, "4_share_awry_paths")


# === SECTION 5: REMOVAL RATE BY ACTIVITY GROUP ===

cat("\n=== SECTION 5: REMOVAL RATE BY ACTIVITY GROUP ===\n")

# Removal rate = removed comments / comments written. Because only the final comment of
# an awry path can be "removed" in this dataset, this rate also reflects how often a user
# writes the last comment in a path. Two versions:
#   pooled  = total removed / total comments in the group
#   per user = average of each user's own rate (each user counts once)
removal_by_group <- users %>%
  group_by(activity_group) %>%
  summarise(
    n_users                = n(),
    total_comments         = sum(n_comments),
    total_removed          = sum(n_removed_comments),
    pooled_rate_pct        = round(100 * total_removed / total_comments, 2),
    mean_user_rate_pct     = round(100 * mean(removal_rate), 2),
    pct_users_ever_removed = round(100 * mean(ever_removed), 1),
    .groups = "drop"
  )

save_csv(removal_by_group, "5_removal_rate_by_activity_group")

removal_panel <- function(var, title) {
  removal_by_group %>%
    ggplot(aes(x = activity_group, y = .data[[var]], fill = activity_group)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = sprintf("%.1f%%", .data[[var]])), vjust = -0.4, size = 3.2) +
    scale_fill_manual(values = col_groups) +
    scale_y_pct(max(removal_by_group[[var]])) +
    scale_x_discrete(labels = function(x) str_wrap(x, 14)) +
    labs(title = title, x = NULL, y = "%") +
    base_theme +
    theme(legend.position = "none", plot.title = element_text(size = 11))
}

p_removal_group <- (removal_panel("pct_users_ever_removed", "% of users with >=1 removed comment") |
                      removal_panel("pooled_rate_pct", "Removed comments as % of the group's comments")) +
  plot_annotation(
    title = "Removal by activity group",
    caption = paste0(cap, "Only the final comment of an awry path can be removed in this dataset."),
    theme = theme(plot.title = element_text(face = "bold", size = 14),
                  plot.caption = element_text(hjust = 0, colour = "#666666", size = 8))
  )

save_fig(p_removal_group, "5_removal_rate_by_activity_group", width = 11, height = 5.5)

# -- Removal groups (saved in the user table and used by script 5b) --
# Removal rate is only shown for users with MIN_COMMENTS_RATE+ comments: with fewer, one
# removal swings the rate too far (1 removed out of 2 comments = 50%).
# Three ways to pick out users who are removed a lot:
#   repeat_removal_author : REPEAT_MIN+ removed comments. A raw count, so it rises with activity.
#   top_removal_rate      : top TOP_RATE_PCT% by removal rate, among users with MIN_COMMENTS_RATE+
#                           comments and at least one removal. Ties at the cut-off are all included.
#   removed_above_chance  : removed share of path endings above 50% by z >= Z_THRESHOLD, among
#                           users with MIN_ENDINGS+ endings (the opportunity-adjusted label from 3b).
# Averages are pooled: all removed comments / all comments in the group.
avg_rate_all  <- sum(users$n_removed_comments) / sum(users$n_comments)
rate_users    <- users %>% filter(n_comments >= MIN_COMMENTS_RATE)
avg_rate_min  <- sum(rate_users$n_removed_comments) / sum(rate_users$n_comments)
rate_cut      <- quantile(rate_users$removal_rate, 1 - TOP_RATE_PCT / 100, names = FALSE)

# Chance check for the threshold: probability that a user with exactly MIN_COMMENTS_RATE comments,
# whose true rate equals the all-user average, shows double the average or more (binomial).
p_double_by_chance <- pbinom(ceiling(2 * avg_rate_all * MIN_COMMENTS_RATE) - 1, MIN_COMMENTS_RATE,
                             avg_rate_all, lower.tail = FALSE)
cat(sprintf("Removal rate threshold: %d+ comments (%s users). Chance an average user shows 2x the average rate: %.1f%%\n",
            MIN_COMMENTS_RATE, fmt(nrow(rate_users)), 100 * p_double_by_chance))

REMOVAL_COUNT_LEVELS <- c("None removed", paste0("1-", REPEAT_MIN - 1, " removed"),
                          paste0(REPEAT_MIN, "+ removed"))

# 5-point bins: with 25+ comments almost no user is above 25%
rate_bins   <- c(-Inf, 0, 0.05, 0.10, 0.15, 0.20, 0.25, 1)
rate_labels <- c("0%", "Up to 5%", "5-10%", "10-15%", "15-20%", "20-25%", "Over 25%")

users <- users %>%
  mutate(
    removal_count_group   = factor(case_when(
      n_removed_comments >= REPEAT_MIN ~ REMOVAL_COUNT_LEVELS[3],
      n_removed_comments >= 1          ~ REMOVAL_COUNT_LEVELS[2],
      TRUE                             ~ REMOVAL_COUNT_LEVELS[1]
    ), levels = REMOVAL_COUNT_LEVELS),
    repeat_removal_author = n_removed_comments >= REPEAT_MIN,
    removal_rate_eligible = n_comments >= MIN_COMMENTS_RATE,
    removal_rate_bin      = if_else(removal_rate_eligible,
                                    as.character(cut(removal_rate, breaks = rate_bins, labels = rate_labels)),
                                    NA_character_),
    removal_rate_pctile   = 100 * percent_rank(if_else(removal_rate_eligible, removal_rate, NA_real_)),
    top_removal_rate      = removal_rate_eligible & n_removed_comments > 0 & removal_rate >= rate_cut,
    endings_z             = if_else(n_endings > 0, (n_removed_comments - n_endings / 2) / sqrt(n_endings / 4),
                                    NA_real_),
    removed_above_chance  = n_endings >= MIN_ENDINGS & coalesce(endings_z, 0) >= Z_THRESHOLD
  )

rate_users <- users %>% filter(removal_rate_eligible)

col_removal_count <- setNames(c("#BDBDBD", "#F4A6A6", col_awry), REMOVAL_COUNT_LEVELS)
col_avg_all <- "#424242"
col_avg_min <- "#8C510A"

lab_avg_all <- sprintf("Average, all users: %.1f%%", 100 * avg_rate_all)
lab_avg_min <- sprintf("Average, users with %d+ comments: %.1f%%", MIN_COMMENTS_RATE, 100 * avg_rate_min)

rate_cap <- paste0(cap, "\nRemoval rate = removed comments / comments written. Users with ", MIN_COMMENTS_RATE,
                   "+ comments only. Only the final comment of an awry path can be removed in this dataset.")

# -- 5b. Removal rate per user: distribution in bins --
# Bins include their upper bound: 1 removed out of 25 comments (4%) falls in "Up to 5%".
rate_dist <- rate_users %>%
  mutate(removal_rate_bin = factor(removal_rate_bin, levels = rate_labels)) %>%
  group_by(removal_rate_bin, .drop = FALSE) %>%
  summarise(n_users = n(), n_removed = sum(n_removed_comments), .groups = "drop") %>%
  mutate(
    pct_users    = round(100 * n_users / sum(n_users), 1),
    pct_removals = round(100 * n_removed / sum(n_removed), 1)
  )

save_csv(rate_dist, "5b_removal_rate_distribution")
save_csv(uncapped_count(rate_users %>% mutate(removal_rate_pct = round(100 * removal_rate, 1)),
                        removal_rate_pct),
         "5b_removal_rate_distribution_uncapped")

# Places a rate on the binned x axis: bin k is centred on k, and the rate is placed within
# the bin in proportion to where it falls between the bin's edges.
rate_to_x <- function(r) {
  k <- findInterval(r, rate_bins, left.open = TRUE)   # bin index: (lower, upper]
  if (r <= 0) return(1)
  lo <- max(rate_bins[k], 0); hi <- rate_bins[k + 1]
  k - 0.5 + (r - lo) / (hi - lo)
}

avg_lines <- tibble(
  x      = c(rate_to_x(avg_rate_all), rate_to_x(avg_rate_min)),
  label  = c(lab_avg_all, lab_avg_min),
  colour = c(col_avg_all, col_avg_min),
  y      = c(95, 87)
)

# Two versions: users and removed comments side by side, and users only
plot_rate_dist <- function(show_removals) {
  d <- rate_dist %>%
    select(removal_rate_bin, `% of users` = pct_users, `% of removed comments` = pct_removals) %>%
    pivot_longer(-removal_rate_bin, names_to = "measure", values_to = "pct")
  if (!show_removals) d <- d %>% filter(measure == "% of users")
  
  d %>%
    ggplot(aes(x = removal_rate_bin, y = pct, fill = measure)) +
    geom_col(position = position_dodge(width = 0.85), width = if (show_removals) 0.8 else 0.6) +
    geom_text(aes(label = paste0(pct, "%")), position = position_dodge(width = 0.85),
              vjust = -0.4, size = 3) +
    geom_vline(xintercept = avg_lines$x, colour = avg_lines$colour, linetype = "dotted", linewidth = 0.8) +
    annotate("text", x = avg_lines$x, y = avg_lines$y, label = avg_lines$label, colour = avg_lines$colour,
             hjust = -0.05, size = 3.2) +
    scale_fill_manual(values = c("% of users" = col_neutral, "% of removed comments" = col_awry)) +
    scale_y_pct() +
    labs(title = "Removal rate per user",
         subtitle = paste0("Users with ", MIN_COMMENTS_RATE, "+ comments (n = ", comma(nrow(rate_users)), ")"),
         x = "Removed comments as % of the user's comments",
         y = if (show_removals) "%" else "% of users",
         caption = paste0(rate_cap, "\nBins include their upper bound. Dotted lines = average removal rate (all removed comments / all comments).")) +
    base_theme +
    theme(legend.position = if (show_removals) "top" else "none")
}

save_fig(plot_rate_dist(show_removals = TRUE),  "5b_removal_rate_distribution")
save_fig(plot_rate_dist(show_removals = FALSE), "5b_removal_rate_distribution_users_only")

# -- 5c. Removal rate vs comment volume --
# Same users as 5b. One point per user, slightly jittered. No user is labelled.
volume_breaks <- c(5, 10, 15, 20, 25, 30, 40, 50, 60, 80, 100, 150, 200, 300, 400, 500)
x_right <- max(rate_users$n_comments)

hlines <- tibble(
  y      = 100 * c(avg_rate_all, avg_rate_min, rate_cut),
  label  = c(lab_avg_all, lab_avg_min,
             sprintf("Top %d%% cut-off: %.1f%%", TOP_RATE_PCT, 100 * rate_cut)),
  colour = c(col_avg_all, col_avg_min, "#757575"),
  type   = c("dotted", "dotted", "dashed"),
  vjust  = c(1.4, -0.6, -0.6)
)

p_rate_volume <- rate_users %>%
  ggplot(aes(x = n_comments, y = 100 * removal_rate, colour = removal_count_group)) +
  geom_hline(data = hlines, aes(yintercept = y), colour = hlines$colour, linetype = hlines$type,
             linewidth = 0.7, inherit.aes = FALSE) +
  geom_text(data = hlines, aes(x = x_right, y = y, label = label), colour = hlines$colour,
            vjust = hlines$vjust, hjust = 1, size = 3, inherit.aes = FALSE) +
  geom_jitter(width = 0.02, height = 0.3, alpha = 0.5, size = 1.4) +
  scale_colour_manual(values = col_removal_count, drop = FALSE) +
  scale_x_log10(breaks = volume_breaks, labels = comma) +
  scale_y_continuous(limits = c(-2, NA), labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Removal rate vs comment volume",
       subtitle = paste0("One point per user with ", MIN_COMMENTS_RATE, "+ comments (n = ", comma(nrow(rate_users)),
                         "), the same users as the binned chart"),
       x = "Comments written (log scale)", y = "Removal rate",
       caption = paste0(rate_cap, "\nPoints are jittered slightly so overlapping users stay visible. ",
                        "The x axis is logarithmic: equal distances = equal ratios.")) +
  base_theme +
  theme(legend.position = "top") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2.5)))

save_fig(p_rate_volume, "5c_removal_rate_vs_volume", width = 9, height = 6)

# -- 5d. High-removal user groups compared --
# Candidate groups for later comparisons (e.g. script 5b, topics). Overlap columns show how
# far the definitions pick out the same users.
n_removed_all <- sum(users$n_removed_comments)

removal_group_def <- setNames(
  list(rep(TRUE, nrow(users)),
       users$removal_rate_eligible,
       users$removal_count_group == REMOVAL_COUNT_LEVELS[1],
       users$removal_count_group == REMOVAL_COUNT_LEVELS[2],
       users$repeat_removal_author,
       users$top_removal_rate,
       users$removed_above_chance),
  c("All identifiable users",
    paste0("Users with ", MIN_COMMENTS_RATE, "+ comments"),
    REMOVAL_COUNT_LEVELS[1],
    REMOVAL_COUNT_LEVELS[2],
    paste0(REMOVAL_COUNT_LEVELS[3], " (repeat removal authors)"),
    paste0("Top ", TOP_RATE_PCT, "% by removal rate (", MIN_COMMENTS_RATE, "+ comments)"),
    paste0("Removed above chance (share of endings, z >= ", Z_THRESHOLD, ", ", MIN_ENDINGS, "+ endings)"))
)

removal_groups_compared <- imap_dfr(removal_group_def, function(keep, g) {
  d <- users[keep, ]
  tibble(
    group                     = g,
    n_users                   = nrow(d),
    pct_of_users              = round(100 * nrow(d) / nrow(users), 1),
    median_comments           = median(d$n_comments),
    mean_comments             = round(mean(d$n_comments), 1),
    median_endings            = median(d$n_endings),
    avg_removal_rate_pct      = round(100 * sum(d$n_removed_comments) / sum(d$n_comments), 1),
    removed_share_endings_pct = round(100 * sum(d$n_removed_comments) / sum(d$n_endings), 1),
    pct_of_removed_comments   = round(100 * sum(d$n_removed_comments) / n_removed_all, 1),
    pct_also_repeat_author    = round(100 * mean(d$repeat_removal_author), 1),
    pct_also_top_rate         = round(100 * mean(d$top_removal_rate), 1),
    pct_also_above_chance     = round(100 * mean(d$removed_above_chance), 1)
  )
})

save_csv(removal_groups_compared, "5d_high_removal_groups_compared")

# === SECTION 6: DELTA FLAIR ===

cat("\n=== SECTION 6: DELTA FLAIR ===\n")

# Delta flair is the user's career delta count as displayed when each comment was collected
# (it includes deltas earned outside this dataset). A user's value here is the highest
# count seen across their comments, i.e. their cumulative count at the latest point
# recorded in the data (comments run to Dec 2022). "None" = no flair shown, or a flair
# without a number.
flair_bins   <- c(-Inf, 0, 1, 2, 5, 10, 25, 50, 100, Inf)
flair_labels <- c("None", "1", "2", "3-5", "6-10", "11-25", "26-50", "51-100", "100+")

users <- users %>%
  mutate(flair_bin = cut(coalesce(max_delta_flair, 0), breaks = flair_bins, labels = flair_labels))

# -- 6a. Overall distribution --
flair_summary <- tibble(
  Metric = c("% of users with delta flair", "Median flair (users with flair)",
             "Mean flair (users with flair)", "Max flair",
             "% of flaired users whose flair changes across their comments",
             "% of comments carrying delta flair"),
  Value = c(
    sprintf("%.1f%%", 100 * mean(users$has_delta_flair)),
    fmt(median(users$max_delta_flair, na.rm = TRUE)),
    fmt(mean(users$max_delta_flair, na.rm = TRUE)),
    fmt(max(users$max_delta_flair, na.rm = TRUE)),
    sprintf("%.1f%%", 100 * mean(users$n_flair_values[users$has_delta_flair] > 1)),
    sprintf("%.1f%%", 100 * mean(!is.na(ce_known$author_flair_text)))
  )
)

save_csv(flair_summary, "6a_delta_flair_summary")

flair_dist <- users %>%
  count(flair_bin, name = "n_users") %>%
  mutate(pct = round(100 * n_users / sum(n_users), 1))

save_csv(flair_dist, "6a_delta_flair_distribution")
save_csv(uncapped_count(users %>% filter(!is.na(max_delta_flair)), max_delta_flair),
         "6a_delta_flair_uncapped")

p_flair <- flair_dist %>%
  ggplot(aes(x = flair_bin, y = pct)) +
  geom_col(fill = col_neutral, width = 0.75) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.4, size = 3.2) +
  scale_y_pct(max(flair_dist$pct)) +
  labs(title = "Delta flair (career deltas) per user",
       x = "Career deltas (cumulative, latest recorded)", y = "% of users",
       caption = paste0(cap, "Cumulative career deltas at the latest point recorded (up to Dec 2022), ",
                        "earned anywhere on r/ChangeMyView, not only in these paths.")) +
  base_theme

save_fig(p_flair, "6a_delta_flair_distribution")

# Comments vs deltas per user, on the same bins (combines 1b and 6a; no new analysis)
comments_vs_deltas <- bind_rows(
  users %>%
    mutate(bin = cut(n_comments, breaks = flair_bins, labels = flair_labels)) %>%
    count(bin, name = "n_users") %>%
    mutate(measure = "Comments written (this dataset)"),
  users %>%
    count(bin = flair_bin, name = "n_users") %>%
    mutate(measure = "Career deltas (flair)")
) %>%
  complete(measure, bin, fill = list(n_users = 0)) %>%
  group_by(measure) %>%
  mutate(pct = round(100 * n_users / sum(n_users), 1)) %>%
  ungroup() %>%
  mutate(measure = factor(measure, levels = c("Comments written (this dataset)", "Career deltas (flair)")))

save_csv(comments_vs_deltas, "6a_comments_vs_deltas_per_user")

p_comments_deltas <- comments_vs_deltas %>%
  ggplot(aes(x = bin, y = pct, fill = measure)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  geom_text(aes(label = paste0(pct, "%")), position = position_dodge(width = 0.85),
            vjust = -0.4, size = 2.8) +
  scale_fill_manual(values = c("Comments written (this dataset)" = col_neutral,
                               "Career deltas (flair)" = "#B8860B")) +
  scale_y_pct(max(comments_vs_deltas$pct)) +
  labs(title = "Comments written vs career deltas, per user",
       x = "Count per user", y = "% of users",
       caption = paste0(cap, "Comments = comments in this dataset.\nDeltas = cumulative career count at the ",
                        "latest point recorded (up to Dec 2022). Every user has >=1 comment, so 'None' applies to deltas only.")) +
  base_theme +
  theme(legend.position = "top")

save_fig(p_comments_deltas, "6a_comments_vs_deltas_per_user", width = 10, height = 5.5)

# -- 6b. By activity group --
flair_by_group <- users %>%
  group_by(activity_group) %>%
  summarise(
    n_users             = n(),
    pct_with_flair      = round(100 * mean(has_delta_flair), 1),
    median_flair_if_any = median(max_delta_flair, na.rm = TRUE),
    mean_flair_if_any   = round(mean(max_delta_flair, na.rm = TRUE), 1),
    .groups = "drop"
  )

save_csv(flair_by_group, "6b_delta_flair_by_activity_group")

flair_dist_by_group <- users %>%
  count(activity_group, flair_bin, name = "n_users") %>%
  group_by(activity_group) %>%
  mutate(pct = round(100 * n_users / sum(n_users), 1)) %>%
  ungroup()

save_csv(flair_dist_by_group, "6b_delta_flair_distribution_by_activity_group")

p_flair_group <- flair_dist_by_group %>%
  ggplot(aes(x = flair_bin, y = pct, fill = activity_group)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  scale_fill_manual(values = col_groups) +
  scale_y_pct(max(flair_dist_by_group$pct)) +
  labs(title = "Delta flair by activity group",
       x = "Career deltas (cumulative, latest recorded)", y = "% of users (within group)",
       caption = cap) +
  base_theme +
  theme(legend.position = "top")

save_fig(p_flair_group, "6b_delta_flair_by_activity_group", width = 10, height = 5.5)

# -- 6c. Removed-comment authors vs everyone else --
# Removed-comment authors are more active on average, and more active users are more
# likely to carry flair - so the comparison is also shown within activity groups.
users <- users %>%
  mutate(removal_status = factor(if_else(ever_removed, "Wrote >=1 removed comment",
                                         "No removed comments"),
                                 levels = c("Wrote >=1 removed comment", "No removed comments")))

flair_by_removal <- users %>%
  group_by(removal_status) %>%
  summarise(
    n_users             = n(),
    median_comments     = median(n_comments),
    pct_with_flair      = round(100 * mean(has_delta_flair), 1),
    median_flair_if_any = median(max_delta_flair, na.rm = TRUE),
    .groups = "drop"
  )

save_csv(flair_by_removal, "6c_delta_flair_by_removal_status")

flair_by_removal_and_group <- users %>%
  group_by(activity_group, removal_status) %>%
  summarise(
    n_users             = n(),
    pct_with_flair      = round(100 * mean(has_delta_flair), 1),
    median_flair_if_any = median(max_delta_flair, na.rm = TRUE),
    .groups = "drop"
  )

save_csv(flair_by_removal_and_group, "6c_delta_flair_by_removal_status_and_activity_group")

flair_dist_by_removal <- users %>%
  count(removal_status, flair_bin, name = "n_users") %>%
  group_by(removal_status) %>%
  mutate(pct = round(100 * n_users / sum(n_users), 1)) %>%
  ungroup()

save_csv(flair_dist_by_removal, "6c_delta_flair_distribution_by_removal_status")

p_flair_removal <- flair_dist_by_removal %>%
  ggplot(aes(x = flair_bin, y = pct, fill = removal_status)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  scale_fill_manual(values = c("Wrote >=1 removed comment" = col_awry,
                               "No removed comments" = col_neutral)) +
  scale_y_pct(max(flair_dist_by_removal$pct)) +
  labs(title = "Delta flair: removed-comment authors vs everyone else",
       x = "Career deltas (cumulative, latest recorded)", y = "% of users (within group)",
       caption = paste0(cap, "See the table by activity group: removed-comment authors are more active on average.")) +
  base_theme +
  theme(legend.position = "top")

save_fig(p_flair_removal, "6c_delta_flair_by_removal_status")


# === SECTION 7: [deleted] ACCOUNTS ===

cat("\n=== SECTION 7: [deleted] ACCOUNTS ===\n")

# [deleted] marks comments whose account was deleted or banned before collection. It
# stands in for an UNKNOWN number of accounts, so per-user figures are not possible.
# Because later comments were collected later (more time for accounts to disappear),
# its share depends on collection timing as well as on behaviour.
ce_deleted <- ce %>%
  mutate(account = if_else(speaker == "[deleted]", "[deleted]", "Identifiable users"))

deleted_summary <- ce_deleted %>%
  group_by(account) %>%
  summarise(
    n_comments               = n(),
    n_removed                = sum(is_removed_comment),
    n_first_replies          = sum(thread_depth == 0),
    n_paths_present          = n_distinct(conversation_id),
    removal_rate_pct         = round(100 * mean(is_removed_comment), 2),
    pct_comments_in_awry     = round(100 * mean(has_removed_comment), 1),
    median_word_count        = median(word_count, na.rm = TRUE),
    median_age_at_retrieval  = round(median(age_at_retrieval_days, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  mutate(
    pct_of_all_comments      = round(100 * n_comments / sum(n_comments), 2),
    pct_of_all_removed       = round(100 * n_removed / sum(n_removed), 2),
    pct_of_all_first_replies = round(100 * n_first_replies / sum(n_first_replies), 2)
  )

save_csv(deleted_summary, "7a_deleted_vs_identifiable")

deleted_by_year <- ce_deleted %>%
  group_by(year) %>%
  summarise(
    n_comments          = n(),
    pct_deleted         = round(100 * mean(account == "[deleted]"), 2),
    median_age_days     = round(median(age_at_retrieval_days, na.rm = TRUE), 1),
    .groups = "drop"
  )

save_csv(deleted_by_year, "7b_deleted_share_by_year")

deleted_panel <- function(var, title) {
  deleted_summary %>%
    ggplot(aes(x = account, y = .data[[var]], fill = account)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = sprintf("%.1f%%", .data[[var]])), vjust = -0.4, size = 3.4) +
    scale_fill_manual(values = c("[deleted]" = col_deleted, "Identifiable users" = col_neutral)) +
    scale_y_pct(max(deleted_summary[[var]])) +
    labs(title = title, x = NULL, y = "%") +
    base_theme +
    theme(legend.position = "none", plot.title = element_text(size = 11))
}

p_deleted <- (deleted_panel("pct_comments_in_awry", "% of comments in awry paths") |
                deleted_panel("removal_rate_pct", "% of comments removed")) +
  plot_annotation(
    title = "[deleted] accounts vs identifiable users",
    caption = paste0("Sample: ", SAMPLE_LABEL, ". [deleted] covers an unknown number of accounts ",
                     "deleted or banned before collection."),
    theme = theme(plot.title = element_text(face = "bold", size = 14),
                  plot.caption = element_text(hjust = 0, colour = "#666666", size = 8))
  )

save_fig(p_deleted, "7_deleted_vs_identifiable", width = 9, height = 5)

# === SECTION 8: SAVE USER TABLE ===

# Shareable version (user IDs)
shared_file <- file.path(SHARED_USER_DIR, paste0("user_table_", SAMPLE_TAG, ".csv"))
write_csv(users, shared_file)
cat(sprintf("\n  -> user table (IDs) saved: %s (%s users, %d columns)\n",
            shared_file, fmt(nrow(users)), ncol(users)))

# Local version (usernames), joined from the private key. Skipped if the key is absent
# (e.g. when the analysis is run from a GitHub clone without the private data).
if (file.exists(KEY_FILE)) {
  key <- read_csv(KEY_FILE, col_types = cols(.default = col_character()), na = "")
  users_identified <- users %>%
    left_join(key %>% select(user_id, username = speaker), by = c("speaker" = "user_id")) %>%
    mutate(speaker = coalesce(username, speaker)) %>%
    select(-username)
  private_file <- file.path(PRIVATE_USER_DIR, paste0("user_table_", SAMPLE_TAG, ".csv"))
  write_csv(users_identified, private_file)
  cat(sprintf("  -> user table (usernames) saved: %s\n", private_file))
} else {
  cat("  Private key not found - username version of the user table not written.\n")
}

cat("\nDone: user descriptives saved to", DESC, "and", FIGS, "\n")