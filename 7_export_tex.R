# 7. export_tex.R for Conversations Gone Awry (CMV) - Political Comments Analysis
#
# Turns the results of scripts 1-6 into LaTeX inputs for Overleaf write-ups.
# It does not compute new results: it reads the CSVs in descriptive_results/
# (plus conversations.rds for post-ID coverage by year, which 1_data_prep.R
# prints but does not save) and writes:
#
#   tex_outputs/numbers_descriptives.tex   \newcommand macros for numbers cited in text
#   tex_outputs/tables/*.tex               booktabs tables (tabular only)
#
# In the Overleaf document:
#   \input{tex_outputs/numbers_descriptives}         % in the preamble
#   ... a removal rate of \removalRateAll\% ...      % in the text
#   \input{tex_outputs/tables/tab_sample_comparison} % inside a table float
#
# Then run sync_overleaf.R to push tex_outputs/ and the figures to Overleaf.
#
# Sections:
#   1. Corpus and analysis sample
#   2. Pair structure and thread dynamics
#   3. Content of final comments
#   4. Repeat encounters
#   5. Users and removals
#   6. Topics
#   7. Tables
#
# Adding a number: call tex_macro() in the matching section (letters-only name),
# re-run, sync. Adding a table: build a data frame of formatted cells and call
# tex_table(). Helpers live in tex_helpers.R.
#
# Run after scripts 1-6 (needs their CSVs and the filled topic coding sheet).


# === SECTION 0: PREP ===

# -- 0. Packages --
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tidyr)

source("tex_helpers.R")
SCRIPT <- "7_export_tex.R"

# -- 1. Paths and readers --
DESC    <- "descriptive_results"
PREPPED <- "prepped_data_anonymized"

rd <- function(sub, file) {
  read_csv(file.path(DESC, sub, file), show_col_types = FALSE,
           col_types = cols(.default = col_character()))
}
src <- function(sub, file) file.path(sub, file)

# Pull one value from a two-column Metric/Value table.
metric <- function(df, name, value_col = 2) {
  v <- df[[value_col]][df[[1]] == name]
  if (length(v) != 1) stop(sprintf("Metric '%s' not found (or not unique).", name))
  parse_csv_num(v)
}

# Pull one cell by matching key columns.
cell <- function(df, col, ...) {
  keys <- list(...)
  keep <- rep(TRUE, nrow(df))
  for (k in names(keys)) keep <- keep & df[[k]] == keys[[k]]
  v <- df[[col]][keep]
  if (length(v) != 1) stop(sprintf("Cell %s [%s] not found (or not unique).", col,
                                   paste(names(keys), keys, sep = "=", collapse = ", ")))
  parse_csv_num(v)
}

tex_macros_start()


# === SECTION 1: CORPUS AND ANALYSIS SAMPLE ===

tex_section("1. Corpus and analysis sample")
samp <- rd("data_checks", "0b_sample_comparison.csv")
S <- src("data_checks", "0b_sample_comparison.csv")
sm <- function(metric_name, col) metric(samp, metric_name, value_col = col)

tex_macro("nCommentsFull", tex_num(sm("Comments", "Full corpus")),                    "comments, full corpus", S)
tex_macro("nPathsFull",    tex_num(sm("Conversations (paths)", "Full corpus")),       "paths, full corpus")
tex_macro("nPairsFull",    tex_num(sm("Pairs", "Full corpus")),                       "pairs, full corpus")
tex_macro("nUsersFull",    tex_num(sm("Unique users (excl. [deleted])", "Full corpus")), "identifiable users, full corpus")
tex_macro("nComments",     tex_num(sm("Comments", "Has post ID")),                    "comments, post-ID sample")
tex_macro("nPaths",        tex_num(sm("Conversations (paths)", "Has post ID")),       "paths, post-ID sample")
tex_macro("nPairs",        tex_num(sm("Pairs", "Has post ID")),                       "pairs, post-ID sample")
tex_macro("nThreads",      tex_num(sm("Posts (threads)", "Has post ID")),             "threads, post-ID sample")
tex_macro("nUsers",        tex_num(sm("Unique users (excl. [deleted])", "Has post ID")), "identifiable users, post-ID sample")
tex_macro("pctPairsInSample",
          tex_num(100 * sm("Pairs", "Has post ID") / sm("Pairs", "Full corpus")),
          "% of corpus pairs in post-ID sample")

thr <- rd("conversations", "3_thread_summary_postid.csv")
S <- src("conversations", "3_thread_summary_postid.csv")
tex_macro("pairsPerThread",     tex_num(metric(thr, "Mean pairs per thread"), 2), "mean pairs per thread", S)
tex_macro("pctThreadsOnePair",  tex_num(100 - metric(thr, "% of threads with >1 pair")), "% threads contributing one pair")
tex_macro("maxPairsPerThread",  tex_num(metric(thr, "Max pairs in one thread")),  "max pairs from one thread")

# Post-ID coverage by year (not saved by 1_data_prep.R, so computed here)
conv <- readRDS(file.path(PREPPED, "conversations.rds"))
by_year <- conv %>%
  group_by(conv_year) %>%
  summarise(n = n(), pct_post_id = 100 * mean(has_post_id), .groups = "drop") %>%
  arrange(conv_year)
tex_macro("pctPostIdSeventeen",
          tex_num(by_year$pct_post_id[by_year$conv_year == 2017], 1),
          "% of 2017 conversations with a post ID", "prepped_data_anonymized/conversations.rds")


# === SECTION 2: PAIR STRUCTURE AND THREAD DYNAMICS ===

tex_section("2. Pair structure and thread dynamics")
within_k <- rd("conversations", "4c_pair_length_within_k_postid.csv")
S <- src("conversations", "4c_pair_length_within_k_postid.csv")
wk <- function(k) cell(within_k, "pct_within", k = as.character(k))
tex_macro("pctSameLength",  tex_num(wk(0)), "% pairs with same path length", S)
tex_macro("pctWithinOne",   tex_num(wk(1)), "% pairs within 1 comment")
tex_macro("pctWithinTwo",   tex_num(wk(2)), "% pairs within 2 comments")
tex_macro("pctWithinFour",  tex_num(wk(4)), "% pairs within 4 comments")

len_unc <- rd("conversations", "2b_path_length_uncapped_postid.csv") %>%
  mutate(across(everything(), parse_csv_num))
S <- src("conversations", "2b_path_length_uncapped_postid.csv")
tex_macro("pctOntrackOverFifteen",
          tex_num(sum(len_unc$pct_ontrack[len_unc$n_comments > 15]), 1),
          "% on-track paths longer than 15 comments", S)
tex_macro("maxAwryLength",    tex_num(max(len_unc$n_comments[len_unc$n_awry > 0])),    "longest awry path")
tex_macro("maxOntrackLength", tex_num(max(len_unc$n_comments[len_unc$n_ontrack > 0])), "longest on-track path")

len_sum <- rd("conversations", "2b_path_length_summary_postid.csv")
S <- src("conversations", "2b_path_length_summary_postid.csv")
tex_macro("meanLenAwry",    tex_num(cell(len_sum, "mean", outcome = "Awry"), 1),     "mean path length, awry", S)
tex_macro("meanLenOntrack", tex_num(cell(len_sum, "mean", outcome = "On-track"), 1), "mean path length, on-track")

spk <- rd("conversations", "6a_participation_summary_postid.csv")
S <- src("conversations", "6a_participation_summary_postid.csv")
tex_macro("meanSpeakersAwry",    tex_num(cell(spk, "mean_speakers", outcome = "Awry"), 2),     "mean speakers per path, awry", S)
tex_macro("meanSpeakersOntrack", tex_num(cell(spk, "mean_speakers", outcome = "On-track"), 2), "mean speakers per path, on-track")

spk_bins <- rd("conversations", "6a_speakers_per_path_postid.csv")
S <- src("conversations", "6a_speakers_per_path_postid.csv")
pct_spk <- function(bin) mean(parse_csv_num(spk_bins$pct[spk_bins$speaker_bin == bin]))
tex_macro("pctTwoSpeakers",   tex_num(pct_spk("2")), "% paths with exactly 2 speakers (mean of awry and on-track)", S)
tex_macro("pctThreeSpeakers", tex_num(pct_spk("3")), "% paths with exactly 3 speakers (mean of awry and on-track)")

chal <- rd("conversations", "6b_challenger_summary_postid.csv")
S <- src("conversations", "6b_challenger_summary_postid.csv")
tex_macro("pctChallengerReturnsAwry",    tex_num(cell(chal, "pct_challenger_returns", outcome = "Awry")),     "% paths where challenger returns, awry", S)
tex_macro("pctChallengerReturnsOntrack", tex_num(cell(chal, "pct_challenger_returns", outcome = "On-track")), "% paths where challenger returns, on-track")

gap <- rd("conversations", "5a_pair_start_gap_summary_postid.csv")
S <- src("conversations", "5a_pair_start_gap_summary_postid.csv")
tex_macro("pctAwryStartedFirst", tex_num(parse_csv_num(gap$pct_awry_started_first), 1), "% pairs where awry path started first", S)
tex_macro("medianStartGapHours", tex_num(parse_csv_num(gap$median_abs_gap_hours), 2),   "median |start gap| in hours")
tex_macro("pctStartWithinDay",   tex_num(parse_csv_num(gap$pct_within_1_day), 1),       "% pairs starting within a day")
tex_macro("pctStartOverDay",     tex_num(100 - parse_csv_num(gap$pct_within_1_day), 1), "% pairs starting more than a day apart")

gap_d <- rd("conversations", "5a_pair_start_gap_detailed_postid.csv")
S <- src("conversations", "5a_pair_start_gap_detailed_postid.csv")
tex_macro("pctStartWithinThreeHours", tex_num(cell(gap_d, "cum_pct", gap_fine = "1-3 hrs")),
          "% pairs starting within three hours", S)

wp <- rd("topics", "5b_4_within_pair_differences_by_political_group.csv")
wp_all <- wp %>% filter(group == "All pairs")
tex_macro("nPairsWithinTable", tex_num(cell(wp_all, "n_pairs", measure = "Path length (comments)")),
          "pairs in the within-pair difference table (threads with a clustered title)",
          src("topics", "5b_4_within_pair_differences_by_political_group.csv"))

role <- rd("conversations", "7a_final_comment_author_role_postid.csv")
S <- src("conversations", "7a_final_comment_author_role_postid.csv")
tex_macro("pctFinalByChallengerAwry",    tex_num(cell(role, "pct", outcome = "Awry", author_role = "Challenger")),     "% final comments by challenger, awry", S)
tex_macro("pctFinalByChallengerOntrack", tex_num(cell(role, "pct", outcome = "On-track", author_role = "Challenger")), "% final comments by challenger, on-track")
tex_macro("pctFinalByNewcomerAwry",      tex_num(cell(role, "pct", outcome = "Awry", author_role = "New to path")),     "% final comments by newcomer to path, awry")
tex_macro("pctFinalByNewcomerOntrack",   tex_num(cell(role, "pct", outcome = "On-track", author_role = "New to path")), "% final comments by newcomer to path, on-track")

tgt <- rd("conversations", "7b_final_comment_reply_target_postid.csv")
S <- src("conversations", "7b_final_comment_reply_target_postid.csv")
tex_macro("pctReplyOtherAwry",    tex_num(cell(tgt, "pct", outcome = "Awry", reply_target = "Replies to another participant")),     "% final comments replying to non-challenger, awry", S)
tex_macro("pctReplyOtherOntrack", tex_num(cell(tgt, "pct", outcome = "On-track", reply_target = "Replies to another participant")), "% final comments replying to non-challenger, on-track")


# === SECTION 3: CONTENT OF FINAL COMMENTS ===

tex_section("3. Content of final comments")
wl <- rd("content", "2c_final_comment_length_within_pair_postid.csv")
S <- src("content", "2c_final_comment_length_within_pair_postid.csv")
tex_macro("ownWordsDiff",       tex_num(cell(wl, "mean_diff_awry_minus_ontrack", measure = "Own words (quotes removed)")), "mean extra own words, removed vs on-track final comment", S)
tex_macro("ownWordsDiffMedian", tex_num(cell(wl, "median_diff", measure = "Own words (quotes removed)")),                  "median difference in own words")

q <- rd("content", "3a_pct_quoting_by_outcome_position_postid.csv")
S <- src("content", "3a_pct_quoting_by_outcome_position_postid.csv")
q_awry <- cell(q, "pct", outcome = "Awry", position = "Final comment")
q_on   <- cell(q, "pct", outcome = "On-track", position = "Final comment")
tex_macro("pctQuoteRemoved", tex_num(q_awry), "% removed comments that quote", S)
tex_macro("pctQuoteOntrack", tex_num(q_on),   "% on-track final comments that quote")
tex_macro("quoteDiffPP",     tex_num(q_awry - q_on, 1), "difference in percentage points")


# === SECTION 4: REPEAT ENCOUNTERS ===

tex_section("4. Repeat encounters")
dy <- rd("networks", "1b_dyads_by_threads_together_uncapped.csv")
S <- src("networks", "1b_dyads_by_threads_together_uncapped.csv")
tex_macro("pctDyadsOneThread",  tex_num(cell(dy, "pct", n_threads = "1"), 1), "% user pairs who exchange replies in one thread only", S)
tex_macro("nDyadsMultiThread",  tex_num(sum(parse_csv_num(dy$n_dyads[dy$n_threads != "1"]))), "user pairs who exchange replies in 2+ threads")

enc <- rd("networks", "1c_repeat_encounters_by_role.csv")
S <- src("networks", "1c_repeat_encounters_by_role.csv")
tex_macro("pctExchangedRemoved", tex_num(cell(enc, "pct_exchanged_earlier", role = "Removed comment")),        "% removed comments replying to someone already exchanged with on the path", S)
tex_macro("pctExchangedOntrack", tex_num(cell(enc, "pct_exchanged_earlier", role = "On-track final comment")), "% on-track final comments replying to someone already exchanged with")


# === SECTION 5: USERS AND REMOVALS ===

tex_section("5. Users and removals")
act <- rd("users", "1a_activity_volume_summary_postid.csv")
S <- src("users", "1a_activity_volume_summary_postid.csv")
tex_macro("pctCommentsTopTen", tex_num(cell(act, "Pct_of_all_comments", Group = "Top 10%")), "% comments by top 10% of users", S)
tex_macro("pctCommentsTopOne", tex_num(cell(act, "Pct_of_all_comments", Group = "Top 1%")),  "% comments by top 1% of users")

rr <- rd("users", "5_removal_rate_by_activity_group_postid.csv")
S <- src("users", "5_removal_rate_by_activity_group_postid.csv")
rates <- parse_csv_num(rr$pooled_rate_pct)
tex_macro("removalRateGroupMin", tex_num(min(rates)), "lowest pooled removal rate across activity groups", S)
tex_macro("removalRateGroupMax", tex_num(max(rates)), "highest pooled removal rate across activity groups")

conc <- rd("users", "3a_removal_concentration_summary_postid.csv")
S <- src("users", "3a_removal_concentration_summary_postid.csv")
n_removal_authors <- metric(conc, "Distinct authors of removed comments")
tex_macro("nRemovalAuthors", tex_num(n_removal_authors), "distinct authors of removed comments", S)

grp <- rd("users", "5d_high_removal_groups_compared_postid.csv")
S <- src("users", "5d_high_removal_groups_compared_postid.csv")
g3 <- "3+ removed (repeat removal authors)"
tex_macro("nRepeatRemovalAuthors",   tex_num(cell(grp, "n_users", group = g3)), "users with 3+ removed comments", S)
tex_macro("pctAuthorsThreePlus",     tex_num(100 * cell(grp, "n_users", group = g3) / n_removal_authors),
          "% of removed-comment authors with 3+ removals")
tex_macro("pctRemovalsThreePlus",    tex_num(cell(grp, "pct_of_removed_comments", group = g3)), "% of removals by users with 3+ removals")
tex_macro("nUsersTwentyFive",        tex_num(cell(grp, "n_users", group = "Users with 25+ comments")), "users with 25+ comments")
tex_macro("removalRateTwentyFive",   tex_num(cell(grp, "avg_removal_rate_pct", group = "Users with 25+ comments"), 1), "mean removal rate, users with 25+ comments")
tex_macro("removalRateAll",          tex_num(cell(grp, "avg_removal_rate_pct", group = "All identifiable users"), 1),  "mean removal rate, all identifiable users")

dist <- rd("users", "5b_removal_rate_distribution_postid.csv")
S <- src("users", "5b_removal_rate_distribution_postid.csv")
over15 <- c("15-20%", "20-25%", "Over 25%")
tex_macro("pctNoRemovalsTwentyFive", tex_num(cell(dist, "pct_users", removal_rate_bin = "0%")), "% of 25+ users with no removals", S)
tex_macro("pctOverFifteenTwentyFive",
          tex_num(sum(parse_csv_num(dist$pct_users[dist$removal_rate_bin %in% over15]))),
          "% of 25+ users with removal rate over 15%")

del <- rd("users", "7a_deleted_vs_identifiable_postid.csv")
S <- src("users", "7a_deleted_vs_identifiable_postid.csv")
tex_macro("removalRateDeleted",       tex_num(cell(del, "removal_rate_pct", account = "[deleted]"), 1),               "removal rate, [deleted]", S)
tex_macro("removalRateIdentifiable",  tex_num(cell(del, "removal_rate_pct", account = "Identifiable users"), 1),      "removal rate, identifiable users")
tex_macro("pctAwryDeleted",           tex_num(cell(del, "pct_comments_in_awry", account = "[deleted]")),              "% of [deleted] comments in awry paths")
tex_macro("pctAwryIdentifiable",      tex_num(cell(del, "pct_comments_in_awry", account = "Identifiable users")),     "% of identifiable users' comments in awry paths")

ug <- rd("topics", "5b_5_user_groups.csv")
S <- src("topics", "5b_5_user_groups.csv")
tex_macro("nHighRateUsers",   tex_num(cell(ug, "n_users", group = "Over 10% removed")), "25+ users with removal rate over 10%", S)
tex_macro("pctHighRateUsers", tex_num(cell(ug, "pct_of_comparison_users", group = "Over 10% removed")), "% of 25+ users over 10%")


# === SECTION 6: TOPICS ===

tex_section("6. Topics")
sizes <- rd("topics", "5b_1_topic_sizes.csv")
S <- src("topics", "5b_1_topic_sizes.csv")
n_titled <- sum(parse_csv_num(sizes$n_threads))
tex_macro("nTopics",           tex_num(nrow(sizes)), "number of topic clusters (k)", S)
tex_macro("nThreadsClustered", tex_num(n_titled),    "threads with a usable title")
tex_macro("nExplicitTopics",   tex_num(sum(sizes$political_group == "Explicitly political")), "explicitly political clusters")

untitled <- rd("topics", "topic_threads_without_title.csv")
tex_macro("nThreadsNoTitle", tex_num(sum(parse_csv_num(untitled$n_threads))),
          "threads excluded (title replaced on deletion)", src("topics", "topic_threads_without_title.csv"))

pg <- rd("topics", "5b_4_topic_sizes_by_political_group.csv")
S <- src("topics", "5b_4_topic_sizes_by_political_group.csv")
tex_macro("pctThreadsExplicit", tex_num(cell(pg, "pct_threads", political_group = "Explicitly political")), "% threads in explicitly political clusters", S)
tex_macro("pctPairsExplicit",   tex_num(cell(pg, "pct_pairs", political_group = "Explicitly political")),   "% pairs in explicitly political clusters")

sweep <- rd("topics", "topic_k_sweep.csv") %>% mutate(across(everything(), parse_csv_num))
S <- src("topics", "topic_k_sweep.csv")
k_main <- nrow(sizes)
tex_macro("silhouetteK",   tex_num(sweep$silhouette[sweep$k == k_main], 2),         "silhouette at chosen k", S)
tex_macro("silhouetteMin", tex_num(min(sweep$silhouette), 2),                       "lowest silhouette in sweep")
tex_macro("silhouetteMax", tex_num(max(sweep$silhouette), 2),                       "highest silhouette in sweep")
tex_macro("ariK",          tex_num(sweep$stability_mean_ari[sweep$k == k_main], 2), "mean ARI across seeds at chosen k")
tex_macro("kSweepMin",     tex_num(min(sweep$k)), "smallest k in sweep")
tex_macro("kSweepMax",     tex_num(max(sweep$k)), "largest k in sweep")

tex_macros_write("numbers_descriptives.tex", script = SCRIPT)


# === SECTION 7: TABLES ===

# -- 7a. Post-ID coverage by year --
tex_table(
  by_year %>% transmute(Year = as.character(conv_year),
                        Conversations = tex_num(n),
                        `With post ID (%)` = tex_num(pct_post_id, 1)),
  "tab_post_id_by_year", align = "lrr",
  header = c("Year", "Conversations", "With post ID (\\%)"), raw_header = TRUE,
  script = SCRIPT
)

# -- 7b. Sample comparison (rows as in 1_data_prep.R) --
keep_rows <- c(
  "Date range"                           = "Date range",
  "Comments"                             = "Comments",
  "Conversations (paths)"                = "Paths",
  "Pairs"                                = "Pairs",
  "Posts (threads)"                      = "Threads",
  "Unique users (excl. [deleted])"       = "Unique users (excl. [deleted])",
  "Mean comments per path"               = "Mean comments per path",
  "Mean comments per path - awry"        = "\\quad awry",
  "Mean comments per path - on-track"    = "\\quad on-track",
  "Mean speakers per path"               = "Mean speakers per path",
  "% comments by top 1% of users"        = "\\% comments by top 1\\% of users",
  "% users with one comment"             = "\\% users with one comment",
  "% comments by [deleted]"              = "\\% comments by [deleted]",
  "Median word count"                    = "Median word count",
  "Median age at retrieval (days)"       = "Median age at retrieval (days)"
)
samp_tab <- samp %>%
  filter(Metric %in% names(keep_rows)) %>%
  mutate(Metric = factor(Metric, levels = names(keep_rows))) %>%
  arrange(Metric) %>%
  mutate(Metric = unname(keep_rows[as.character(Metric)]),
         Metric = ifelse(grepl("\\\\", Metric), Metric, tex_escape(Metric))) %>%
  # Date ranges are stacked over two lines so the table fits the text width
  mutate(across(-Metric, ~ ifelse(Metric == "Date range",
                                  stack_head(paste0(sub(" - .*", "", .x), "--"), sub(".* - ", "", .x), "r"),
                                  .x)))
tex_table(
  samp_tab, "tab_sample_comparison", align = "lrrrr",
  # stacked headings (no extra packages needed) keep the table within the text width
  header = c("", stack_head("Full", "corpus"), stack_head("Pairs from", "2015--17"),
             stack_head("Pairs from", "2018--22"), stack_head("Has post ID", "(analysis sample)")),
  raw_header = TRUE, raw_cols = names(samp_tab), midrule_after = 6, script = SCRIPT
)

# -- 7c. Within-pair differences (awry minus on-track), all pairs --
wp_note <- c(
  "Path length (comments)"               = sprintf("%s\\%% of pairs are the same length", tex_num(wk(0))),
  "Speakers per path"                    = sprintf("$\\sim$%s\\%% of paths have exactly two speakers; $\\sim$%s\\%% have three",
                                                   tex_num(pct_spk("2")), tex_num(pct_spk("3"))),
  "Final comment: own words"             = sprintf("Median difference is %s words",
                                                   tex_num(cell(wp_all, "median", measure = "Final comment: own words"), 0, plus = TRUE)),
  "Final comment quotes (pct points)"    = sprintf("%s\\%% vs %s\\%%", tex_num(q_awry), tex_num(q_on)),
  "Challenger returns (pct points)"      = sprintf("Challenger returns in $\\sim$%s\\%% of paths",
                                                   tex_num(mean(c(cell(chal, "pct_challenger_returns", outcome = "Awry"),
                                                                  cell(chal, "pct_challenger_returns", outcome = "On-track"))))),
  "Awry path started first (% of pairs)" = sprintf("50\\%% = no difference; %s\\%% of pairs start within three hours",
                                                   tex_num(cell(gap_d, "cum_pct", gap_fine = "1-3 hrs")))
)
wp_label <- c(
  "Path length (comments)"               = "Path length (comments)",
  "Speakers per path"                    = "Speakers per path",
  "Final comment: own words"             = "Final comment: own words",
  "Final comment quotes (pct points)"    = "Final comment quotes (pp)",
  "Challenger returns (pct points)"      = "Challenger returns (pp)",
  "Awry path started first (% of pairs)" = "Awry path started first (\\% of pairs)"
)
fmt_ci <- function(m, lo, hi, d, plus) {
  sprintf("%s [%s, %s]", tex_num(m, d, plus = plus), tex_num(lo, d), tex_num(hi, d))
}
wp_tab <- wp_all %>%
  mutate(across(c(mean, ci_lower, ci_upper, ref), parse_csv_num),
         d = case_when(measure == "Speakers per path" ~ 2,
                       measure == "Path length (comments)" ~ 2,
                       TRUE ~ 1),
         is_share = measure == "Awry path started first (% of pairs)",
         est = mapply(fmt_ci, mean, ci_lower, ci_upper, d, !is_share)) %>%
  transmute(Measure = unname(wp_label[measure]),
            Estimate = est,
            Notes = unname(wp_note[measure]))
stopifnot(!anyNA(wp_tab$Measure), !anyNA(wp_tab$Notes))
tex_table(
  wp_tab, "tab_within_pair_differences",
  align = ">{\\raggedright\\arraybackslash}p{0.27\\linewidth}l>{\\raggedright\\arraybackslash}p{0.40\\linewidth}",
  header = c("Measure", "Mean difference [95\\% CI]", "Notes"), raw_header = TRUE,
  raw_cols = c("Measure", "Notes"), script = SCRIPT
)

# -- 7d. Within-pair differences by political group (for later write-ups) --
wp_grp <- wp %>%
  filter(group != "All pairs") %>%
  mutate(across(c(mean, ci_lower, ci_upper), parse_csv_num),
         is_share = measure == "Awry path started first (% of pairs)",
         d = ifelse(measure %in% c("Speakers per path", "Path length (comments)"), 2, 1),
         est = mapply(fmt_ci, mean, ci_lower, ci_upper, d, !is_share),
         measure = unname(wp_label[measure])) %>%
  select(measure, group, est) %>%
  pivot_wider(names_from = group, values_from = est)
tex_table(
  wp_grp, "tab_within_pair_by_political_group", align = "lll",
  header = c("Measure", "Explicitly political", "Implicitly political"),
  raw_cols = "measure", script = SCRIPT
)

# -- 7e. Topic clusters (for later write-ups) --
topic_tab <- sizes %>%
  mutate(across(c(n_threads, n_pairs, pct_threads), parse_csv_num)) %>%
  arrange(desc(n_threads)) %>%
  transmute(Topic = topic_name,
            Coding = sub(" political", "", political_group),
            Subtype = political_subtype,
            Threads = tex_num(n_threads),
            Pairs = tex_num(n_pairs),
            `% threads` = tex_num(pct_threads, 1))
tex_table(
  topic_tab, "tab_topic_clusters", align = "lllrrr",
  header = c("Topic", "Political", "Subtype", "Threads", "Pairs", "\\% threads"),
  raw_header = TRUE, script = SCRIPT
)

message("\nDone. Next: source('sync_overleaf.R') to push tex_outputs/ and figures to Overleaf.")
