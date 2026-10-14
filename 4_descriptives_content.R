# 4. descriptives_content.R for Conversations Gone Awry (CMV) - Political Comments Analysis
#
# Comment text: cleaning, length, quoting, and how deltas appear in the text.
# No manual coding; everything here is computed from the text.
#
# Taxonomy (as in scripts 2-3):
#   Thread / first reply / challenger / path / pair.
#   Position in path: First reply (depth 0) | Middle | Final comment.
#   In awry paths the final comment is the removed comment (validated in 1_data_prep.R, 0a).
#   A path of 2 comments has no middle; the final comment is never counted as the first reply.
#
# Sections (output files are numbered to match):
#   1. Text cleaning: stored HTML symbols converted back; quoted paragraphs marked;
#      quote source (comment replied to / earlier comment / not in path)
#   2. Length, with and without quotes, by outcome and position
#   3. Quoting behaviour, by outcome and position
#   4. (Thread topics - see 5_topics.py and 5b_descriptives_topics.R)
#   5. How deltas appear in the text
#
# Text notes:
#   - Reddit stores text HTML-escaped: ">" as "&gt;", "&" as "&amp;", "<" as "&lt;".
#     Some text is escaped twice ("&amp;#8710;" for the delta sign), so "&amp;" is resolved
#     repeatedly before the other symbols.
#   - Quotes: Reddit markdown quotes a paragraph that starts with ">". A quote runs to the
#     next blank line (a line break without a blank line stays inside the quote), so text is
#     split into paragraphs on blank lines and a paragraph is a quote if it starts with ">".
#   - The original post is not in the data, so text quoted from the post (or any outside
#     source) shows up as "not found in the path". First replies can only quote the post.
#
# Means are shown with 95% confidence intervals. Comments are nested in paths, pairs and
# users, so intervals that treat comments as independent are narrower than they should be;
# they are a guide to precision, not tests.
#
# Outputs:
#   prepped_data_anonymized/comments_text_clean.rds   (full corpus; used by later scripts)
#   descriptive_results/content/*_<sample>.csv
#   graphical_output/content/*_<sample>.png
#
# Run after 1_data_prep.R and 1b_pseudonymize_users.R


# === SECTION 0: PREP ===

# -- 0. Packages --
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggpubr, patchwork, scales)

# -- 1. Sample switch --
# TRUE  = paths with a post ID only (Aug 2017 onward, ~83% of pairs). Default.
# FALSE = full corpus. Cleaning (section 1) always runs on the full corpus.
USE_POST_ID_SAMPLE <- TRUE

SAMPLE_TAG   <- if (USE_POST_ID_SAMPLE) "postid" else "full"
SAMPLE_LABEL <- if (USE_POST_ID_SAMPLE) "Paths with post ID (Aug 2017-Dec 2022)" else "Full corpus (Aug 2015-Dec 2022)"

# -- 2. Paths and helpers --
PREPPED <- "prepped_data_anonymized"   # shareable copy with user IDs (from 1b_pseudonymize_users.R)
DESC    <- file.path("descriptive_results", "content")
FIGS    <- file.path("graphical_output", "content")
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

# Count / mean axes: the tallest bar sits at ~80% of the axis height
scale_y_80 <- function(...) scale_y_continuous(..., expand = expansion(mult = c(0, 0.25)))

# Percentage axes: always the full 0-100% scale
scale_y_pct <- function(...) {
  list(
    scale_y_continuous(..., limits = c(0, 100), breaks = seq(0, 100, 20), expand = c(0, 0)),
    coord_cartesian(clip = "off")
  )
}

# Mean with t-based 95% CI (plus median), by any grouping columns
mean_ci <- function(df, value, ...) {
  df %>%
    group_by(...) %>%
    summarise(
      n      = sum(!is.na({{ value }})),
      mean   = mean({{ value }}, na.rm = TRUE),
      sd     = sd({{ value }}, na.rm = TRUE),
      median = median({{ value }}, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      se       = sd / sqrt(n),
      t95      = if_else(n > 1, qt(0.975, pmax(n - 1, 1)), NA_real_),
      ci_lower = mean - t95 * se,
      ci_upper = mean + t95 * se
    ) %>%
    select(-t95)
}

# Wilson 95% interval for a proportion, returned in percent
wilson_lower <- function(x, n, z = 1.96) {
  p <- x / n
  100 * ((p + z^2 / (2 * n)) - z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / (1 + z^2 / n)
}
wilson_upper <- function(x, n, z = 1.96) {
  p <- x / n
  100 * ((p + z^2 / (2 * n)) + z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / (1 + z^2 / n)
}

# Share of comments with a TRUE flag, with Wilson CI, by any grouping columns
pct_ci <- function(df, flag, ...) {
  df %>%
    group_by(...) %>%
    summarise(n = n(), n_yes = sum({{ flag }}, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      pct      = 100 * n_yes / n,
      ci_lower = wilson_lower(n_yes, n),
      ci_upper = wilson_upper(n_yes, n)
    )
}

POSITION_LEVELS <- c("First reply", "Middle", "Final comment")
STEP_LEVELS     <- c("5+ before", "4 before", "3 before", "2 before", "1 before", "Final")

# -- 3. Load data --
message("Loading prepped data...")
ce_all <- readRDS(file.path(PREPPED, "comments_enriched.rds"))

# Position in path. Paths are linear (1_data_prep.R, 7a), so depth order = reply order.
ce_all <- ce_all %>%
  mutate(outcome = factor(if_else(has_removed_comment, "Awry", "On-track"),
                          levels = c("Awry", "On-track"))) %>%
  arrange(conversation_id, thread_depth) %>%
  group_by(conversation_id) %>%
  mutate(
    path_length    = n(),
    pos            = row_number(),
    steps_from_end = path_length - pos,
    challenger     = speaker[pos == 1][1],
    parent_speaker = lag(speaker)
  ) %>%
  ungroup() %>%
  mutate(
    position = case_when(
      steps_from_end == 0 ~ "Final comment",
      pos == 1            ~ "First reply",
      TRUE                ~ "Middle"
    ) %>% factor(levels = POSITION_LEVELS),
    steps_group = case_when(
      steps_from_end == 0 ~ "Final",
      steps_from_end >= 5 ~ "5+ before",
      TRUE                ~ paste(steps_from_end, "before")
    ) %>% factor(levels = STEP_LEVELS)
  )


# === SECTION 1: TEXT CLEANING ===

cat("\n=== SECTION 1: TEXT CLEANING ===\n")

# -- 1a. Convert stored symbols back --

dec_entity_to_chr <- function(m) {
  code <- strtoi(str_remove_all(m, "^&#|;$"), base = 10L)
  coalesce(intToUtf8(code, multiple = TRUE), "")
}
hex_entity_to_chr <- function(m) {
  code <- strtoi(str_remove_all(m, "^&#[xX]|;$"), base = 16L)
  coalesce(intToUtf8(code, multiple = TRUE), "")
}

unescape_html <- function(x) {
  x <- coalesce(x, "")
  for (i in 1:3) x <- str_replace_all(x, fixed("&amp;"), "&")   # double-escaped text
  x <- str_replace_all(x, c("&gt;" = ">", "&lt;" = "<", "&quot;" = "\"",
                            "&#39;" = "'", "&nbsp;" = " "))
  x <- str_replace_all(x, "&#[xX][0-9A-Fa-f]+;", hex_entity_to_chr)
  x <- str_replace_all(x, "&#[0-9]+;", dec_entity_to_chr)
  str_remove_all(x, "[\u200B\u200C\u200D\uFEFF]")               # zero-width characters
}

normalize_text <- function(x) {
  x %>% str_to_lower() %>% str_replace_all("[^a-z0-9]+", " ") %>% str_squish()
}

# Which stored symbols occur, before cleaning
entity_counts <- tibble(text = coalesce(ce_all$text, "")) %>%
  mutate(entity = str_extract_all(text, "&#?[A-Za-z0-9]+;")) %>%
  select(entity) %>%
  unnest_longer(entity) %>%
  filter(!is.na(entity)) %>%
  count(entity, sort = TRUE, name = "n_occurrences")

ce_all <- ce_all %>%
  mutate(
    text_clean = unescape_html(text),
    text_norm  = normalize_text(text_clean)
  )

cleaning_check <- tibble(
  Check = c("Comments", "Comments with a stored symbol (&...;) before cleaning",
            "Comments with a stored symbol after cleaning",
            "Comments starting a line with '&gt;' before cleaning",
            "Comments with a quote paragraph after cleaning (see 1b)"),
  N = c(nrow(ce_all),
        sum(str_detect(coalesce(ce_all$text, ""), "&#?[A-Za-z0-9]+;")),
        sum(str_detect(ce_all$text_clean, "&#?[A-Za-z0-9]+;")),
        sum(str_detect(coalesce(ce_all$text, ""), "(?m)^\\s*&gt;")),
        NA_integer_)
)

# -- 1b. Mark quoted paragraphs --
paras <- ce_all %>%
  select(comment_id, conversation_id, thread_depth, text_clean) %>%
  mutate(para = str_split(text_clean, "\\n[ \\t]*\\n")) %>%
  select(-text_clean) %>%
  unnest_longer(para, indices_to = "para_n") %>%
  mutate(para = str_trim(para)) %>%
  filter(!is.na(para), para != "") %>%
  mutate(
    is_quote   = str_detect(para, "^>"),
    para_plain = if_else(is_quote, str_remove_all(para, "(?m)^\\s*>+\\s?"), para),
    para_words = str_count(para_plain, "\\S+")
  ) %>%
  group_by(comment_id) %>%
  mutate(new_block = is_quote & !lag(is_quote, default = FALSE)) %>%
  ungroup()

quote_summary <- paras %>%
  group_by(comment_id) %>%
  summarise(
    n_quote_blocks = sum(new_block),
    words_quoted   = sum(para_words[is_quote]),
    words_own      = sum(para_words[!is_quote]),
    text_own       = paste(para[!is_quote], collapse = "\n\n"),
    .groups = "drop"
  )

ce_all <- ce_all %>%
  left_join(quote_summary, by = "comment_id") %>%
  mutate(
    n_quote_blocks = coalesce(n_quote_blocks, 0L),
    words_quoted   = coalesce(words_quoted, 0L),
    words_own      = coalesce(words_own, 0L),
    text_own       = coalesce(text_own, ""),
    words_total    = words_own + words_quoted,
    has_quote      = n_quote_blocks > 0,
    pct_quoted     = if_else(words_total > 0, 100 * words_quoted / words_total, NA_real_)
  )

cleaning_check$N[5] <- sum(ce_all$has_quote)

# -- 1c. Where quoted text comes from --
# A quote paragraph is matched to the earlier comments in its path: its first 60 characters
# (lower case, letters and digits only) are searched for in each earlier comment.
# Paragraphs shorter than 15 characters after normalising are too short to match reliably.
QUOTE_SNIPPET_CHARS <- 60
QUOTE_MIN_CHARS     <- 15

quote_paras <- paras %>%
  filter(is_quote) %>%
  mutate(snippet = str_trim(str_sub(normalize_text(para_plain), 1, QUOTE_SNIPPET_CHARS)),
         matchable = nchar(snippet) >= QUOTE_MIN_CHARS)

quote_matches <- quote_paras %>%
  filter(matchable) %>%
  select(comment_id, conversation_id, thread_depth, para_n, snippet) %>%
  inner_join(ce_all %>% select(conversation_id, anc_depth = thread_depth, anc_norm = text_norm),
             by = "conversation_id", relationship = "many-to-many") %>%
  filter(anc_depth < thread_depth) %>%
  mutate(found = str_detect(anc_norm, fixed(snippet))) %>%
  group_by(comment_id, para_n) %>%
  summarise(
    found_parent  = any(found & anc_depth == thread_depth - 1),
    found_earlier = any(found & anc_depth <  thread_depth - 1),
    .groups = "drop"
  )

quote_source_para <- quote_paras %>%
  select(comment_id, para_n, matchable) %>%
  left_join(quote_matches, by = c("comment_id", "para_n")) %>%
  mutate(across(c(found_parent, found_earlier), ~ coalesce(.x, FALSE)))

QUOTE_SOURCE_LEVELS <- c("Quotes the comment replied to",
                         "Quotes an earlier comment only",
                         "Quoted text not in path (post or outside source)",
                         "Quotes too short to match")

quote_source_comment <- quote_source_para %>%
  group_by(comment_id) %>%
  summarise(
    quote_source = case_when(
      any(found_parent)  ~ QUOTE_SOURCE_LEVELS[1],
      any(found_earlier) ~ QUOTE_SOURCE_LEVELS[2],
      any(matchable)     ~ QUOTE_SOURCE_LEVELS[3],
      TRUE               ~ QUOTE_SOURCE_LEVELS[4]
    ),
    .groups = "drop"
  )

ce_all <- ce_all %>%
  left_join(quote_source_comment, by = "comment_id") %>%
  mutate(quote_source = factor(quote_source, levels = QUOTE_SOURCE_LEVELS))

# -- 1d. Delta mentions (used in section 5) --
DELTA_SYMBOL_RE <- "[\u0394\u2206]"     # Greek capital delta and the increment sign
ce_all <- ce_all %>%
  mutate(
    delta_symbol_any   = str_detect(text_clean, DELTA_SYMBOL_RE),
    delta_symbol_own   = str_detect(text_own, DELTA_SYMBOL_RE),
    delta_symbol_quote = delta_symbol_any & !delta_symbol_own,
    delta_word         = str_detect(text_clean, regex("\\bdeltas?\\b", ignore_case = TRUE)),
    delta_command      = str_detect(text_clean, regex("!delta", ignore_case = TRUE))
  )

# -- 1e. Save the cleaned text table (full corpus) --
text_clean_out <- ce_all %>%
  select(comment_id, conversation_id, pair_key, post_id, thread_depth, position, steps_from_end,
         text_clean, text_own, words_total, words_own, words_quoted, pct_quoted,
         n_quote_blocks, has_quote, quote_source,
         delta_symbol_any, delta_symbol_own, delta_word, delta_command)

saveRDS(text_clean_out, file.path(PREPPED, "comments_text_clean.rds"))
cat(sprintf("  -> saved: %s/comments_text_clean.rds (%s comments, full corpus)\n",
            PREPPED, fmt(nrow(text_clean_out))))

save_csv(entity_counts,  "1a_stored_symbols_before_cleaning")
save_csv(cleaning_check, "1a_cleaning_check")

# -- Apply sample for sections 2-5 --
ce <- if (USE_POST_ID_SAMPLE) ce_all %>% filter(has_post_id) else ce_all

cap <- paste0("Sample: ", SAMPLE_LABEL, ". Final comment in awry paths = removed comment. ")
cap_ci <- "95% CIs treat comments as independent (they are nested in paths and users), so they are too narrow."

cat(sprintf("\nSample: %s | %s comments | %s paths\n",
            SAMPLE_LABEL, fmt(nrow(ce)), fmt(n_distinct(ce$conversation_id))))

# Quote source table, by outcome and position (comments with at least one quote)
quote_source_table <- ce %>%
  filter(has_quote) %>%
  count(outcome, position, quote_source, name = "n") %>%
  complete(outcome, position, quote_source, fill = list(n = 0L)) %>%
  group_by(outcome, position) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  ungroup()

save_csv(quote_source_table, "1c_quote_source_by_outcome_position")


# === SECTION 2: LENGTH, WITH AND WITHOUT QUOTES ===

cat("\n=== SECTION 2: LENGTH ===\n")

LENGTH_MEASURES <- c("words_total" = "All words", "words_own" = "Own words (quotes removed)")

length_long <- ce %>%
  select(comment_id, pair_key, outcome, position, steps_group, words_total, words_own) %>%
  pivot_longer(c(words_total, words_own), names_to = "measure_var", values_to = "words") %>%
  mutate(measure = factor(LENGTH_MEASURES[measure_var], levels = LENGTH_MEASURES))

# -- 2a. By outcome and position --
length_by_position <- length_long %>%
  mean_ci(words, measure, outcome, position) %>%
  mutate(across(where(is.double), ~ round(.x, 2)))

save_csv(length_by_position, "2a_length_by_outcome_position")

p_len_pos <- length_by_position %>%
  ggplot(aes(x = position, y = mean, fill = outcome)) +
  geom_col(position = position_dodge(0.8), width = 0.75) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), position = position_dodge(0.8),
                width = 0.2, colour = "#333333") +
  geom_text(aes(y = ci_upper, label = round(mean, 0)), position = position_dodge(0.8),
            vjust = -0.6, size = 3) +
  facet_wrap(~ measure) +
  scale_fill_manual(values = col_outcome) +
  scale_y_80() +
  labs(title = "Comment length by position in path",
       subtitle = "Bars = mean words per comment; lines = 95% CI",
       x = NULL, y = "Mean words per comment",
       caption = paste0(cap, "Quotes = paragraphs starting with '>'.\n", cap_ci)) +
  base_theme +
  theme(legend.position = "top", strip.text = element_text(face = "bold"))

save_fig(p_len_pos, "2a_length_by_outcome_position", width = 11, height = 5.5)

# -- 2b. Lead-up to the final comment --
length_by_step <- length_long %>%
  mean_ci(words, measure, outcome, steps_group) %>%
  mutate(across(where(is.double), ~ round(.x, 2)))

save_csv(length_by_step, "2b_length_by_steps_before_final")

p_len_step <- length_by_step %>%
  ggplot(aes(x = steps_group, y = mean, colour = outcome, group = outcome)) +
  geom_line(linewidth = 0.8, position = position_dodge(0.3)) +
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper), position = position_dodge(0.3), size = 0.35) +
  facet_wrap(~ measure) +
  scale_colour_manual(values = col_outcome) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Comment length in the lead-up to the final comment",
       subtitle = "Points = mean words per comment; lines = 95% CI",
       x = "Comments before the final comment", y = "Mean words per comment",
       caption = paste0(cap, "'5+ before' only exists in paths of 6+ comments, so the left of each line comes from longer paths.\n",
                        cap_ci)) +
  base_theme +
  theme(legend.position = "top", strip.text = element_text(face = "bold"))

save_fig(p_len_step, "2b_length_by_steps_before_final", width = 11, height = 5.5)

# -- 2c. Within pairs: removed comment vs the on-track path's final comment --
final_pairs <- ce %>%
  filter(position == "Final comment") %>%
  mutate(o = if_else(outcome == "Awry", "awry", "ontrack")) %>%
  select(pair_key, o, words_total, words_own, has_quote) %>%
  pivot_wider(names_from = o, values_from = c(words_total, words_own, has_quote)) %>%
  filter(!is.na(words_total_awry), !is.na(words_total_ontrack))

paired_summary <- function(diff, label) {
  n  <- sum(!is.na(diff))
  m  <- mean(diff, na.rm = TRUE)
  se <- sd(diff, na.rm = TRUE) / sqrt(n)
  t  <- qt(0.975, n - 1)
  tibble(
    measure             = label,
    n_pairs             = n,
    mean_diff_awry_minus_ontrack = round(m, 2),
    ci_lower            = round(m - t * se, 2),
    ci_upper            = round(m + t * se, 2),
    median_diff         = round(median(diff, na.rm = TRUE), 2),
    pct_pairs_awry_higher = round(100 * mean(diff > 0, na.rm = TRUE), 2),
    pct_pairs_equal       = round(100 * mean(diff == 0, na.rm = TRUE), 2),
    pct_pairs_awry_lower  = round(100 * mean(diff < 0, na.rm = TRUE), 2)
  )
}

final_pair_length <- bind_rows(
  paired_summary(final_pairs$words_total_awry - final_pairs$words_total_ontrack, "All words"),
  paired_summary(final_pairs$words_own_awry   - final_pairs$words_own_ontrack,   "Own words (quotes removed)")
)

save_csv(final_pair_length, "2c_final_comment_length_within_pair")

# Full distribution of length (quantiles), for reference
length_quantiles <- length_long %>%
  group_by(measure, outcome, position) %>%
  summarise(
    n   = n(),
    p10 = quantile(words, 0.10), p25 = quantile(words, 0.25), p50 = quantile(words, 0.50),
    p75 = quantile(words, 0.75), p90 = quantile(words, 0.90), p99 = quantile(words, 0.99),
    max = max(words),
    pct_zero_words = round(100 * mean(words == 0), 2),
    .groups = "drop"
  )

save_csv(length_quantiles, "2d_length_quantiles_detailed")


# === SECTION 3: QUOTING BEHAVIOUR ===

cat("\n=== SECTION 3: QUOTING ===\n")

# -- 3a. Share of comments that quote, by outcome and position --
quote_by_position <- ce %>%
  pct_ci(has_quote, outcome, position) %>%
  mutate(across(where(is.double), ~ round(.x, 2)))

save_csv(quote_by_position, "3a_pct_quoting_by_outcome_position")

p_quote_pos <- quote_by_position %>%
  ggplot(aes(x = position, y = pct, fill = outcome)) +
  geom_col(position = position_dodge(0.8), width = 0.75) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), position = position_dodge(0.8),
                width = 0.2, colour = "#333333") +
  geom_text(aes(y = ci_upper, label = paste0(round(pct, 1), "%")), position = position_dodge(0.8),
            vjust = -0.6, size = 3) +
  scale_fill_manual(values = col_outcome) +
  scale_y_pct() +
  labs(title = "Share of comments that quote",
       subtitle = "Comments with at least one paragraph starting with '>'; lines = 95% CI",
       x = NULL, y = "% of comments",
       caption = paste0(cap, "\n", cap_ci)) +
  base_theme +
  theme(legend.position = "top")

save_fig(p_quote_pos, "3a_pct_quoting_by_outcome_position", width = 8, height = 5.5)

# -- 3b. How much is quoted, among comments that quote --
quote_amount <- bind_rows(
  ce %>% filter(has_quote) %>% mean_ci(pct_quoted, outcome, position) %>%
    mutate(measure = "% of words that are quoted"),
  ce %>% filter(has_quote) %>% mean_ci(n_quote_blocks, outcome, position) %>%
    mutate(measure = "Quote blocks per comment"),
  ce %>% filter(has_quote) %>% mean_ci(words_own, outcome, position) %>%
    mutate(measure = "Own words (quotes removed)")
) %>%
  relocate(measure) %>%
  mutate(across(where(is.double), ~ round(.x, 2)))

save_csv(quote_amount, "3b_quote_amount_among_quoting_comments")

# -- 3c. Where quotes come from (middle and final comments) --
# First replies are left out: the only thing they can quote is the post, which is not in the data.
quote_source_plot <- quote_source_table %>%
  filter(position != "First reply", quote_source != QUOTE_SOURCE_LEVELS[4]) %>%
  group_by(outcome, position) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  mutate(quote_source = factor(quote_source, levels = rev(QUOTE_SOURCE_LEVELS[1:3])))

col_source <- setNames(c("#3B6FB6", "#9ECAE1", "#D9D9D9"), QUOTE_SOURCE_LEVELS[1:3])

p_quote_source <- quote_source_plot %>%
  ggplot(aes(x = outcome, y = pct, fill = quote_source)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = if_else(pct >= 4, paste0(round(pct, 0), "%"), "")),
            position = position_stack(vjust = 0.5), size = 3) +
  facet_wrap(~ position) +
  scale_fill_manual(values = col_source, breaks = QUOTE_SOURCE_LEVELS[1:3]) +
  scale_y_pct() +
  labs(title = "What quoting comments quote",
       subtitle = "Comments with at least one quote; a comment counts once, by its closest match",
       x = NULL, y = "% of quoting comments",
       caption = paste0(cap, "First replies excluded (they can only quote the post, which is not in the data). ",
                        "Quotes under 15 characters excluded.\n",
                        "Match = first 60 characters of the quote found in an earlier comment of the same path.")) +
  base_theme +
  theme(legend.position = "top", strip.text = element_text(face = "bold")) +
  guides(fill = guide_legend(nrow = 1))

save_fig(p_quote_source, "3c_quote_source_by_outcome_position", width = 10, height = 5.5)

# -- 3d. Quoting in the lead-up to the final comment --
quote_by_step <- ce %>%
  pct_ci(has_quote, outcome, steps_group) %>%
  mutate(across(where(is.double), ~ round(.x, 2)))

save_csv(quote_by_step, "3d_pct_quoting_by_steps_before_final")

p_quote_step <- quote_by_step %>%
  ggplot(aes(x = steps_group, y = pct, colour = outcome, group = outcome)) +
  geom_line(linewidth = 0.8, position = position_dodge(0.3)) +
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper), position = position_dodge(0.3), size = 0.35) +
  scale_colour_manual(values = col_outcome) +
  scale_y_pct() +
  labs(title = "Quoting in the lead-up to the final comment",
       subtitle = "% of comments with at least one quote; lines = 95% CI",
       x = "Comments before the final comment", y = "% of comments",
       caption = paste0(cap, "'5+ before' only exists in paths of 6+ comments.\n", cap_ci)) +
  base_theme +
  theme(legend.position = "top")

save_fig(p_quote_step, "3d_pct_quoting_by_steps_before_final", width = 9, height = 5.5)

# -- 3e. Within pairs: does the removed comment quote when the on-track final comment does not? --
final_pair_quote <- final_pairs %>%
  mutate(pattern = case_when(
    has_quote_awry & has_quote_ontrack   ~ "Both final comments quote",
    has_quote_awry & !has_quote_ontrack  ~ "Only the removed comment quotes",
    !has_quote_awry & has_quote_ontrack  ~ "Only the on-track final comment quotes",
    TRUE                                 ~ "Neither quotes"
  )) %>%
  count(pattern, name = "n_pairs") %>%
  mutate(pct = round(100 * n_pairs / sum(n_pairs), 2))

save_csv(final_pair_quote, "3e_final_comment_quoting_within_pair")


# === SECTION 4: THREAD TOPICS ===
# Built in 5_topics.py (clustering of post title slugs) and described in 5b_descriptives_topics.R.


# === SECTION 5: HOW DELTAS APPEAR IN THE TEXT ===

cat("\n=== SECTION 5: DELTAS IN THE TEXT ===\n")

# In CMV a delta is awarded by writing the delta sign (or "!delta") in a reply, with an
# explanation. DeltaBot then confirms it and updates the recipient's flair.
# DeltaBot's own comments are not in the corpus (checked on the username export), so the
# only traces are the sign in users' text and the flair counts (script 3, section 6).

# -- 5a. Which delta markers appear at all --
delta_markers <- tibble(
  Marker = c("Delta sign (\u0394 or \u2206) anywhere in the text",
             "  - in the comment's own words",
             "  - only inside quoted text",
             "The word 'delta' / 'deltas'",
             "The '!delta' command",
             "Comments whose author shows delta flair"),
  N_comments = c(sum(ce$delta_symbol_any), sum(ce$delta_symbol_own), sum(ce$delta_symbol_quote),
                 sum(ce$delta_word), sum(ce$delta_command),
                 sum(!is.na(ce$author_flair_text) & str_detect(ce$author_flair_text, DELTA_SYMBOL_RE)))
) %>%
  mutate(pct_of_comments = round(100 * N_comments / nrow(ce), 3))

save_csv(delta_markers, "5a_delta_markers")

if (sum(ce$delta_word) == 0 && sum(ce$delta_command) == 0) {
  cat(paste0("\nNOTE: the word 'delta' and the '!delta' command never appear, although the sign does.\n",
             "In a subreddit organised around deltas this points to filtering during corpus\n",
             "construction (comments or words removed). Worth checking against the corpus documentation.\n"))
}

# -- 5b. Which comments are awarded a delta --
# Table of where the delta sign is WRITTEN (the awarding comment), kept for reference:
delta_by_position <- ce %>%
  pct_ci(delta_symbol_own, outcome, position) %>%
  mutate(across(where(is.double), ~ round(.x, 3)))

save_csv(delta_by_position, "5b_delta_sign_given_by_outcome_position")

# Chart: which comments RECEIVE a delta. A comment counts as awarded when the next comment in
# the path (its reply) contains the delta sign in its own words. Final comments are left out:
# nothing replies to them within the path, so they cannot be awarded one here.
AWARD_POS_LEVELS <- c("First reply", "Second reply", "Third reply or later")

delta_awarded <- ce %>%
  arrange(conversation_id, thread_depth) %>%
  group_by(conversation_id) %>%
  mutate(awarded_delta = lead(delta_symbol_own, default = FALSE)) %>%
  ungroup() %>%
  filter(position != "Final comment") %>%
  mutate(award_position = factor(case_when(pos == 1 ~ "First reply",
                                           pos == 2 ~ "Second reply",
                                           TRUE     ~ "Third reply or later"),
                                 levels = AWARD_POS_LEVELS))

delta_awarded_by_position <- delta_awarded %>%
  pct_ci(awarded_delta, outcome, award_position) %>%
  mutate(across(where(is.double), ~ round(.x, 3)))

save_csv(delta_awarded_by_position, "5b_delta_awarded_by_outcome_position")

# Percentages are small, so this chart uses its own axis (an exception to the 0-100% rule):
# at least 0.3%, extended if needed so no confidence interval is cut off.
DELTA_AXIS_MAX <- max(0.3, ceiling(10 * max(delta_awarded_by_position$ci_upper) + 0.5) / 10)
DELTA_AXIS_STEP <- if (DELTA_AXIS_MAX > 1) 0.25 else 0.1

p_delta_awarded <- delta_awarded_by_position %>%
  ggplot(aes(x = award_position, y = pct, fill = outcome)) +
  geom_col(position = position_dodge(0.8), width = 0.75) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), position = position_dodge(0.8),
                width = 0.2, colour = "#333333") +
  geom_text(aes(y = ci_upper, label = paste0(formatC(pct, format = "f", digits = 2), "%\n(n = ", n_yes, ")")),
            position = position_dodge(0.8), vjust = -0.3, size = 2.8, lineheight = 0.9) +
  scale_fill_manual(values = col_outcome) +
  scale_y_continuous(limits = c(0, DELTA_AXIS_MAX), breaks = seq(0, DELTA_AXIS_MAX, DELTA_AXIS_STEP),
                     labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "Which comments are awarded deltas",
       subtitle = "% of comments whose reply contains the delta sign; lines = 95% CI",
       x = NULL, y = "% of comments awarded a delta",
       caption = paste0("Sample: ", SAMPLE_LABEL, ". Awarded = the next comment in the path contains the delta sign in its own words (not in a quote).\n",
                        "Final comments excluded: nothing replies to them within the path. ",
                        "Axis runs to ", DELTA_AXIS_MAX, "% (not 0-100%) because all values are small.")) +
  base_theme +
  theme(legend.position = "top")

save_fig(p_delta_awarded, "5b_delta_awarded_by_outcome_position", width = 8, height = 5.5)

# -- 5c. Paths with a delta before the final comment --
delta_paths <- ce %>%
  group_by(conversation_id, outcome) %>%
  summarise(
    delta_before_final = any(delta_symbol_own & position != "Final comment"),
    delta_in_final     = any(delta_symbol_own & position == "Final comment"),
    .groups = "drop"
  ) %>%
  group_by(outcome) %>%
  summarise(
    n_paths                   = n(),
    n_delta_before_final      = sum(delta_before_final),
    pct_delta_before_final    = round(100 * mean(delta_before_final), 3),
    n_delta_in_final          = sum(delta_in_final),
    pct_delta_in_final        = round(100 * mean(delta_in_final), 3),
    .groups = "drop"
  )

save_csv(delta_paths, "5c_paths_with_delta_sign")

# -- 5d. Who gives and receives a delta within the path --
# Giver = author of the comment with the sign; recipient = author of the comment it replies to.
# The original poster is not identifiable (the post is not in the data), so the giver is
# described relative to the challenger (first-reply author) only.
delta_roles <- ce %>%
  filter(delta_symbol_own, position != "First reply") %>%
  mutate(
    recipient = case_when(
      parent_speaker == "[deleted]"  ~ "Recipient is [deleted]",
      parent_speaker == challenger   ~ "Recipient is the challenger",
      TRUE                           ~ "Recipient is another user"
    ),
    giver = case_when(
      speaker == "[deleted]"  ~ "Giver is [deleted]",
      speaker == challenger   ~ "Giver is the challenger",
      TRUE                    ~ "Giver is another user"
    )
  ) %>%
  count(outcome, giver, recipient, name = "n_comments") %>%
  group_by(outcome) %>%
  mutate(pct = round(100 * n_comments / sum(n_comments), 2)) %>%
  ungroup()

save_csv(delta_roles, "5d_delta_giver_recipient")

# Who receives the delta, by outcome (givers pooled; giver breakdown is in the table above)
RECIPIENT_LEVELS <- c("Recipient is the challenger", "Recipient is another user", "Recipient is [deleted]")
col_recipient <- setNames(c(col_neutral, "#BDBDBD", "#757575"), RECIPIENT_LEVELS)

delta_recipient <- delta_roles %>%
  group_by(outcome, recipient) %>%
  summarise(n_comments = sum(n_comments), .groups = "drop") %>%
  complete(outcome, recipient = RECIPIENT_LEVELS, fill = list(n_comments = 0L)) %>%
  group_by(outcome) %>%
  mutate(n_total = sum(n_comments), pct = round(100 * n_comments / n_total, 2)) %>%
  ungroup()

save_csv(delta_recipient, "5d_delta_recipient")

giver_shares <- delta_roles %>%
  group_by(giver) %>%
  summarise(n = sum(n_comments), .groups = "drop") %>%
  mutate(pct = 100 * n / sum(n))
giver_pct <- function(g) round(sum(giver_shares$pct[giver_shares$giver == g]), 0)

p_delta_recipient <- delta_recipient %>%
  mutate(recipient   = factor(recipient, levels = rev(RECIPIENT_LEVELS)),
         outcome_lab = paste0(outcome, "\n(n = ", n_total, ")")) %>%
  ggplot(aes(x = outcome_lab, y = pct, fill = recipient)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = if_else(pct >= 4, paste0(round(pct, 0), "%"), "")),
            position = position_stack(vjust = 0.5), size = 3.2) +
  scale_fill_manual(values = col_recipient, breaks = RECIPIENT_LEVELS) +
  scale_y_pct() +
  labs(title = "Deltas in the text mostly go to the challenger",
       subtitle = "Comments containing the delta sign, by who wrote the comment being replied to",
       x = NULL, y = "% of delta comments",
       caption = paste0(cap, "\nRecipient = author of the comment the delta comment replies to; challenger = first-reply author. ",
                        "Givers: ", giver_pct("Giver is another user"), "% another user, ",
                        giver_pct("Giver is the challenger"), "% the challenger, ",
                        giver_pct("Giver is [deleted]"), "% [deleted]\n",
                        "(consistent with the original poster awarding deltas, though the original poster cannot be identified in the data).")) +
  base_theme +
  theme(legend.position = "top") +
  guides(fill = guide_legend(nrow = 1))

save_fig(p_delta_recipient, "5d_delta_recipient", width = 8, height = 5.5)

# Where delta comments sit relative to the end of the path
delta_steps <- ce %>%
  filter(delta_symbol_own) %>%
  count(outcome, steps_group, name = "n_comments", .drop = FALSE) %>%
  group_by(outcome) %>%
  mutate(pct = round(100 * n_comments / sum(n_comments), 2)) %>%
  ungroup()

save_csv(delta_steps, "5e_delta_sign_by_steps_before_final")

cat("\nDone. Cleaned text saved to", file.path(PREPPED, "comments_text_clean.rds"), "\n")