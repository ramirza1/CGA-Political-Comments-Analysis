# 5b. descriptives_topics.R for Conversations Gone Awry (CMV) - Political Comments Analysis
#
# Describes the inductive thread topics built in 5_topics.py: what topics the corpus
# contains, how that changes over time, and whether the within-pair differences
# (awry minus on-track) vary by topic.
#
# Both paths in a pair come from the same thread, so they always share a topic: topic
# cannot predict which path goes awry. What it can show is what the corpus is about, and
# whether the awry/on-track contrast looks different in different kinds of threads.
#
# Topic names: if descriptive_results/topics/topic_coding_k<K>.csv (or .xlsx) exists - the
# filled copy of the BLANK coding sheet from 5_topics.py - its topic_name, political and
# political_subtype columns are used. Otherwise topics are labelled by number and top terms,
# and the named chart (1b) and section 4 are skipped.
#
# Sample: paths with a post ID only (topics come from post title slugs).
#
# Sections:
#   1. Topic sizes: threads, pairs, paths, comments, users
#      1b. Threads per topic with topic names, coloured by political coding (once coded).
#          The original chart from 5_topics.py (topic_sizes_k<K>.png) is left as it is.
#   2. Topics over time (share of each year's threads)
#   3. Within-pair differences by topic
#   4. Within-pair differences by political coding (once the coding sheet is filled in)
#   5. Where high-removal users post: share of each user's threads by political coding
#      Whole sample: 3+ removed comments vs 1-2 vs none
#      Users with 25+ comments: removal rate over 10% vs 10% or less
#      Breakdowns: explicit/implicit; subtypes and paired subtypes (both exploratory)
#
# Outputs:
#   descriptive_results/topics/5b_*.csv
#   graphical_output/topics/5b_*.png
#
# Run after 3_descriptives_users.R (section 5 reads its user table), 4_descriptives_content.R
# and 5_topics.py


# === SECTION 0: PREP ===

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggpubr, scales)

PREPPED     <- "prepped_data_anonymized"
TOPICS_FILE <- file.path(PREPPED, "thread_topics.csv")
TEXT_FILE   <- file.path(PREPPED, "comments_text_clean.rds")
USER_FILE   <- file.path("input_data_anonymized", "user_table_postid.csv")   # from script 3
DESC        <- file.path("descriptive_results", "topics")
FIGS        <- file.path("graphical_output", "topics")
dir.create(DESC, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGS, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(TOPICS_FILE)) stop("Run 5_topics.py first: ", TOPICS_FILE, " not found.")
if (!file.exists(TEXT_FILE))   stop("Run 4_descriptives_content.R first: ", TEXT_FILE, " not found.")

SAMPLE_LABEL <- "Paths with post ID (Aug 2017-Dec 2022)"

fmt <- function(x) format(round(x, 2), big.mark = ",")

save_csv <- function(df, name) {
  filename <- paste0(name, ".csv")
  write_csv(df, file.path(DESC, filename))
  cat(sprintf("  -> saved: %s\n", filename))
  print(df, n = 50, width = Inf)
  invisible(df)
}

save_fig <- function(p, name, width = 9, height = 5) {
  filename <- paste0(name, ".png")
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

col_neutral <- "#5C85D6"

# -- Load --
topics <- read_csv(TOPICS_FILE, col_types = cols(post_id = col_character(), .default = col_guess()))
K <- first(topics$k)

# Coding sheet: topic_coding_k<K>.csv or .xlsx (filled copy of the BLANK sheet from 5_topics.py).
# Columns used: topic, topic_name, political, political_subtype (optional; add it if needed).
# Values are used as entered, so the scheme can change without editing this script.
# Shorthand is expanded: Explicit -> "Explicitly political", Implicit -> "Implicitly political",
# Y -> "Political", N -> "Non-political", M -> "Mixed".
read_topic_coding <- function(dir, K) {
  # Finds the filled coding sheet: topic_coding_k<K> or topic_coding_sheet_k<K>, saved as
  # .csv or .xlsx (a doubled ".csv.xlsx" from Excel also works). The _BLANK sheet is ignored.
  # If several match, the most recently saved one is used.
  pattern <- paste0("^topic_coding(_sheet)?_k", K, "(\\.csv)?\\.(csv|xlsx)$")
  files <- list.files(dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  files <- files[!str_detect(basename(files), regex("blank", ignore_case = TRUE))]
  if (length(files) == 0) {
    message("\n*** No filled coding sheet found in ", dir, " (looked for topic_coding_k", K,
            ".csv / .xlsx).\n*** Topics are labelled by top terms and the political charts are skipped.\n")
    return(tibble(topic = double(), topic_name = character(), political = character(),
                  political_subtype = character()))
  }
  file <- files[order(file.mtime(files), decreasing = TRUE)][1]
  if (length(files) > 1) message("Several coding sheets found; using the most recent: ", basename(file))
  cat("Using topic coding from", file, "\n")
  raw <- if (str_detect(file, regex("\\.xlsx$", ignore_case = TRUE))) {
    readxl::read_excel(file)
  } else {
    read_csv(file, show_col_types = FALSE)
  }
  if (!"topic" %in% names(raw) || nrow(raw) > 3 * K) {
    stop(basename(file), " does not look like the coding sheet (", nrow(raw), " rows; expected one row per topic). ",
         "Is it the thread table (thread_topics_k", K, ".csv) under the wrong name?")
  }
  for (col in c("topic_name", "political", "political_subtype")) {
    if (!col %in% names(raw)) raw[[col]] <- NA_character_
  }
  out <- raw %>%
    transmute(topic = as.double(topic),
              across(c(topic_name, political, political_subtype),
                     ~ na_if(str_trim(as.character(.x)), ""))) %>%
    filter(!is.na(topic))
  # One row per topic. Exact repeats are dropped with a warning; conflicting codes stop the run.
  dup_topics <- out %>% count(topic) %>% filter(n > 1) %>% pull(topic)
  if (length(dup_topics) > 0) {
    conflicting <- out %>% filter(topic %in% dup_topics) %>% distinct() %>%
      count(topic) %>% filter(n > 1) %>% pull(topic)
    if (length(conflicting) > 0) {
      stop("Coding sheet has different codes for the same topic: ",
           paste(conflicting, collapse = ", "), ". Keep one row per topic.")
    }
    warning("Coding sheet repeats topic(s) ", paste(dup_topics, collapse = ", "),
            " - duplicate rows dropped. Keep one row per topic.")
    out <- out %>% distinct(topic, .keep_all = TRUE)
  }
  cat(sprintf("  %d topics coded: %d with a name, %d with political, %d with political_subtype\n",
              nrow(out), sum(!is.na(out$topic_name)), sum(!is.na(out$political)),
              sum(!is.na(out$political_subtype))))
  out
}

expand_political <- function(x) {
  key <- str_to_lower(str_trim(x))
  case_when(
    is.na(key)                                         ~ NA_character_,
    key %in% c("explicit", "explicitly political")     ~ "Explicitly political",
    key %in% c("implicit", "implicitly political")     ~ "Implicitly political",
    key %in% c("y", "yes", "political")                ~ "Political",
    key %in% c("n", "no", "non-political", "not political") ~ "Non-political",
    key %in% c("m", "mixed")                           ~ "Mixed",
    TRUE                                               ~ str_trim(x)
  )
}

coding <- read_topic_coding(DESC, K)

topic_lookup <- topics %>%
  group_by(topic) %>%
  summarise(topic_terms = first(topic_terms), .groups = "drop") %>%
  left_join(coding, by = "topic", relationship = "one-to-one") %>%
  mutate(
    # Named topics read "Name (#id)"; uncoded topics fall back to "#id top terms"
    topic_label        = if_else(!is.na(topic_name), paste0(topic_name, " (#", topic, ")"),
                                 paste0("#", topic, " ", topic_terms)),
    political_group    = expand_political(political),
    political_category = if_else(!is.na(political_subtype) & !is.na(political_group),
                                 paste0(political_group, ": ", political_subtype),
                                 political_group)
  )

# Colours for political subtypes: one distinct colour per subtype. Known subtypes keep fixed
# colours across charts; any new subtype gets the next spare colour; uncoded = grey.
SUBTYPE_COLOURS <- c("Actors and institutions" = "#B2182B",
                     "Identity-based"          = "#7B3294",
                     "Policy issues"           = "#2166AC",
                     "Policy issue"            = "#2166AC",
                     "Moral and lifestyle"     = "#1B7837",
                     "Public figures"          = "#E08214",
                     "Mixed"                   = "#8C510A")
SPARE_COLOURS <- c("#35978F", "#C51B7D", "#DFC27D", "#4D4D4D", "#80CDC1")

subtype_palette <- function(subtypes) {
  s   <- sort(unique(na.omit(setdiff(subtypes, "Not coded"))))
  out <- unname(SUBTYPE_COLOURS[s])
  new <- which(is.na(out))
  out[new] <- SPARE_COLOURS[seq_along(new)]
  c(setNames(out, s), "Not coded" = "#BDBDBD")
}

PANEL_LEVELS <- c("Explicitly political", "Implicitly political", "Political", "Mixed",
                  "Non-political", "Not coded")

# Horizontal bar chart of topics. Once coded: bars coloured by political subtype and split
# into panels by the political column (explicit / implicit). Otherwise one neutral colour.
plot_topic_bars <- function(df, value, pct, title, x_lab, extra_caption = "") {
  coded <- any(!is.na(df$political_group)) || any(!is.na(df$political_subtype))
  d <- df %>%
    mutate(subtype = coalesce(political_subtype, "Not coded"),
           panel   = factor(coalesce(political_group, "Not coded"),
                            levels = union(PANEL_LEVELS, unique(coalesce(political_group, "Not coded"))))) %>%
    arrange({{ value }}) %>%
    mutate(topic_label = factor(topic_label, levels = unique(topic_label)))
  
  bars <- if (coded) geom_col(aes(fill = subtype), width = 0.7) else geom_col(fill = col_neutral, width = 0.7)
  
  p <- ggplot(d, aes(y = topic_label, x = {{ value }})) +
    bars +
    geom_text(aes(label = paste0(fmt({{ value }}), " (", round({{ pct }}, 1), "%)")), hjust = -0.1, size = 3) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.25)), labels = comma) +
    labs(title = title, x = x_lab, y = NULL, caption = paste0(cap, extra_caption)) +
    base_theme
  
  if (coded) {
    p <- p +
      facet_grid(panel ~ ., scales = "free_y", space = "free_y") +
      scale_fill_manual(values = subtype_palette(d$subtype), name = "Political subtype") +
      theme(legend.position = "top", legend.title = element_text(size = 9, face = "bold"),
            strip.text.y = element_text(angle = 0, face = "bold", size = 9),
            strip.background = element_rect(fill = "#F0F0F0", colour = NA))
  }
  p
}

HAS_POLITICAL <- any(!is.na(topic_lookup$political_group))

ce  <- readRDS(file.path(PREPPED, "comments_enriched.rds")) %>% filter(has_post_id)
txt <- readRDS(TEXT_FILE)

ce_topic <- ce %>%
  inner_join(topics %>% select(post_id, topic), by = "post_id") %>%
  left_join(topic_lookup %>% select(topic, topic_label, political_group, political_category), by = "topic", relationship = "many-to-one")

cap <- paste0("Sample: ", SAMPLE_LABEL, ". Topics: k-means clusters of post title slugs (k = ", K, "). ")

cat(sprintf("\nThreads with post ID: %s | with a topic: %s | pairs with a topic: %s\n",
            fmt(n_distinct(ce$post_id)), fmt(n_distinct(ce_topic$post_id)),
            fmt(n_distinct(ce_topic$pair_key))))


# === SECTION 1: TOPIC SIZES ===

cat("\n=== SECTION 1: TOPIC SIZES ===\n")

topic_sizes <- ce_topic %>%
  group_by(topic, topic_label, political_group, political_category) %>%
  summarise(
    n_threads  = n_distinct(post_id),
    n_pairs    = n_distinct(pair_key),
    n_paths    = n_distinct(conversation_id),
    n_comments = n(),
    n_users    = n_distinct(speaker[speaker != "[deleted]"]),
    .groups = "drop"
  ) %>%
  mutate(
    pct_threads        = round(100 * n_threads / sum(n_threads), 2),
    pct_pairs          = round(100 * n_pairs / sum(n_pairs), 2),
    pairs_per_thread   = round(n_pairs / n_threads, 2),
    comments_per_path  = round(n_comments / n_paths, 2)
  ) %>%
  arrange(desc(n_pairs)) %>%
  left_join(topic_lookup %>% select(topic, topic_name, political_subtype),
            by = "topic", relationship = "one-to-one") %>%
  relocate(topic_name, political_subtype, .after = topic_label)

save_csv(topic_sizes, "5b_1_topic_sizes")

TOPIC_ORDER <- topic_sizes$topic_label          # largest first

p_sizes <- plot_topic_bars(topic_sizes, n_pairs, pct_pairs,
                           "What the corpus is about: pairs by thread topic", "Pairs",
                           "\nA thread can contribute several pairs; thread counts are in the table and chart 1b.")

save_fig(p_sizes, "5b_1_pairs_by_topic", width = 11, height = 0.4 * nrow(topic_sizes) + 3)

# -- 1b. Threads per topic cluster, with names and political coding --
# Second version of topic_sizes_k<K>.png from 5_topics.py (which is kept as it is).
HAS_NAMES <- any(!is.na(topic_lookup$topic_name))

if (!HAS_NAMES) {
  cat("1b skipped: fill in topic_name in the coding sheet.\n")
} else {
  p_named <- plot_topic_bars(topic_sizes, n_threads, pct_threads,
                             "Threads per topic cluster", "Threads",
                             "\nTopic names and political coding assigned by hand from each cluster's titles. Percentages are of threads with a usable title.")
  
  save_fig(p_named, paste0("5b_1b_threads_by_topic_named_k", K),
           width = 11, height = 0.4 * nrow(topic_sizes) + 3)
}


# === SECTION 2: TOPICS OVER TIME ===

cat("\n=== SECTION 2: TOPICS OVER TIME ===\n")

topics_by_year <- topics %>%
  left_join(topic_lookup %>% select(topic, topic_label), by = "topic", relationship = "many-to-one") %>%
  count(year, topic_label, name = "n_threads") %>%
  complete(year, topic_label, fill = list(n_threads = 0L)) %>%
  group_by(year) %>%
  mutate(pct_of_year = round(100 * n_threads / sum(n_threads), 2)) %>%
  ungroup()

save_csv(topics_by_year, "5b_2_topics_by_year")

year_n <- topics %>% count(year, name = "n")

p_years <- topics_by_year %>%
  mutate(topic_label = factor(topic_label, levels = rev(TOPIC_ORDER)),
         year_lab = factor(paste0(year, "\n(n=", year_n$n[match(year, year_n$year)], ")"))) %>%
  ggplot(aes(x = year_lab, y = topic_label, fill = pct_of_year)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(pct_of_year, 0)), size = 2.8) +
  scale_fill_gradient(low = "#F7FBFF", high = "#2171B5", name = "% of year's threads") +
  labs(title = "Topic shares by year",
       subtitle = "Cells = % of that year's threads in each topic (columns sum to 100)",
       x = NULL, y = NULL,
       caption = paste0(cap, "Year = start of the thread's first sampled path. 2017 covers Aug-Dec only.")) +
  base_theme +
  theme(legend.position = "right", legend.title = element_text(size = 8))

save_fig(p_years, "5b_2_topics_by_year", width = 11, height = 0.35 * nrow(topic_sizes) + 2.5)


# === SECTION 3: WITHIN-PAIR DIFFERENCES BY TOPIC ===

cat("\n=== SECTION 3: WITHIN-PAIR DIFFERENCES BY TOPIC ===\n")

paths <- ce_topic %>%
  arrange(conversation_id, thread_depth) %>%
  group_by(conversation_id) %>%
  summarise(
    pair_key           = first(pair_key),
    post_id            = first(post_id),
    outcome            = if_else(first(has_removed_comment), "awry", "ontrack"),
    n_comments         = n(),
    n_speakers         = n_distinct(speaker),
    start_ts           = first(timestamp),
    challenger_returns = sum(speaker == first(speaker)) > 1,
    .groups = "drop"
  ) %>%
  left_join(txt %>%
              filter(position == "Final comment") %>%
              select(conversation_id, final_words_own = words_own, final_has_quote = has_quote),
            by = "conversation_id")

pairs_wide <- paths %>%
  pivot_wider(id_cols = c(pair_key, post_id), names_from = outcome,
              values_from = c(n_comments, n_speakers, start_ts, challenger_returns,
                              final_words_own, final_has_quote)) %>%
  filter(!is.na(n_comments_awry), !is.na(n_comments_ontrack))

MEASURES <- c("Path length (comments)", "Speakers per path", "Final comment: own words",
              "Final comment quotes (pct points)", "Challenger returns (pct points)",
              "Awry path started first (% of pairs)")

pair_long <- pairs_wide %>%
  transmute(
    pair_key, post_id,
    `Path length (comments)`               = n_comments_awry - n_comments_ontrack,
    `Speakers per path`                    = n_speakers_awry - n_speakers_ontrack,
    `Final comment: own words`             = final_words_own_awry - final_words_own_ontrack,
    `Final comment quotes (pct points)`    = 100 * (final_has_quote_awry - final_has_quote_ontrack),
    `Challenger returns (pct points)`      = 100 * (challenger_returns_awry - challenger_returns_ontrack),
    `Awry path started first (% of pairs)` = 100 * (start_ts_awry < start_ts_ontrack)
  ) %>%
  pivot_longer(-c(pair_key, post_id), names_to = "measure", values_to = "value") %>%
  mutate(measure = factor(measure, levels = MEASURES)) %>%
  left_join(topics %>% select(post_id, topic), by = "post_id") %>%
  left_join(topic_lookup %>% select(topic, topic_label, political_group, political_category), by = "topic", relationship = "many-to-one")

diff_summary <- function(df, group_col) {
  df %>%
    group_by({{ group_col }}, measure) %>%
    summarise(
      n_pairs = sum(!is.na(value)),
      mean    = mean(value, na.rm = TRUE),
      sd      = sd(value, na.rm = TRUE),
      median  = median(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      se       = sd / sqrt(n_pairs),
      t95      = if_else(n_pairs > 1, qt(0.975, pmax(n_pairs - 1, 1)), NA_real_),
      ci_lower = mean - t95 * se,
      ci_upper = mean + t95 * se,
      ref      = if_else(str_detect(as.character(measure), "started first"), 50, 0)
    ) %>%
    select(-t95)
}

diff_by_topic <- bind_rows(
  pair_long %>% mutate(topic_label = "All pairs") %>% diff_summary(topic_label),
  pair_long %>% diff_summary(topic_label)
) %>%
  mutate(across(c(mean, sd, median, se, ci_lower, ci_upper), ~ round(.x, 2)))

save_csv(diff_by_topic, "5b_3_within_pair_differences_by_topic")

# group_subtype (optional): named vector group -> political subtype, to colour points by subtype
plot_forest <- function(df, group_levels, title, extra_caption = "", group_subtype = NULL) {
  coloured <- !is.null(group_subtype) && any(!is.na(group_subtype))
  d <- df %>%
    mutate(colour_key = if (coloured) {
      if_else(group == "All pairs", "All pairs",
              coalesce(unname(group_subtype[as.character(group)]), "Not coded"))
    } else {
      if_else(group == "All pairs", "All pairs", "Topic")
    },
    group = factor(group, levels = rev(group_levels)))
  
  colour_values <- if (coloured) {
    c("All pairs" = "#000000", subtype_palette(d$colour_key[d$colour_key != "All pairs"]))
  } else {
    c("All pairs" = "#000000", "Topic" = col_neutral)
  }
  
  d %>%
    ggplot(aes(y = group, x = mean, xmin = ci_lower, xmax = ci_upper, colour = colour_key)) +
    geom_vline(aes(xintercept = ref), linetype = "dashed", colour = "#999999") +
    geom_pointrange(size = 0.3) +
    facet_wrap(~ measure, scales = "free_x", ncol = 3) +
    scale_colour_manual(values = colour_values,
                        breaks = setdiff(names(colour_values), "All pairs"),
                        name = "Political subtype",
                        guide = if (coloured) "legend" else "none") +
    labs(title = title,
         subtitle = "Awry minus on-track, per pair; points = mean, lines = 95% CI; dashed line = no difference",
         x = NULL, y = NULL,
         caption = paste0(cap, extra_caption,
                          "\nWith many groups, about 1 interval in 20 will miss the dashed line by chance. ",
                          "Timing panel: % of pairs where the awry path's first reply came first (50 = no difference).")) +
    base_theme +
    theme(strip.text = element_text(face = "bold", size = 9),
          axis.text.y = element_text(size = 8),
          legend.position = "top",
          legend.title = element_text(size = 9, face = "bold"))
}

p_forest_topic <- diff_by_topic %>%
  rename(group = topic_label) %>%
  plot_forest(c("All pairs", TOPIC_ORDER), "Do within-pair differences vary by topic?",
              group_subtype = setNames(topic_lookup$political_subtype, topic_lookup$topic_label))

save_fig(p_forest_topic, "5b_3_within_pair_differences_by_topic",
         width = 13, height = 0.45 * length(TOPIC_ORDER) + 5)


# === SECTION 4: WITHIN-PAIR DIFFERENCES BY POLITICAL CODING ===

cat("\n=== SECTION 4: POLITICAL CODING ===\n")

if (!HAS_POLITICAL) {
  cat("Skipped: fill in the 'political' column (e.g. Explicit / Implicit) in the coding sheet.\n")
} else {
  # Runs at two levels: the political column alone, and political + subtype (if subtypes are coded)
  levels_to_run <- list(group = "political_group")
  if (any(!is.na(topic_lookup$political_subtype))) levels_to_run$category <- "political_category"
  
  for (lvl in names(levels_to_run)) {
    col <- levels_to_run[[lvl]]
    
    political_sizes <- topic_sizes %>%
      mutate(grp = coalesce(.data[[col]], "Not coded")) %>%
      group_by(grp) %>%
      summarise(n_topics = n(), across(c(n_threads, n_pairs, n_paths, n_comments), sum), .groups = "drop") %>%
      mutate(pct_threads = round(100 * n_threads / sum(n_threads), 2),
             pct_pairs   = round(100 * n_pairs / sum(n_pairs), 2)) %>%
      arrange(desc(n_threads)) %>%
      rename(!!col := grp)
    
    save_csv(political_sizes, paste0("5b_4_topic_sizes_by_", col))
    
    diff_by_political <- bind_rows(
      pair_long %>% mutate(group = "All pairs") %>% diff_summary(group),
      pair_long %>% mutate(group = coalesce(.data[[col]], "Not coded")) %>% diff_summary(group)
    ) %>%
      mutate(across(c(mean, sd, median, se, ci_lower, ci_upper), ~ round(.x, 2)))
    
    save_csv(diff_by_political, paste0("5b_4_within_pair_differences_by_", col))
    
    group_order <- c("All pairs", political_sizes[[col]])
    p_forest_pol <- diff_by_political %>%
      plot_forest(group_order,
                  if (lvl == "group") "Within-pair differences by political coding"
                  else "Within-pair differences by political coding and subtype",
                  "Political coding is by topic cluster (topic_coding sheet).")
    
    save_fig(p_forest_pol, paste0("5b_4_within_pair_differences_by_", col),
             width = 13, height = 0.45 * length(group_order) + 5)
  }
}


# === SECTION 5: WHERE HIGH-REMOVAL USERS POST ===

cat("\n=== SECTION 5: WHERE HIGH-REMOVAL USERS POST ===\n")

# Do users who are removed a lot post in different kinds of threads? For each user: the share
# of the threads they posted in that fall in each political category. Threads, not comments,
# so one long exchange does not dominate. Groups come from the user table (script 3):
#   Whole sample: 3+ removed comments vs 1-2 vs none. Removal count rises with activity
#      (3+ authors write far more), so this comparison also reflects volume.
#   Users with 25+ comments (removal_rate_eligible in script 3): removal rate over
#      HIGH_RATE_CUT vs at or below it. Rates are stable enough to compare at this volume.
# Breakdowns: political coding (explicit / implicit); subtypes and paired subtypes
# (exploratory: few users per cell).
HIGH_RATE_CUT <- 0.10

# Paired subtypes: matched on the subtype text, so spelling variants are covered
pair_subtype <- function(x) {
  key <- str_to_lower(coalesce(x, ""))
  case_when(
    str_detect(key, "policy|moral|lifestyle")             ~ "Policy issues + moral and lifestyle",
    str_detect(key, "identity|actor|institution|figure")  ~ "Identity + actors and institutions",
    key == ""                                             ~ "Not coded",
    TRUE                                                  ~ "Other"
  )
}

if (!HAS_POLITICAL) {
  cat("Skipped: fill in the 'political' column in the coding sheet.\n")
} else if (!file.exists(USER_FILE)) {
  cat("Skipped: run 3_descriptives_users.R first (", USER_FILE, " not found).\n")
} else {
  user_table <- read_csv(USER_FILE, col_types = cols(speaker = col_character(), .default = col_guess()))
  if (!all(c("removal_count_group", "removal_rate_eligible") %in% names(user_table))) {
    stop(USER_FILE, " has no removal groups: re-run the updated 3_descriptives_users.R first.")
  }
  min_comments_rate <- min(user_table$n_comments[user_table$removal_rate_eligible])
  n_users_all   <- nrow(user_table)
  n_removed_all <- sum(user_table$n_removed_comments)
  n_comments_all <- sum(user_table$n_comments)
  
  # -- Group definitions, labelled with their size --
  count_levels <- c("3+ removed", "1-2 removed", "None removed")
  rate_levels  <- c(sprintf("Over %.0f%% removed", 100 * HIGH_RATE_CUT),
                    sprintf("%.0f%% or less removed", 100 * HIGH_RATE_CUT))
  
  # Comparison names (shown in the chart subtitles)
  COMP_COUNT <- "Whole sample, by removed comments"
  COMP_RATE  <- paste0("Users with ", min_comments_rate, "+ comments, by removal rate")
  
  user_groups <- bind_rows(
    user_table %>%
      transmute(speaker, n_comments, n_removed_comments,
                comparison = COMP_COUNT,
                group = as.character(removal_count_group)),
    user_table %>%
      filter(removal_rate_eligible) %>%
      transmute(speaker, n_comments, n_removed_comments,
                comparison = COMP_RATE,
                group = if_else(removal_rate > HIGH_RATE_CUT, rate_levels[1], rate_levels[2]))
  )
  
  group_sizes <- user_groups %>%
    group_by(comparison, group) %>%
    summarise(n_users = n(), median_comments = median(n_comments),
              n_comments = sum(n_comments), n_removed = sum(n_removed_comments), .groups = "drop") %>%
    group_by(comparison) %>%
    mutate(pct_of_comparison_users = round(100 * n_users / sum(n_users), 1)) %>%
    ungroup() %>%
    mutate(
      pct_of_all_users        = round(100 * n_users / n_users_all, 1),
      pct_of_all_comments     = round(100 * n_comments / n_comments_all, 1),
      pct_of_removed_comments = round(100 * n_removed / n_removed_all, 1),
      avg_removal_rate_pct    = round(100 * n_removed / n_comments, 1),
      # Legend label: n, and share of all users (removal count) or of the comparison's users (rate)
      group_label = if_else(
        comparison == COMP_COUNT,
        sprintf("%s (n = %s; %.1f%% of users, %.0f%% of removals)", group, comma(n_users),
                pct_of_all_users, pct_of_removed_comments),
        sprintf("%s (n = %s; %.0f%% of %d+ comment users)", group, comma(n_users),
                pct_of_comparison_users, min_comments_rate)
      )
    )
  
  save_csv(group_sizes, "5b_5_user_groups")
  
  # -- Each user's threads, with the thread's political coding --
  subtype_lookup <- topic_lookup %>% select(topic, political_subtype)
  
  user_threads <- ce_topic %>%
    filter(speaker != "[deleted]") %>%
    distinct(speaker, post_id, topic, political_group) %>%
    left_join(subtype_lookup, by = "topic", relationship = "many-to-one") %>%
    mutate(
      `Political coding`                   = coalesce(political_group, "Not coded"),
      `Subtype (exploratory)`              = coalesce(political_subtype, "Not coded"),
      `Paired subtypes (exploratory)`      = pair_subtype(political_subtype)
    ) %>%
    select(speaker, post_id, `Political coding`, `Subtype (exploratory)`, `Paired subtypes (exploratory)`) %>%
    pivot_longer(-c(speaker, post_id), names_to = "breakdown", values_to = "category")
  
  BREAKDOWNS <- c("Political coding", "Subtype (exploratory)", "Paired subtypes (exploratory)")
  
  # Share of each user's threads in each category (zeros filled in)
  user_shares <- user_threads %>%
    count(speaker, breakdown, category, name = "n_threads") %>%
    group_by(breakdown) %>%
    complete(speaker, category, fill = list(n_threads = 0L)) %>%
    group_by(speaker, breakdown) %>%
    mutate(user_threads = sum(n_threads), share = n_threads / user_threads) %>%
    ungroup() %>%
    filter(user_threads > 0)
  
  group_shares <- user_groups %>%
    inner_join(user_shares, by = "speaker", relationship = "many-to-many") %>%
    group_by(comparison, group, breakdown, category) %>%
    summarise(
      n_users      = n(),
      mean_share   = 100 * mean(share),
      sd           = 100 * sd(share),
      pooled_share = 100 * sum(n_threads) / sum(user_threads),
      .groups = "drop"
    ) %>%
    mutate(
      se       = sd / sqrt(n_users),
      ci_lower = mean_share - qt(0.975, pmax(n_users - 1, 1)) * se,
      ci_upper = mean_share + qt(0.975, pmax(n_users - 1, 1)) * se,
      across(c(mean_share, sd, pooled_share, se, ci_lower, ci_upper), ~ round(.x, 1))
    ) %>%
    left_join(group_sizes %>% select(comparison, group, group_label), by = c("comparison", "group")) %>%
    mutate(breakdown = factor(breakdown, levels = BREAKDOWNS)) %>%
    arrange(comparison, breakdown, category, group)
  
  save_csv(group_shares, "5b_5_thread_shares_by_user_group")
  
  # -- Charts: one per comparison, a panel per breakdown --
  plot_user_groups <- function(comp, group_levels, colours, file_name, extra_caption) {
    d <- group_shares %>%
      filter(comparison == comp, category != "Not coded") %>%
      mutate(group = factor(group, levels = group_levels)) %>%
      arrange(group) %>%
      mutate(group_label = factor(group_label, levels = unique(group_label)))
    label_colours <- setNames(colours[as.character(levels(droplevels(d$group)))],
                              levels(d$group_label))
    
    p <- d %>%
      ggplot(aes(x = str_wrap(category, 16), y = mean_share, fill = group_label)) +
      geom_col(position = position_dodge(width = 0.85), width = 0.8) +
      geom_errorbar(aes(ymin = pmax(ci_lower, 0), ymax = ci_upper),
                    position = position_dodge(width = 0.85), width = 0.25, colour = "#333333") +
      geom_text(aes(label = round(mean_share), y = ci_upper), position = position_dodge(width = 0.85),
                vjust = -0.5, size = 2.8) +
      facet_grid(. ~ breakdown, scales = "free_x", space = "free_x") +
      scale_fill_manual(values = label_colours) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                         labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
      coord_cartesian(clip = "off") +
      labs(title = "Where do high-removal users post?",
           subtitle = paste0(comp, ". Bars = mean share of each user's threads; lines = 95% CI"),
           x = NULL, y = "% of the user's threads",
           caption = paste0(cap, "\nShares are of the threads each user posted in (each thread counted once per user). ",
                            "Within each panel a user's shares sum to 100%. [deleted] excluded.\n", extra_caption)) +
      base_theme +
      guides(fill = guide_legend(ncol = 1)) +
      theme(legend.position = "top",
            strip.text = element_text(face = "bold", size = 9),
            strip.background = element_rect(fill = "#F0F0F0", colour = NA),
            axis.text.x = element_text(size = 8))
    save_fig(p, file_name, width = 13, height = 6.5)
  }
  
  plot_user_groups(COMP_COUNT, count_levels,
                   c("3+ removed" = "#EF5350", "1-2 removed" = "#F4A6A6", "None removed" = "#BDBDBD"),
                   "5b_5a_thread_shares_by_removal_count",
                   "3+ authors write far more comments than other users, so this comparison also reflects activity. Subtype panels are exploratory.")
  
  plot_user_groups(COMP_RATE, rate_levels,
                   setNames(c("#EF5350", "#5C85D6"), rate_levels),
                   "5b_5b_thread_shares_by_removal_rate",
                   paste0("Removal rate = removed comments / comments written (script 3). Users with ", min_comments_rate,
                          "+ comments only. Subtype panels are exploratory."))
}

cat("\nDone.\n")