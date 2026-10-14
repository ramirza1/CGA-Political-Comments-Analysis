# 6. descriptives_networks.R for Conversations Gone Awry (CMV) - Political Comments Analysis
#
# Networks of users and topics.
#
#   1. Reply network (users): who replies to whom. Edges run from the replier to the author
#      of the comment replied to; an edge is flagged when the reply was removed.
#      Main question: do removals concentrate in repeat encounters between the same people?
#        - across threads: had the two users met in an earlier thread?
#        - within the path: had they already exchanged replies earlier in the same path?
#      The removed comment is compared with the on-track path's final comment (same position,
#      other outcome) and with all other replies.
#   2. Users x topics (needs 5_topics.py): an edgelist of users and the topics they comment
#      in, and how specialised users are (share of their threads in their main topic),
#      compared with a baseline where thread topics are shuffled.
#   3. Topic x topic: two topics are linked by the users who comment in both. Each thread has
#      one topic, so topics only connect through people. Observed shared users are compared
#      with the shuffled baseline, otherwise the most active users link everything.
#
# What the data can and cannot show:
#   - Paths are sampled from threads, so "met before" means met in another sampled path.
#     Encounters outside the corpus are invisible; prior-encounter rates are lower bounds.
#   - More active users have more chances to meet anyone again. Removed-comment authors are
#     more active on average (script 3b), so section 1 is also shown by author activity group.
#   - [deleted] accounts are excluded from both ends of every edge; self-replies are dropped.
#   - First replies are not edges: they reply to the post, whose author is not in the data.
#
# Sample: paths with a post ID only (threads are identified by post ID).
#
# Outputs:
#   descriptive_results/networks/*.csv
#   graphical_output/networks/*.png
#
# Run after 1_data_prep.R and 1b_pseudonymize_users.R (sections 2-3 also need 5_topics.py)


# === SECTION 0: PREP ===

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggpubr, scales, igraph, Matrix)

PREPPED     <- "prepped_data_anonymized"
TOPICS_FILE <- file.path(PREPPED, "thread_topics.csv")
DESC        <- file.path("descriptive_results", "networks")
FIGS        <- file.path("graphical_output", "networks")
dir.create(DESC, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGS, recursive = TRUE, showWarnings = FALSE)

SAMPLE_LABEL <- "Paths with post ID (Aug 2017-Dec 2022)"

MIN_THREADS_SPEC <- 3     # users need at least this many threads for specialisation (section 2)
N_PERM           <- 200   # shuffles of thread topics for the baseline (sections 2-3)
set.seed(2026)

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

col_neutral  <- "#5C85D6"
col_expected <- "#BDBDBD"
col_role <- c("Removed comment"        = "#EF5350",
              "On-track final comment" = "#81C784",
              "Other replies"          = "#9E9E9E")
ROLE_LEVELS <- names(col_role)

col_groups <- c("One comment"                     = "#B3CDE3",
                "Other (2+ comments, bottom 95%)" = "#6BAED6",
                "Top 5% (excl. top 1%)"           = "#CD7F32",
                "Top 1%"                          = "#B8860B")
ACTIVITY_LEVELS <- names(col_groups)

scale_y_80 <- function(...) scale_y_continuous(..., expand = expansion(mult = c(0, 0.25)))
scale_y_pct <- function(...) {
  list(scale_y_continuous(..., limits = c(0, 100), breaks = seq(0, 100, 20), expand = c(0, 0)),
       coord_cartesian(clip = "off"))
}

wilson_lower <- function(x, n, z = 1.96) {
  p <- x / n
  100 * ((p + z^2 / (2 * n)) - z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / (1 + z^2 / n)
}
wilson_upper <- function(x, n, z = 1.96) {
  p <- x / n
  100 * ((p + z^2 / (2 * n)) + z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / (1 + z^2 / n)
}

cap <- paste0("Sample: ", SAMPLE_LABEL, ". [deleted] accounts and self-replies excluded. ")

# -- Load --
ce <- readRDS(file.path(PREPPED, "comments_enriched.rds")) %>%
  filter(has_post_id) %>%
  arrange(conversation_id, thread_depth) %>%
  group_by(conversation_id) %>%
  mutate(
    parent_speaker = lag(speaker),
    is_final       = row_number() == n()
  ) %>%
  ungroup() %>%
  mutate(role = case_when(
    is_final & has_removed_comment ~ "Removed comment",
    is_final                       ~ "On-track final comment",
    TRUE                           ~ "Other replies"
  ) %>% factor(levels = ROLE_LEVELS))

# Author activity groups (as in script 3), from comment counts in this sample
user_counts <- ce %>% filter(speaker != "[deleted]") %>% count(speaker, name = "n_comments")
q95 <- quantile(user_counts$n_comments, 0.95)
q99 <- quantile(user_counts$n_comments, 0.99)
user_counts <- user_counts %>%
  mutate(activity_group = case_when(
    n_comments >= q99 ~ "Top 1%",
    n_comments >= q95 ~ "Top 5% (excl. top 1%)",
    n_comments == 1   ~ "One comment",
    TRUE              ~ "Other (2+ comments, bottom 95%)"
  ) %>% factor(levels = ACTIVITY_LEVELS))

removal_authors <- ce %>% filter(is_removed_comment, speaker != "[deleted]") %>% distinct(speaker) %>% pull()


# === SECTION 1: REPLY NETWORK ===

cat("\n=== SECTION 1: REPLY NETWORK ===\n")

replies_all <- ce %>% filter(thread_depth > 0, !is.na(parent_speaker))

replies <- replies_all %>%
  filter(speaker != "[deleted]", parent_speaker != "[deleted]", speaker != parent_speaker) %>%
  mutate(dyad = if_else(speaker < parent_speaker,
                        paste(speaker, parent_speaker, sep = "|"),
                        paste(parent_speaker, speaker, sep = "|")))

# -- 1a. Overview --
edges_dir <- replies %>%
  count(from = speaker, to = parent_speaker, name = "n_replies")

g <- graph_from_data_frame(edges_dir, directed = TRUE)
comp <- components(g, mode = "weak")

network_overview <- tibble(
  Measure = c("Replies (comments at depth 1+)",
              "  excluded: replier or replied-to is [deleted]",
              "  excluded: self-replies",
              "Replies in the network",
              "Users (nodes)",
              "Directed ties (user A replied to user B at least once)",
              "Density (share of possible directed ties present)",
              "Reciprocity (share of ties that run both ways)",
              "Weakly connected components",
              "% of users in the largest component",
              "Removed comments in the network",
              "Removed comments excluded ([deleted] or self-reply)"),
  Value = c(nrow(replies_all),
            sum(replies_all$speaker == "[deleted]" | replies_all$parent_speaker == "[deleted]"),
            sum(replies_all$speaker != "[deleted]" & replies_all$speaker == replies_all$parent_speaker),
            nrow(replies),
            vcount(g),
            ecount(g),
            edge_density(g),
            reciprocity(g),
            comp$no,
            100 * max(comp$csize) / vcount(g),
            sum(replies$is_removed_comment),
            sum(replies_all$is_removed_comment) - sum(replies$is_removed_comment))
) %>%
  mutate(Value = round(Value, 3))

save_csv(network_overview, "1a_reply_network_overview")

# How many distinct users each user replies to / is replied to by
degree_table <- tibble(
  speaker    = V(g)$name,
  out_degree = degree(g, mode = "out"),
  in_degree  = degree(g, mode = "in")
) %>%
  pivot_longer(c(out_degree, in_degree), names_to = "direction", values_to = "n_users") %>%
  mutate(direction = recode(direction, out_degree = "Users replied to", in_degree = "Users replied by")) %>%
  count(direction, n_users, name = "n_people") %>%
  group_by(direction) %>%
  mutate(pct = round(100 * n_people / sum(n_people), 2),
         cum_pct = round(100 * cumsum(n_people) / sum(n_people), 2)) %>%
  ungroup()

save_csv(degree_table, "1a_degree_distribution_uncapped")

# -- 1b. Dyads: how often do the same two users meet? --
dyad_threads <- replies %>%
  group_by(dyad, post_id) %>%
  summarise(first_ts = min(timestamp), n_replies = n(), n_removed = sum(is_removed_comment),
            .groups = "drop")

dyads <- replies %>%
  group_by(dyad) %>%
  summarise(
    n_replies = n(),
    n_paths   = n_distinct(conversation_id),
    n_threads = n_distinct(post_id),
    n_removed = sum(is_removed_comment),
    .groups = "drop"
  )

dyad_repeat_uncapped <- dyads %>%
  count(n_threads, name = "n_dyads") %>%
  mutate(pct = round(100 * n_dyads / sum(n_dyads), 3),
         cum_pct = round(100 * cumsum(n_dyads) / sum(n_dyads), 3))

save_csv(dyad_repeat_uncapped, "1b_dyads_by_threads_together_uncapped")

p_dyads <- dyads %>%
  mutate(threads = factor(if_else(n_threads >= 5, "5+", as.character(n_threads)),
                          levels = c("1", "2", "3", "4", "5+"))) %>%
  count(threads, name = "n_dyads") %>%
  mutate(pct = 100 * n_dyads / sum(n_dyads)) %>%
  ggplot(aes(x = threads, y = n_dyads)) +
  geom_col(fill = col_neutral, width = 0.7) +
  geom_text(aes(label = paste0(fmt(n_dyads), "\n(", round(pct, 1), "%)")), vjust = -0.3, size = 3) +
  scale_y_80(labels = comma) +
  labs(title = "How often do the same two users meet?",
       subtitle = "Pairs of users who replied to each other, by number of threads they did so in",
       x = "Threads in which the two users exchanged replies", y = "Pairs of users",
       caption = paste0(cap, "\nOnly sampled paths are observed (two per pair), so repeat meetings elsewhere on CMV are invisible: this is a lower bound.")) +
  base_theme

save_fig(p_dyads, "1b_dyads_by_threads_together", width = 8, height = 5)

# -- 1c. Removals and repeat encounters --

# Across threads: number of OTHER threads where the two users had exchanged replies
# before this reply was written
prior_threads <- replies %>%
  select(comment_id, dyad, post_id, timestamp) %>%
  left_join(dyad_threads %>% select(dyad, other_post = post_id, other_first_ts = first_ts),
            by = "dyad", relationship = "many-to-many") %>%
  group_by(comment_id) %>%
  summarise(prior_threads = sum(other_post != post_id & other_first_ts < timestamp), .groups = "drop")

# Within the path: replies between the same two users earlier in this path
replies <- replies %>%
  left_join(prior_threads, by = "comment_id") %>%
  group_by(conversation_id, dyad) %>%
  mutate(prior_in_path = row_number() - 1L) %>%          # replies are in depth order
  ungroup() %>%
  left_join(user_counts %>% select(speaker, activity_group), by = "speaker")

encounter_summary <- function(df, ...) {
  df %>%
    group_by(...) %>%
    summarise(
      n_replies                 = n(),
      n_met_before              = sum(prior_threads > 0),
      mean_prior_threads        = mean(prior_threads),
      n_exchanged_earlier_path  = sum(prior_in_path > 0),
      mean_prior_in_path        = mean(prior_in_path),
      .groups = "drop"
    ) %>%
    mutate(
      pct_met_before          = 100 * n_met_before / n_replies,
      pct_met_before_lower    = wilson_lower(n_met_before, n_replies),
      pct_met_before_upper    = wilson_upper(n_met_before, n_replies),
      pct_exchanged_earlier   = 100 * n_exchanged_earlier_path / n_replies,
      pct_exchanged_lower     = wilson_lower(n_exchanged_earlier_path, n_replies),
      pct_exchanged_upper     = wilson_upper(n_exchanged_earlier_path, n_replies)
    ) %>%
    mutate(across(where(is.double), ~ round(.x, 2)))
}

encounters_by_role <- encounter_summary(replies, role)
save_csv(encounters_by_role, "1c_repeat_encounters_by_role")

encounters_by_role_activity <- encounter_summary(replies, activity_group, role)
save_csv(encounters_by_role_activity, "1c_repeat_encounters_by_role_and_author_activity")

# Chart: within-path repeat encounters only. Encounters across threads are ~0% for every
# group (users almost never meet again in the sampled paths, see 1b), so they stay in the
# table above but are left off the chart.
p_encounters <- encounters_by_role %>%
  ggplot(aes(x = role, y = pct_exchanged_earlier, fill = role)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = pct_exchanged_lower, ymax = pct_exchanged_upper),
                width = 0.2, colour = "#333333") +
  geom_text(aes(y = pct_exchanged_upper, label = paste0(round(pct_exchanged_earlier, 1), "%")),
            vjust = -0.6, size = 3.2) +
  scale_fill_manual(values = col_role) +
  scale_y_pct() +
  labs(title = "Is the removed comment aimed at someone the author has already argued with?",
       subtitle = "% of replies where the two users had already exchanged replies earlier in the same path; lines = 95% CI",
       x = NULL, y = "% of replies",
       caption = paste0(cap, "\nRemoved comment vs the on-track path's final comment = same position, other outcome. ",
                        "Final comments sit deeper in the path, which gives more chances for an earlier exchange.\n",
                        "Meetings in earlier threads are ~0% for every group, so are left out (see 1c_repeat_encounters_by_role.csv). ",
                        "By author activity: 1c_repeat_encounters_by_role_and_author_activity.csv.")) +
  base_theme +
  theme(legend.position = "none", axis.text.x = element_text(size = 10))

save_fig(p_encounters, "1c_repeat_encounters_by_role", width = 9, height = 5.5)

# -- 1d. Do removals repeat within the same pair of users? --
repeat_removals <- dyads %>%
  summarise(
    dyads                        = n(),
    dyads_with_a_removal         = sum(n_removed > 0),
    dyads_with_2plus_removals    = sum(n_removed >= 2),
    removed_comments             = sum(n_removed),
    removed_in_2plus_dyads       = sum(n_removed[n_removed >= 2]),
    pct_removed_in_2plus_dyads   = round(100 * removed_in_2plus_dyads / removed_comments, 2)
  ) %>%
  pivot_longer(everything(), names_to = "Measure", values_to = "Value")

save_csv(repeat_removals, "1d_repeat_removals_within_dyads")


# === SECTIONS 2-3: USERS AND TOPICS ===

if (!file.exists(TOPICS_FILE)) {
  cat("\nSections 2-3 skipped: run 5_topics.py first (", TOPICS_FILE, " not found).\n", sep = "")
} else {
  
  cat("\n=== SECTION 2: USERS x TOPICS ===\n")
  
  topics <- read_csv(TOPICS_FILE, col_types = cols(post_id = col_character(), .default = col_guess()))
  K <- first(topics$k)
  # Topic coding: same reader as 5b_descriptives_topics.R (csv or xlsx; values used as entered)
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
  
  coding <- read_topic_coding(file.path("descriptive_results", "topics"), K)
  topic_lookup <- topics %>%
    group_by(topic) %>%
    summarise(topic_terms = first(topic_terms), .groups = "drop") %>%
    left_join(coding, by = "topic", relationship = "one-to-one") %>%
    mutate(topic_label     = if_else(!is.na(topic_name), paste0(topic_name, " (#", topic, ")"),
                                     paste0("#", topic, " ", topic_terms)),
           political_group = expand_political(political))
  HAS_POLITICAL <- any(!is.na(topic_lookup$political_group))
  
  cap_topics <- paste0(cap, "Topics: k-means clusters of post title slugs (k = ", K, "). ")
  
  # -- 2a. User x topic edgelist --
  user_topic_edgelist <- ce %>%
    filter(speaker != "[deleted]") %>%
    inner_join(topics %>% select(post_id, topic), by = "post_id") %>%
    group_by(speaker, topic) %>%
    summarise(n_comments = n(), n_threads = n_distinct(post_id),
              n_removed = sum(is_removed_comment), .groups = "drop") %>%
    left_join(topic_lookup %>% select(topic, topic_label), by = "topic", relationship = "many-to-one")
  
  write_csv(user_topic_edgelist, file.path(DESC, "2a_user_topic_edgelist.csv"))
  cat(sprintf("  -> saved: 2a_user_topic_edgelist.csv (%s user-topic ties, %s users)\n",
              fmt(nrow(user_topic_edgelist)), fmt(n_distinct(user_topic_edgelist$speaker))))
  
  # -- Incidence matrices: users x threads, threads x topics --
  user_threads  <- ce %>%
    filter(speaker != "[deleted]") %>%
    inner_join(topics %>% select(post_id, topic), by = "post_id") %>%
    distinct(speaker, post_id)
  users_vec     <- sort(unique(user_threads$speaker))
  threads_vec   <- sort(unique(user_threads$post_id))
  topic_levels  <- sort(unique(topics$topic))
  thread_topic  <- topics$topic[match(threads_vec, topics$post_id)]
  
  UT <- sparseMatrix(i = match(user_threads$speaker, users_vec),
                     j = match(user_threads$post_id, threads_vec),
                     x = 1, dims = c(length(users_vec), length(threads_vec)))
  onehot <- function(tt) sparseMatrix(i = seq_along(tt), j = match(tt, topic_levels), x = 1,
                                      dims = c(length(tt), length(topic_levels)))
  
  user_n_threads <- rowSums(UT)
  spec_rows  <- which(user_n_threads >= MIN_THREADS_SPEC)   # specialisation (section 2)
  multi_rows <- which(user_n_threads >= 2)                  # topic links (section 3)
  UT_spec  <- UT[spec_rows, , drop = FALSE]
  UT_multi <- UT[multi_rows, , drop = FALSE]
  
  spec_users <- tibble(speaker = users_vec[spec_rows]) %>%
    left_join(user_counts %>% select(speaker, activity_group), by = "speaker") %>%
    mutate(removal_author = if_else(speaker %in% removal_authors,
                                    "Wrote a removed comment", "No removed comment"))
  
  top_share <- function(M) apply(M, 1, max) / rowSums(M)
  n_topics  <- function(M) rowSums(M > 0)
  
  group_means <- function(values) {
    bind_rows(
      tibble(group = "All users (3+ threads)", value = values),
      tibble(group = as.character(spec_users$activity_group), value = values),
      tibble(group = spec_users$removal_author, value = values)
    ) %>%
      group_by(group) %>%
      summarise(mean = mean(value), .groups = "drop")
  }
  
  M_obs <- as.matrix(UT_spec %*% onehot(thread_topic))
  obs_share  <- group_means(top_share(M_obs))  %>% rename(observed_top_share = mean)
  obs_ntop   <- group_means(n_topics(M_obs))   %>% rename(observed_n_topics = mean)
  co_obs <- {
    B <- (as.matrix(UT_multi %*% onehot(thread_topic)) > 0) * 1
    crossprod(B)
  }
  
  # -- Shuffled baseline: thread topics permuted (topic sizes and each user's threads kept) --
  cat(sprintf("Running %d shuffles of thread topics...\n", N_PERM))
  perm_share <- vector("list", N_PERM)
  perm_ntop  <- vector("list", N_PERM)
  co_perm    <- array(NA_real_, dim = c(length(topic_levels), length(topic_levels), N_PERM))
  for (b in seq_len(N_PERM)) {
    tt <- sample(thread_topic)
    M  <- as.matrix(UT_spec %*% onehot(tt))
    perm_share[[b]] <- group_means(top_share(M)) %>% mutate(perm = b)
    perm_ntop[[b]]  <- group_means(n_topics(M))  %>% mutate(perm = b)
    B <- (as.matrix(UT_multi %*% onehot(tt)) > 0) * 1
    co_perm[, , b] <- as.matrix(crossprod(B))
  }
  
  null_summary <- function(lst, prefix) {
    bind_rows(lst) %>%
      group_by(group) %>%
      summarise(!!paste0(prefix, "_null_mean")  := mean(mean),
                !!paste0(prefix, "_null_lower") := quantile(mean, 0.025),
                !!paste0(prefix, "_null_upper") := quantile(mean, 0.975),
                .groups = "drop")
  }
  
  GROUP_LEVELS <- c("All users (3+ threads)", ACTIVITY_LEVELS, "No removed comment", "Wrote a removed comment")
  
  specialisation <- obs_share %>%
    left_join(null_summary(perm_share, "top_share"), by = "group") %>%
    left_join(obs_ntop, by = "group") %>%
    left_join(null_summary(perm_ntop, "n_topics"), by = "group") %>%
    left_join(bind_rows(
      tibble(group = "All users (3+ threads)", n = nrow(spec_users)),
      spec_users %>% count(group = as.character(activity_group)),
      spec_users %>% count(group = removal_author)
    ), by = "group") %>%
    rename(n_users = n) %>%
    mutate(group = factor(group, levels = GROUP_LEVELS)) %>%
    arrange(group) %>%
    mutate(across(c(starts_with("top_share"), observed_top_share), ~ round(100 * .x, 2)),
           across(c(starts_with("n_topics"), observed_n_topics), ~ round(.x, 2)))
  
  save_csv(specialisation, "2b_user_topic_specialisation")
  
  p_spec <- specialisation %>%
    filter(!is.na(n_users), n_users > 0) %>%
    mutate(group = fct_rev(group)) %>%
    ggplot(aes(y = group)) +
    geom_linerange(aes(xmin = top_share_null_lower, xmax = top_share_null_upper),
                   colour = col_expected, linewidth = 4) +
    geom_point(aes(x = top_share_null_mean, shape = "Shuffled topics (95% range)"), colour = "#757575", size = 2) +
    geom_point(aes(x = observed_top_share, shape = "Observed"), colour = col_neutral, size = 3) +
    geom_text(aes(x = observed_top_share, label = paste0(round(observed_top_share, 1), "%")),
              hjust = -0.4, size = 3) +
    scale_shape_manual(values = c("Observed" = 16, "Shuffled topics (95% range)" = 15)) +
    scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    labs(title = "Do users stick to one topic?",
         subtitle = paste0("Mean share of a user's threads in their most frequent topic (users with ",
                           MIN_THREADS_SPEC, "+ threads)"),
         x = "% of the user's threads in their main topic", y = NULL,
         caption = paste0(cap_topics, "\nShuffled: thread topics randomly reassigned ", N_PERM,
                          " times, keeping topic sizes and each user's threads.\n",
                          "Observed to the right of the grey range = more concentrated in one topic than chance. ",
                          "Users with more threads have lower shares by arithmetic, so compare each dot with its own grey range.")) +
    base_theme +
    theme(legend.position = "top")
  
  save_fig(p_spec, "2b_user_topic_specialisation", width = 10, height = 5.5)
  
  # -- 2c. Share of each user's threads in each political category (once coded) --
  if (HAS_POLITICAL) {
    thread_group <- topic_lookup$political_group[match(thread_topic, topic_lookup$topic)]
    political_share <- map_dfr(sort(unique(na.omit(topic_lookup$political_group))), function(cat_name) {
      cat_cols   <- which(topic_levels %in% topic_lookup$topic[topic_lookup$political_group %in% cat_name])
      share_user <- 100 * rowSums(M_obs[, cat_cols, drop = FALSE]) / rowSums(M_obs)
      bind_rows(
        tibble(group = "All users (3+ threads)", value = share_user),
        tibble(group = as.character(spec_users$activity_group), value = share_user),
        tibble(group = spec_users$removal_author, value = share_user)
      ) %>%
        group_by(group) %>%
        summarise(n_users = n(), mean_pct = mean(value), sd = sd(value), .groups = "drop") %>%
        mutate(political_group   = cat_name,
               ci_lower          = mean_pct - qt(0.975, pmax(n_users - 1, 1)) * sd / sqrt(n_users),
               ci_upper          = mean_pct + qt(0.975, pmax(n_users - 1, 1)) * sd / sqrt(n_users),
               corpus_pct_threads = 100 * mean(thread_group %in% cat_name))
    }) %>%
      mutate(group = factor(group, levels = GROUP_LEVELS)) %>%
      arrange(political_group, group) %>%
      relocate(political_group) %>%
      mutate(across(where(is.double), ~ round(.x, 2)))
    
    save_csv(political_share, "2c_user_share_of_threads_by_political_group")
  } else {
    cat("2c skipped: fill in the 'political' column in the topic coding sheet.\n")
  }
  
  
  # === SECTION 3: TOPIC x TOPIC (through shared users) ===
  
  cat("\n=== SECTION 3: TOPIC x TOPIC ===\n")
  
  co_expected <- apply(co_perm, c(1, 2), mean)
  co_p_upper  <- apply(sweep(co_perm, c(1, 2), co_obs, `>=`), c(1, 2), mean)
  users_in_topic <- diag(co_obs)
  
  labels_vec <- topic_lookup$topic_label[match(topic_levels, topic_lookup$topic)]
  
  topic_links <- expand_grid(i = seq_along(topic_levels), j = seq_along(topic_levels)) %>%
    filter(i < j) %>%
    mutate(
      topic_a        = labels_vec[i],
      topic_b        = labels_vec[j],
      shared_users   = co_obs[cbind(i, j)],
      expected_users = co_expected[cbind(i, j)],
      ratio_obs_exp  = shared_users / expected_users,
      jaccard        = shared_users / (users_in_topic[i] + users_in_topic[j] - shared_users),
      share_of_perms_at_least_observed = co_p_upper[cbind(i, j)]
    ) %>%
    arrange(desc(ratio_obs_exp)) %>%
    mutate(across(c(expected_users, ratio_obs_exp, jaccard), ~ round(.x, 3))) %>%
    select(-i, -j)
  
  save_csv(topic_links, "3a_topic_links_shared_users")
  
  ORDER <- labels_vec[order(-users_in_topic)]
  
  p_links <- expand_grid(i = seq_along(topic_levels), j = seq_along(topic_levels)) %>%
    filter(i != j) %>%
    mutate(topic_a = factor(labels_vec[i], levels = ORDER),
           topic_b = factor(labels_vec[j], levels = rev(ORDER)),
           ratio   = co_obs[cbind(i, j)] / co_expected[cbind(i, j)]) %>%
    filter(as.integer(topic_a) < length(ORDER) + 1 - as.integer(topic_b)) %>%   # lower triangle
    ggplot(aes(x = topic_a, y = topic_b, fill = log2(ratio))) +
    geom_tile(colour = "white") +
    geom_text(aes(label = sprintf("%.2f", ratio)), size = 2.5) +
    scale_fill_gradient2(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B", midpoint = 0,
                         name = "Observed / expected\n(log2 colour scale)",
                         labels = function(x) sprintf("%.2f", 2^x)) +
    labs(title = "Which topics share commenters more than chance?",
         subtitle = paste0("Users who comment in both topics, compared with chance (topics randomly reassigned to threads)\n",
                           "1 = as many shared commenters as chance; below 1 = more separate audiences; above 1 = more shared audience"),
         x = NULL, y = NULL,
         caption = paste0(cap_topics, "\nUsers with 2+ threads. Chance = mean over ", N_PERM,
                          " shuffles of thread topics, keeping topic sizes and each user's threads.\n",
                          "Users lean slightly towards their own topics (chart 2b), so most cells fall below 1: compare cells with each other.")) +
    base_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          axis.text.y = element_text(size = 8), legend.position = "right",
          legend.title = element_text(size = 8))
  
  save_fig(p_links, "3a_topic_links_shared_users",
           width = 0.45 * length(topic_levels) + 7, height = 0.4 * length(topic_levels) + 4)
}

cat("\nDone.\n")