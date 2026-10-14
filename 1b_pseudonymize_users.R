# 1b. pseudonymize_users.R for Conversations Gone Awry (CMV) - Political Comments Analysis
#
# Makes shareable copies of the data with Reddit usernames replaced by ranked IDs:
#
#   LOCAL ONLY (usernames, gitignored)      SHAREABLE (IDs, pushed to GitHub)
#   input_data_from_python/*.csv       -->  input_data_anonymized/*.csv
#   prepped_data/*.rds                 -->  prepped_data_anonymized/*.rds
#
# IDs: user_00001 = the user with the most comments in the corpus, user_00002 the next, ...
#      Ties broken alphabetically by username (C locale). [deleted] stays "[deleted]".
#      Ranked on the full corpus, so IDs are stable across samples and re-runs.
#
# Replaced:
#   - Any `speaker` or `speaker_id` column, in every CSV and RDS file
#   - Username mentions inside comment text: u/name, /u/name, .../user/name
#       (known users -> u/user_XXXXX ; other accounts -> u/[user])
# NOT replaced (re-identification is still possible):
#   - Comment text itself, comment/conversation IDs and permalinks link back to Reddit
#   - Usernames written in text without a u/ prefix (counted below, not replaced)
#   This is pseudonymisation, not anonymisation.
#
# The username <-> ID key is written to input_data_from_python/PRIVATE_user_id_key.csv
# (local only). Join on it to trace any ID back to a username.
#
# Run order: Python export -> 1_data_prep.R -> 1b_pseudonymize_users.R -> 2_..., 3_...

# -- 0. Packages --
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)

# -- 1. Folders --
PRIVATE_CSV_DIR <- "input_data_from_python"     # usernames - local only
PRIVATE_RDS_DIR <- "prepped_data"               # usernames - local only
SHARED_CSV_DIR  <- "input_data_anonymized"      # IDs - shareable
SHARED_RDS_DIR  <- "prepped_data_anonymized"    # IDs - shareable
KEY_FILE        <- file.path(PRIVATE_CSV_DIR, "PRIVATE_user_id_key.csv")

ID_PREFIX    <- "user_"
SPEAKER_COLS <- c("speaker", "speaker_id")
TEXT_COLS    <- c("text")

dir.create(SHARED_CSV_DIR, showWarnings = FALSE)
dir.create(SHARED_RDS_DIR, showWarnings = FALSE)

# CSVs are read and written as text so values are copied exactly. Only empty fields
# are treated as missing, so a literal "NA" survives.
read_raw  <- function(path) read_csv(path, col_types = cols(.default = col_character()),
                                     na = "", show_col_types = FALSE, progress = FALSE)
write_raw <- function(df, path) write_csv(df, path, na = "")


# -- 2. Ranked ID key, built from the original comments.csv --
message("Building user ID key ...")
comments_raw <- read_raw(file.path(PRIVATE_CSV_DIR, "comments.csv"))

id_key <- comments_raw %>%
  filter(!is.na(speaker), speaker != "[deleted]") %>%
  count(speaker, name = "n_comments") %>%
  arrange(desc(n_comments), speaker) %>%
  mutate(
    rank    = row_number(),
    user_id = paste0(ID_PREFIX, formatC(rank, width = nchar(n()), flag = "0"))
  )

write_raw(id_key, KEY_FILE)
cat(sprintf("Key: %s users (%s ... %s) -> %s\n",
            format(nrow(id_key), big.mark = ","), first(id_key$user_id), last(id_key$user_id), KEY_FILE))

lookup <- setNames(id_key$user_id, id_key$speaker)

lookup_lower_df <- id_key %>% mutate(lower = tolower(speaker)) %>% distinct(lower, .keep_all = TRUE)
lookup_lower    <- setNames(lookup_lower_df$user_id, lookup_lower_df$lower)   # usernames are case-insensitive

id_pattern <- paste0("^", ID_PREFIX, "\\d+$")

# Usernames -> IDs. Values that are already IDs are kept; anything else unmatched stops the script.
pseudonymize_speaker <- function(x) {
  x   <- as.character(x)
  out <- ifelse(is.na(x) | x == "[deleted]", x, unname(lookup[x]))
  already_id <- is.na(out) & !is.na(x) & str_detect(x, id_pattern)
  out[already_id] <- x[already_id]
  unmatched <- !is.na(x) & x != "[deleted]" & is.na(out)
  if (any(unmatched)) {
    stop(sprintf("%d speaker values have no ID (e.g. '%s') - not found in comments.csv.",
                 sum(unmatched), x[unmatched][1]))
  }
  out
}


# -- 3. Masking of username mentions in text --
MENTION_RE <- regex("(?:/|\\b)u(?:ser)?/[A-Za-z0-9_-]{3,20}", ignore_case = TRUE)
PREFIX_RE  <- regex("^/?u(?:ser)?/", ignore_case = TRUE)

mention_counts <- c(known = 0L, unknown = 0L)

mask_mentions <- function(text) {
  str_replace_all(text, MENTION_RE, function(m) {
    prefix <- str_extract(m, PREFIX_RE)
    name   <- str_remove(m, PREFIX_RE)
    id     <- unname(lookup_lower[tolower(name)])
    mention_counts["known"]   <<- mention_counts["known"]   + sum(!is.na(id))
    mention_counts["unknown"] <<- mention_counts["unknown"] + sum(is.na(id))
    paste0(prefix, ifelse(is.na(id), "[user]", id))
  })
}

# Apply both replacements to one data frame; returns the frame and what was changed
pseudonymize_df <- function(df) {
  changed <- character(0)
  for (col in intersect(SPEAKER_COLS, names(df))) {
    df[[col]] <- pseudonymize_speaker(df[[col]])
    changed <- c(changed, col)
  }
  for (col in intersect(TEXT_COLS, names(df))) {
    df[[col]] <- mask_mentions(df[[col]])
    changed <- c(changed, paste0(col, " (u/ mentions)"))
  }
  list(df = df, changed = changed)
}

report <- function(file, df, changed) {
  cat(sprintf("  %-30s %9s rows | replaced: %s\n", file, format(nrow(df), big.mark = ","),
              if (length(changed)) paste(changed, collapse = ", ") else "nothing (no username columns)"))
}


# -- 4. CSVs: input_data_from_python/ -> input_data_anonymized/ --
cat("\n-- CSV files --\n")
csv_files <- list.files(PRIVATE_CSV_DIR, pattern = "\\.csv$", full.names = TRUE)
csv_files <- csv_files[basename(csv_files) != basename(KEY_FILE)]

mention_counts[] <- 0L
for (f in csv_files) {
  df  <- if (basename(f) == "comments.csv") comments_raw else read_raw(f)
  res <- pseudonymize_df(df)
  write_raw(res$df, file.path(SHARED_CSV_DIR, basename(f)))
  report(basename(f), res$df, res$changed)
}
cat(sprintf("  u/ mentions replaced in CSV text: %s known users, %s other accounts\n",
            format(mention_counts["known"], big.mark = ","), format(mention_counts["unknown"], big.mark = ",")))


# -- 5. RDS: prepped_data/ -> prepped_data_anonymized/ --
cat("\n-- RDS files --\n")
rds_files <- list.files(PRIVATE_RDS_DIR, pattern = "\\.rds$", full.names = TRUE)
if (length(rds_files) == 0) {
  warning("No .rds files in ", PRIVATE_RDS_DIR, "/ - run 1_data_prep.R first.")
}

mention_counts[] <- 0L
for (f in rds_files) {
  obj <- readRDS(f)
  if (is.data.frame(obj)) {
    res <- pseudonymize_df(obj)
    saveRDS(res$df, file.path(SHARED_RDS_DIR, basename(f)))
    report(basename(f), res$df, res$changed)
  } else {
    message("  Skipped (not a data frame): ", basename(f))
  }
}
cat(sprintf("  u/ mentions replaced in RDS text: %s known users, %s other accounts\n",
            format(mention_counts["known"], big.mark = ","), format(mention_counts["unknown"], big.mark = ",")))


# -- 6. Checks --
cat("\n-- Checks --\n")

check_speakers <- function(df, label) {
  for (col in intersect(SPEAKER_COLS, names(df))) {
    bad <- sum(!is.na(df[[col]]) & df[[col]] != "[deleted]" & !str_detect(df[[col]], id_pattern))
    cat(sprintf("  %-45s non-ID values (should be 0): %d\n", paste0(label, " $", col), bad))
  }
}

shared_csv <- list.files(SHARED_CSV_DIR, pattern = "\\.csv$", full.names = TRUE)
for (f in shared_csv) check_speakers(read_raw(f), basename(f))

shared_rds <- list.files(SHARED_RDS_DIR, pattern = "\\.rds$", full.names = TRUE)
for (f in shared_rds) {
  obj <- readRDS(f)
  if (is.data.frame(obj)) check_speakers(obj, basename(f))
}

# Private and shared prepped data should match row for row, apart from usernames and masked text
if (file.exists(file.path(PRIVATE_RDS_DIR, "comments_enriched.rds")) &&
    file.exists(file.path(SHARED_RDS_DIR, "comments_enriched.rds"))) {
  ce_private <- readRDS(file.path(PRIVATE_RDS_DIR, "comments_enriched.rds"))
  ce_shared  <- readRDS(file.path(SHARED_RDS_DIR, "comments_enriched.rds"))
  same_cols  <- setdiff(names(ce_private), c(SPEAKER_COLS, TEXT_COLS))
  cat(sprintf("  comments_enriched.rds rows - local: %s | shared: %s\n",
              format(nrow(ce_private), big.mark = ","), format(nrow(ce_shared), big.mark = ",")))
  cat(sprintf("  Other columns identical in both versions: %s\n",
              isTRUE(all.equal(ce_private[same_cols], ce_shared[same_cols], check.attributes = FALSE))))
  cat(sprintf("  Username -> ID mapping consistent with key: %s\n",
              all(unname(lookup[ce_private$speaker[ce_private$speaker != "[deleted]"]]) ==
                    ce_shared$speaker[ce_private$speaker != "[deleted]"])))
}

# Residual risk: top-50 usernames (5+ characters) written as bare words, without u/
top_names <- id_key %>% filter(nchar(speaker) >= 5) %>% slice_head(n = 50) %>% pull(speaker)
texts     <- read_raw(file.path(SHARED_CSV_DIR, "comments.csv"))$text
bare_hits <- map_int(top_names, ~ sum(str_detect(
  texts, regex(paste0("\\b", str_replace_all(.x, "(\\W)", "\\\\\\1"), "\\b"), ignore_case = TRUE)),
  na.rm = TRUE))
cat(sprintf("  Comments still naming a top-50 user as a bare word: %s (across %d names) - not replaced\n",
            format(sum(bare_hits), big.mark = ","), sum(bare_hits > 0)))

cat("\nDone.\n")
cat("  Local only : ", PRIVATE_CSV_DIR, "/ (incl. key), ", PRIVATE_RDS_DIR, "/\n", sep = "")
cat("  Shareable  : ", SHARED_CSV_DIR, "/, ", SHARED_RDS_DIR, "/\n", sep = "")
cat("Reminder: text, comment IDs and permalinks still link back to Reddit authors.\n")
