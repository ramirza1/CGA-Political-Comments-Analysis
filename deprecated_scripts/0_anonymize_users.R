# 0. anonymize_users.R for Conversations Gone Awry (CMV) - Political Comments Analysis
#
# Creates a copy of input_data_from_python/ in which Reddit usernames are replaced by
# ranked pseudonymous IDs:
#   user_00001 = the user with the most comments in the corpus, user_00002 the next, ...
#   Ties are broken alphabetically by username (C locale, case-sensitive).
#   [deleted] is not a user and is left as "[deleted]".
#
# IDs are ranked on the FULL corpus (comments.csv), so they are stable across samples
# (e.g. user_table_postid.csv and user_table_full.csv use the same IDs).
#
# What is replaced:
#   - Any column named `speaker` or `speaker_id`, in every CSV in the input folder
#   - Username mentions inside comment text ("u/name", "/u/name", ".../user/name" links),
#     if MASK_MENTIONS = TRUE:
#       known users -> u/user_XXXXX ; unknown accounts -> u/[user]
#
# What is NOT replaced (re-identification is still possible from these):
#   - Comment text itself, comment / conversation IDs, and permalinks all link back to
#     the original Reddit comment and therefore its author.
#   - Usernames written in text WITHOUT a u/ prefix (reported below, not replaced).
#   So this is pseudonymisation, not anonymisation.
#
# The username <-> ID key is written to the PRIVATE input folder only, never to the
# anonymised folder, so analyses can be traced back internally.
#
# Run from the project root, after the Python export and before 1_data_prep.R.

# -- 0. Packages --
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)

# -- 1. Settings --
IN_DIR        <- "input_data_from_python"          # private (contains usernames)
OUT_DIR       <- "input_data_anonymized"           # pseudonymised copy
KEY_FILE      <- file.path(IN_DIR, "PRIVATE_user_id_key.csv")
MASK_MENTIONS <- TRUE
ID_PREFIX     <- "user_"
SPEAKER_COLS  <- c("speaker", "speaker_id")

dir.create(OUT_DIR, showWarnings = FALSE)

# Read/write everything as text so values (IDs, timestamps, text) are copied exactly.
# Only empty fields are treated as missing, so a literal "NA" (e.g. a username) survives.
read_raw  <- function(path) read_csv(path, col_types = cols(.default = col_character()),
                                     na = "", show_col_types = FALSE, progress = FALSE)
write_raw <- function(df, path) write_csv(df, path, na = "")


# -- 2. Build the ranked ID key from comments.csv --
message("Building user ID key from comments.csv ...")
comments_raw <- read_raw(file.path(IN_DIR, "comments.csv"))

id_key <- comments_raw %>%
  filter(!is.na(speaker), speaker != "[deleted]") %>%
  count(speaker, name = "n_comments") %>%
  arrange(desc(n_comments), speaker) %>%
  mutate(
    rank    = row_number(),
    user_id = paste0(ID_PREFIX, formatC(rank, width = nchar(n()), flag = "0"))
  )

write_raw(id_key, KEY_FILE)
cat(sprintf("Key: %s users -> %s (%s ... %s)\n",
            format(nrow(id_key), big.mark = ","), KEY_FILE,
            first(id_key$user_id), last(id_key$user_id)))

lookup       <- setNames(id_key$user_id, id_key$speaker)
lookup_lower <- id_key %>%                       # Reddit usernames are case-insensitive
  mutate(lower = tolower(speaker)) %>%
  distinct(lower, .keep_all = TRUE)
lookup_lower <- setNames(lookup_lower$user_id, lookup_lower$lower)

# Usernames are replaced via the key; values that are already IDs (e.g. a user table built
# from the anonymised data) are kept as they are.
anonymize_speaker <- function(x) {
  out <- ifelse(is.na(x) | x == "[deleted]", x, unname(lookup[x]))
  already_id <- is.na(out) & !is.na(x) & str_detect(x, paste0("^", ID_PREFIX, "\\d+$"))
  out[already_id] <- x[already_id]
  unmatched <- !is.na(x) & x != "[deleted]" & is.na(out)
  if (any(unmatched)) {
    stop(sprintf("%d speaker values have no ID (e.g. '%s'). They do not appear in comments.csv.",
                 sum(unmatched), x[unmatched][1]))
  }
  out
}

# -- 3. Masking of u/ mentions in text --
# Matches u/name, /u/name, and profile links such as reddit.com/u/name or /user/name
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


# -- 4. Process every CSV in the input folder --
in_files <- list.files(IN_DIR, pattern = "\\.csv$", full.names = TRUE)
in_files <- in_files[basename(in_files) != basename(KEY_FILE)]

other_files <- setdiff(list.files(IN_DIR, full.names = TRUE), list.files(IN_DIR, pattern = "\\.csv$", full.names = TRUE))
if (length(other_files) > 0) {
  message("Skipped (not CSV, not copied): ", paste(basename(other_files), collapse = ", "))
}

for (f in in_files) {
  df <- if (basename(f) == "comments.csv") comments_raw else read_raw(f)
  changed <- character(0)
  
  for (col in intersect(SPEAKER_COLS, names(df))) {
    df[[col]] <- anonymize_speaker(df[[col]])
    changed <- c(changed, col)
  }
  
  if (MASK_MENTIONS && "text" %in% names(df)) {
    df$text <- mask_mentions(df$text)
    changed <- c(changed, "text (u/ mentions)")
  }
  
  write_raw(df, file.path(OUT_DIR, basename(f)))
  cat(sprintf("  %-28s %8s rows | replaced: %s\n", basename(f),
              format(nrow(df), big.mark = ","),
              if (length(changed)) paste(changed, collapse = ", ") else "nothing (no username columns)"))
}

if (MASK_MENTIONS) {
  cat(sprintf("\nu/ mentions replaced: %s known users, %s unknown accounts\n",
              format(mention_counts["known"], big.mark = ","),
              format(mention_counts["unknown"], big.mark = ",")))
}


# -- 5. Checks --
cat("\n-- Checks --\n")
comments_anon <- read_raw(file.path(OUT_DIR, "comments.csv"))

# Every speaker is now an ID or [deleted]
bad_speakers <- comments_anon %>%
  filter(!is.na(speaker), speaker != "[deleted]",
         !str_detect(speaker, paste0("^", ID_PREFIX, "\\d+$")))
cat(sprintf("Speaker values that are not an ID or [deleted] (should be 0): %d\n", nrow(bad_speakers)))

# Comment counts preserved: user_00001 has the most comments
top_check <- comments_anon %>%
  filter(speaker != "[deleted]") %>%
  count(speaker, sort = TRUE) %>%
  slice_head(n = 3)
cat("Top 3 IDs by comments (should be user_...1, 2, 3):\n")
print(top_check)

# Row counts match
cat(sprintf("comments.csv rows - original: %s | anonymised: %s\n",
            format(nrow(comments_raw), big.mark = ","), format(nrow(comments_anon), big.mark = ",")))

# Residual risk: usernames of the 50 most active users written in text WITHOUT u/
# (reported only - bare words cannot be replaced safely, e.g. a username that is an
# ordinary word). Names shorter than 5 characters are skipped.
top_names <- id_key %>% filter(nchar(speaker) >= 5) %>% slice_head(n = 50) %>% pull(speaker)
bare_hits <- map_int(top_names, ~ sum(str_detect(
  comments_anon$text, regex(paste0("\\b", str_replace_all(.x, "([\\W])", "\\\\\\1"), "\\b"),
                            ignore_case = TRUE)), na.rm = TRUE))
cat(sprintf("Comments still containing a top-50 username as a bare word: %s (across %d of 50 names)\n",
            format(sum(bare_hits), big.mark = ","), sum(bare_hits > 0)))

cat(sprintf("\nDone. Pseudonymised copy written to %s/\n", OUT_DIR))
cat("Reminder: text, comment IDs and permalinks still link back to Reddit authors.\n")