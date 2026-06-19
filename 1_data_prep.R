# 1. data_prep.R for Conversations Gone Awry (CMV) - Political Comments Analysis

# This script loads the three CSVs exported from Python/Convokit data extraction, 
# corrects column types, derives key variables, and builds dataframes ready for 
# joint analysis of the datasets, cacheing them to prepped_data/ as .rds files.

# This script runs before analysis files.

# Outputs:
#   prepped_data/comments_enriched.rds  (main analysis table)
#   prepped_data/conversations.rds
#   prepped_data/speakers.rds

# -- 0. Packages --
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate)
fmt <- function(x) format(x, big.mark = ",")

# -- 1. Paths --
INPUT_DIR  <- "input_data_from_python"
OUTPUT_DIR <- "prepped_data"

# -- 2. Load raw CSVs --

message("Loading CSVs...")

comments      <- read_csv(file.path(INPUT_DIR, "comments.csv"),      show_col_types = FALSE)
conversations <- read_csv(file.path(INPUT_DIR, "conversations.csv"), show_col_types = FALSE)
speakers      <- read_csv(file.path(INPUT_DIR, "speakers.csv"),      show_col_types = FALSE)

message(sprintf("  comments:      %s rows", format(nrow(comments),      big.mark = ",")))
message(sprintf("  conversations: %s rows", format(nrow(conversations), big.mark = ",")))
message(sprintf("  speakers:      %s rows", format(nrow(speakers),      big.mark = ",")))


# -- 3. Data cleaning for R format --


# Clean timestamp data, gilded, stickied data, and metadata for root, top_level_reply, 
# nested_reply; Compute word counts, convert potential N/A text fields to blanks

comments <- comments %>%
  mutate(
    # datetime + temporal extracts
    timestamp_dt = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"),
    year         = year(timestamp_dt),
    month        = month(timestamp_dt),
    # award and moderation flags
    gilded   = as.integer(gilded),
    stickied = stickied == "True",
    # thread position
    comment_type = case_when(
      comment_id == conversation_id                    ~ "root",
      !is.na(reply_to) & reply_to == conversation_id  ~ "top_level_reply",
      TRUE                                             ~ "nested_reply"
    ) %>% factor(levels = c("root", "top_level_reply", "nested_reply")),
    # text length
    char_count = nchar(coalesce(text, "")),
    word_count = str_count(coalesce(text, ""), "\\S+")
  )


# -- 4. Build "enriched" comments table --

# Joins comments to it's conversational outcome (awry vs not-awry) and metadata. This is key
# table for downstream scripts, as most analyses require "has_removed_comment" (i.e., awry)
# conversation at the comment level.

comments_enriched <- comments %>%
  left_join(
    conversations %>%
      select(conversation_id, pair_id, has_removed_comment, split),
    by = "conversation_id"
  )

# Sanity check: every comment should match a conversation.
n_unmatched <- sum(is.na(comments_enriched$has_removed_comment))
if (n_unmatched > 0) {
  warning(sprintf(
    "%s comments did not match a conversation — check for ID mismatches in the CSVs.",
    format(n_unmatched, big.mark = ",")
  ))
} else {
  message("Join complete: all comments matched to a conversation.")
}

# -- 5. Save to prepped data --

message("Saving .rds files to prepped_data/...")
saveRDS(comments_enriched, file.path(OUTPUT_DIR, "comments_enriched.rds"))
saveRDS(conversations,     file.path(OUTPUT_DIR, "conversations.rds"))
saveRDS(speakers,          file.path(OUTPUT_DIR, "speakers.rds"))


# -- 6. Summary checks --


n_awry     <- sum( conversations$has_removed_comment, na.rm = TRUE)
n_on_track <- sum(!conversations$has_removed_comment, na.rm = TRUE)
date_min   <- min(comments$timestamp_dt, na.rm = TRUE)
date_max   <- max(comments$timestamp_dt, na.rm = TRUE)

cat(sprintf("\nPrep complete. %s comments | %s conversations | %s speakers\n",
            fmt(nrow(comments_enriched)),
            fmt(nrow(conversations)),
            fmt(nrow(speakers))))
cat(sprintf("Awry: %s / %s (%.1f%%) | Date range: %s to %s\n",
            format(n_awry),
            format(n_awry + n_on_track),
            100 * n_awry / (n_awry + n_on_track),
            format(date_min, "%b %Y"), format(date_max, "%b %Y")))
print(table(comments_enriched$comment_type))