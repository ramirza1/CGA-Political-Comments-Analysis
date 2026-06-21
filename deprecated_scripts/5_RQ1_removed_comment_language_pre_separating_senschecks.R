# removed_comment_language.R
# This script attempts to answer RQ1a: What is linguistically distinctive 
# about the final,moderator-removed comment in awry conversations, relative 
# to the rest of the corpus? Is its vocabulary disproportionately political?

# Unit of text : the comment (one row per comment in comments_enriched).
# Outcome      : is_removed_comment (TRUE = the removed final comment of an awry
#                chain; FALSE = every other comment in the corpus).
# Method       : skip-gram DTM -> multinomial* LASSO (alpha = 0.99) with year
#                fixed effects forced in -> relaxed LASSO to triangulate.
#                n-gram + stem representations as robustness checks.

# Mirrors the text-mining pipeline established in a prior project
# (clean_corpus / skipgram_tokenizer / ngram_tokenizer, alpha = 0.99,
# lambda.1se, relaxed refit).

# --- 0. Packages ---
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyverse, stringr, tm, SnowballC, tidytext,
               NLP, RColorBrewer, wordcloud, glmnet, Matrix, tictoc)

set.seed(5710)   # project-wide seed, matching the partisan-disliking study

# --- 1. Paths ---

PREPPED <- file.path("prepped_data") # Load data built by 1_data_prep.R

DIR_MAIN <- file.path("main_results", "removed_comment_language") # csv exhibits for the writeup
DIR_FIG  <- file.path("graphical_output", "removed_comment_language")
DIR_TXT  <- file.path("text_outputs", "removed_comment_language")          # full model dumps
for (d in c(DIR_MAIN, DIR_FIG, DIR_TXT)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# --- 2. Load prepped data ---
ce   <- readRDS(file.path(PREPPED, "comments_enriched.rds")) # core to RQ
conv <- readRDS(file.path(PREPPED, "conversations.rds")) # for checks
spkr <- readRDS(file.path(PREPPED, "speakers.rds")) # for checks


# Expected columns in ce:
#   comment_id, conversation_id, speaker, reply_to, timestamp, text, score,
#   top_level_comment, gilded, stickied, author_flair_text, timestamp_dt,
#   year, month, comment_type, char_count, word_count, pair_id,
#   has_removed_comment, split, is_removed_comment, thread_depth
stopifnot(all(c("text", "is_removed_comment", "year") %in% names(ce)))
print(names(ce))


# --- 3. Set outcome variable---
# is_removed_comment is already a logical with exactly one TRUE per awry
# conversation (9,789 removed vs 107,004 not). This is our outcome variable.
ce <- ce %>%
  mutate(
    text = as.character(text),
    year = as.integer(year),                       # 2015..2022
    is_removed_comment = as.logical(is_removed_comment)
  ) %>%
  filter(!is.na(text), trimws(text) != "", !is.na(year))

cat("Comments total:        ", nrow(ce), "\n")
cat("Removed (outcome=TRUE): ", sum(ce$is_removed_comment), "\n")
cat("Not removed:            ", sum(!ce$is_removed_comment), "\n")

# Outcome vector (factor; works for both binary and multinomial glmnet).
# Level order: reference = "not_removed", target = "removed".
outcome_vec <- factor(ifelse(ce$is_removed_comment, "removed", "not_removed"),
                      levels = c("not_removed", "removed"))

# --- 4. Build text corpus ---
# We only need a single corpus: every comment is a document, and the outcome
# vector flags which documents are the removed ones.
mycorpus <- VCorpus(VectorSource(ce$text))

# Reddit-specific pre-scrub: decode HTML entities (&gt; from quote blocks,
# &amp;, &lt;) and strip URLs, before the standard cleaning. Without this,
# "gt", "http", "www" surface as spurious high-frequency terms.

strip_reddit <- content_transformer(function(x) {
  x <- gsub("&gt;|&lt;|&amp;", " ", x)
  x <- gsub("http\\S+|www\\.\\S+", " ", x)
  x <- gsub("/?u/\\S+|/?r/\\S+", " ", x)     # user/subreddit mentions
  x
})

clean_corpus <- function(corpus, remove_punct = TRUE) {
  corpus <- tm_map(corpus, strip_reddit)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  if (remove_punct) corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus
}

# Punctuation retained for skip/n-gram adjacency; removed for stems.
tic("Corpus cleaning")
corpus_clean <- clean_corpus(mycorpus, remove_punct = FALSE)   # skip & ngram
corpus_stem  <- clean_corpus(mycorpus, remove_punct = TRUE)
corpus_stem  <- tm_map(corpus_stem, stemDocument, lazy = TRUE)
toc()

# ---- 5. Tokenizers ---
ngram_tokenizer <- function(x, n = 2) {
  unlist(lapply(ngrams(words(x), 1:n), paste, collapse = "_"), use.names = FALSE)
}

skipgram_tokenizer <- function(x, n = 2, k = 2) {
  toks <- words(x)
  if (length(toks) < 2) return(character(0))
  df <- data.frame(text = paste(toks, collapse = " "), stringsAsFactors = FALSE)
  sg <- tidytext::unnest_tokens(df, ngram, text, token = "skip_ngrams", n = n, k = k)
  sg$ngram
}

# --- 6. Build DTMs (with two sparsity schemes) ---
# Primary, sparsity set at >=1% of the REMOVED comments, specifically (~98 docs), 
# not the whole corpus. A corpus-wide floor is a high bar for any single term
# to be informative about a class with only 9.789 positive examples.

# As a sensitivity check, we also test the sparsity index for 1% of the whole corpus.

# This duplicates downstream LASSO fitting (6 models instead of 3), and the minority
# floor is far more permissive, resulting a larger vocabularity and longer runtime
# for the minority sparsity index

is_removed <- ce$is_removed_comment        # same row order as the corpus
n_removed  <- sum(is_removed)

TARGET_PCT_MINORITY <- 0.01
min_docs_minority   <- ceiling(TARGET_PCT_MINORITY * n_removed)
SPARSITY_FULL        <- 0.01

cat("\nMinority-conditional floor: term must appear in >=", min_docs_minority,
    "of the", n_removed, "removed comments\n")
cat("Whole-corpus floor (sensitivity): >=", ceiling(SPARSITY_FULL * nrow(ce)),
    "of", nrow(ce), "comments\n\n")

# Keep columns whose document frequency WITHIN the removed-comment subset
# clears min_docs, applied back to the full (all-row) matrix.
filter_by_minority_doc_freq <- function(dtm, is_minority, min_docs) {
  dtm_minority      <- dtm[is_minority, ]
  doc_freq_minority <- slam::col_sums(dtm_minority > 0)
  dtm[, doc_freq_minority >= min_docs, drop = FALSE]
}

tic("Skip-gram DTM (raw)")
dtm.skip.raw <- DocumentTermMatrix(
  corpus_clean,
  control = list(tokenize = function(x) skipgram_tokenizer(x, n = 2, k = 2))
)
toc()
stopifnot(nrow(dtm.skip.raw) == length(is_removed))

tic("N-gram DTM (raw)")
dtm.ngram.raw <- DocumentTermMatrix(
  corpus_clean,
  control = list(tokenize = function(x) ngram_tokenizer(x, 2))
)
toc()

tic("Stem DTM (raw)")
dtm.stem.raw <- DocumentTermMatrix(corpus_stem)
toc()

# Apply both schemes to each raw DTM.
dtm.skip.min   <- filter_by_minority_doc_freq(dtm.skip.raw,  is_removed, min_docs_minority)
dtm.ngram.min  <- filter_by_minority_doc_freq(dtm.ngram.raw, is_removed, min_docs_minority)
dtm.stem.min   <- filter_by_minority_doc_freq(dtm.stem.raw,  is_removed, min_docs_minority)

dtm.skip.full  <- removeSparseTerms(dtm.skip.raw,  1 - SPARSITY_FULL)
dtm.ngram.full <- removeSparseTerms(dtm.ngram.raw, 1 - SPARSITY_FULL)
dtm.stem.full  <- removeSparseTerms(dtm.stem.raw,  1 - SPARSITY_FULL)

# Year-token + punctuation-only cleanup, applied to all six.
remove_terms <- c("year", "years", as.character(2015:2022))
drop_terms <- function(dtm) dtm[, !colnames(dtm) %in% remove_terms, drop = FALSE]
drop_nonalpha_terms <- function(dtm) {
  has_letter <- grepl("[a-z]", colnames(dtm))
  dtm[, has_letter, drop = FALSE]
}
clean_dtm <- function(dtm) drop_nonalpha_terms(drop_terms(dtm))

dtm.skip.min   <- clean_dtm(dtm.skip.min)
dtm.ngram.min  <- clean_dtm(dtm.ngram.min)
dtm.stem.min   <- clean_dtm(dtm.stem.min)
dtm.skip.full  <- clean_dtm(dtm.skip.full)
dtm.ngram.full <- clean_dtm(dtm.ngram.full)
dtm.stem.full  <- clean_dtm(dtm.stem.full)

cat("\nDTM term counts:\n")
cat("  [PRIMARY: minority floor]      skip:", ncol(dtm.skip.min),  " ngram:", ncol(dtm.ngram.min),  " stem:", ncol(dtm.stem.min),  "\n")
cat("  [SENSITIVITY: whole-corpus 1%] skip:", ncol(dtm.skip.full), " ngram:", ncol(dtm.ngram.full), " stem:", ncol(dtm.stem.full), "\n")

# --- 6b. Vocabulary overlap between sparsity schemes ---
terms_min  <- colnames(dtm.skip.min)
terms_full <- colnames(dtm.skip.full)

cat("\nSkip-gram vocab overlap: in both =", length(intersect(terms_min, terms_full)),
    " | minority-only =", length(setdiff(terms_min, terms_full)),
    " | whole-corpus-only =", length(setdiff(terms_full, terms_min)), "\n")

# -- 7. LASSO variable selection ---
# alpha = 0.99 (elastic net, ~1% ridge) to reduce risk of collinearity in 
# skip-gram features.
# Year forced in with zero penalty -> coefficients on text are within-year.
# lambda.1se for parsimony. Binary family (logistic), can set to FAMILY <- "multinomial" 
# if we add multi-class outcome

FAMILY <- "binomial"

# Class imbalance note: removed comments are ~8.4% of the corpus (9,789 /
# 116,793). type.measure = "class" optimizes misclassification, which has a
# degenerate optimum here (predict "not_removed" for everyone) and can select
# few/zero text terms. type.measure = "deviance" optimises the likelihood
# instead and doesn't share that failure mode, so it's the default below.
# This keeps the full natural sample and the true 8.4% base rate, which
# matters for a descriptive model.
#
# Escalation path if deviance still selects a thin/empty model:
#   1. obs_weights below (upweight the minority class within the full
#      sample) - preserves N and the natural base rate, just reweights the
#      loss function.
#   2. Only as a last resort, downsample non-removed comments - and if so,
#      repeat across multiple random draws rather than one fixed sample, the
#      same way bootstrap-stability is planned for the overlap analysis.

LASSO_TYPE_MEASURE <- "deviance"

fit_lasso <- function(dtm, outcome, year, family = FAMILY, seed = 5710,
                      type_measure = LASSO_TYPE_MEASURE, use_weights = FALSE) {
  dtm_mat <- as.matrix(dtm)
  colnames(dtm_mat) <- make.names(colnames(dtm_mat), unique = TRUE)   
  model_data <- data.frame(
    outcome = outcome,
    year    = factor(year),
    dtm_mat,
    check.names = FALSE
  )
  X <- sparse.model.matrix(outcome ~ ., data = model_data)[, -1]
  
  year_cols <- grep("^year", colnames(X))
  penalty   <- rep(1, ncol(X))
  penalty[year_cols] <- 0   # force year FEs in
  
  # Optional observation weights (escalation step 1 above): upweight the
  # minority ("removed") class so it contributes proportionally to the loss,
  # without discarding any non-removed comments. Inverse-frequency weights,
  # normalized so the average weight is 1.
  
  obs_weights <- NULL
  if (use_weights) {
    tab <- table(outcome)
    w   <- ifelse(outcome == names(tab)[which.max(tab)],
                  1, max(tab) / min(tab))
    obs_weights <- w / mean(w)
  }
  
  set.seed(seed)
  cvfit <- cv.glmnet(
    x = X, y = model_data$outcome,
    alpha = 0.99, family = family,
    type.measure = type_measure,
    penalty.factor = penalty,
    weights = obs_weights   # NULL = unweighted (glmnet default)
  )
  list(cvfit = cvfit, X = X, y = model_data$outcome, penalty = penalty)
}

# Run LASSO - minority sparsity index
tic("LASSO: skip-gram [minority]")
lasso.skip.min   <- fit_lasso(dtm.skip.min,   outcome_vec, ce$year)
toc()

tic("LASSO: n-gram [minority]")
lasso.ngram.min  <- fit_lasso(dtm.ngram.min,  outcome_vec, ce$year)
toc()

tic("LASSO: stem [minority]")
lasso.stem.min   <- fit_lasso(dtm.stem.min,   outcome_vec, ce$year)
toc()

# Run LASSO - full sparsity index

tic("LASSO: skip-gram [whole-corpus]")
lasso.skip.full  <- fit_lasso(dtm.skip.full,  outcome_vec, ce$year)
toc()

tic("LASSO: n-gram [whole-corpus]")
lasso.ngram.full <- fit_lasso(dtm.ngram.full, outcome_vec, ce$year)
toc()

tic("LASSO: stem [whole-corpus]")
lasso.stem.full  <- fit_lasso(dtm.stem.full,  outcome_vec, ce$year)
toc()

# --- 7b. LASSO diagnostics ---
# CV curves justifying the lambda.1se choice for the primary (skip-gram)
# model, both schemes. Placed here, not under word clouds, since this is a
# model-fit diagnostic rather than a vocabulary visualization.

png(file.path(DIR_FIG, "rq1a_removed_skip_cv_minority.png"), width = 1200, height = 900, res = 150)
plot(lasso.skip.min$cvfit)
title("LASSO CV: comment removal (skip-grams, minority floor)", line = 2.5)
dev.off()

png(file.path(DIR_FIG, "rq1a_removed_skip_cv_wholecorpus.png"), width = 1200, height = 900, res = 150)
plot(lasso.skip.full$cvfit)
title("LASSO CV: comment removal (skip-grams, whole-corpus floor)", line = 2.5)
dev.off()


# ---- 8. Extract coefficients --------------
# For binomial glmnet, we get one matrix of coefficients, which describe how each
# term affects the odds that a comment is classified as removed. If a coefficient
# is positive, removal is more likely. For multinomial versions, glmnet would give
# separate coefficients for each outcome class.


extract_coefs <- function(lasso_obj, s = "lambda.1se") {
  cc <- coef(lasso_obj$cvfit, s = s)
  if (is.list(cc)) cc <- cc[["removed"]]          # multinomial case
  idx   <- which(cc[, 1] != 0)
  terms <- rownames(cc)[idx]
  vals  <- cc[idx, 1]
  keep  <- !grepl("^\\(Intercept\\)$", terms)
  data.frame(
    term    = terms[keep],
    coef    = as.numeric(vals[keep]),
    is_year = grepl("^year", terms[keep]),
    row.names = NULL
  ) %>% arrange(desc(coef))
}

coef.skip.min   <- extract_coefs(lasso.skip.min)
coef.ngram.min  <- extract_coefs(lasso.ngram.min)
coef.stem.min   <- extract_coefs(lasso.stem.min)

coef.skip.full  <- extract_coefs(lasso.skip.full)
coef.ngram.full <- extract_coefs(lasso.ngram.full)
coef.stem.full  <- extract_coefs(lasso.stem.full)

n_text <- function(cdf) sum(!cdf$is_year)
cat("\nNon-zero TEXT terms selected at lambda.1se:\n")
cat("  [minority]      skip:", n_text(coef.skip.min),  " ngram:", n_text(coef.ngram.min),  " stem:", n_text(coef.stem.min),  "\n")
cat("  [whole-corpus]  skip:", n_text(coef.skip.full), " ngram:", n_text(coef.ngram.full), " stem:", n_text(coef.stem.full), "\n")


# Top-N tables (primary = skip-gram). Positive coef = predicts removal
top_terms <- function(cdf, n = 50) {
  cdf %>% filter(!is_year, coef > 0) %>% slice_head(n = n)
}

top10.skip.min <- top_terms(coef.skip.min, 10)
top25.skip.min <- top_terms(coef.skip.min, 25)
top50.skip.min <- top_terms(coef.skip.min, 50)

top10.skip.full <- top_terms(coef.skip.full, 10)
top25.skip.full <- top_terms(coef.skip.full, 25)
top50.skip.full <- top_terms(coef.skip.full, 50)

cat("\nTop 10 skip-gram terms predicting REMOVAL [minority-conditional, PRIMARY]:\n")
print(top10.skip.min, row.names = FALSE)
cat("\nTop 10 skip-gram terms predicting REMOVAL [whole-corpus, sensitivity]:\n")
print(top10.skip.full, row.names = FALSE)

# Write coefficient exhibits
write.csv(coef.skip.min,   file.path(DIR_MAIN, "rq1a_removed_skip_min_coefs.csv"),   row.names = FALSE)
write.csv(coef.ngram.min,  file.path(DIR_MAIN, "rq1a_removed_ngram_min_coefs.csv"),  row.names = FALSE)
write.csv(coef.stem.min,   file.path(DIR_MAIN, "rq1a_removed_stem_min_coefs.csv"),   row.names = FALSE)
write.csv(coef.skip.full,  file.path(DIR_MAIN, "rq1a_removed_skip_full_coefs.csv"),  row.names = FALSE)
write.csv(coef.ngram.full, file.path(DIR_MAIN, "rq1a_removed_ngram_full_coefs.csv"), row.names = FALSE)
write.csv(coef.stem.full,  file.path(DIR_MAIN, "rq1a_removed_stem_full_coefs.csv"),  row.names = FALSE)
write.csv(top50.skip.min,  file.path(DIR_MAIN, "rq1a_removed_skip_min_top50.csv"),   row.names = FALSE)
write.csv(top50.skip.full, file.path(DIR_MAIN, "rq1a_removed_skip_full_top50.csv"),  row.names = FALSE)

# --- 8b. Distinct vocabulary: candidates vs. what LASSO actually selected ---
minority_only_raw    <- sort(setdiff(terms_min, terms_full))
wholecorpus_only_raw <- sort(setdiff(terms_full, terms_min))

# Build the same raw->sanitized mapping fit_lasso() used internally 
# (full column set, unique = TRUE) so this this lookup matches that:

name_map_min  <- setNames(make.names(colnames(dtm.skip.min),  unique = TRUE), colnames(dtm.skip.min))
name_map_full <- setNames(make.names(colnames(dtm.skip.full), unique = TRUE), colnames(dtm.skip.full))


minority_only_selected <- coef.skip.min %>%
  filter(term %in% make.names(minority_only_raw)) %>%
  arrange(desc(coef))

wholecorpus_only_selected <- coef.skip.full %>%
  filter(term %in% make.names(wholecorpus_only_raw)) %>%
  arrange(desc(coef))

cat("\nMinority-only terms LASSO selected (", nrow(minority_only_selected), "of", length(minority_only_raw), "):\n")
print(minority_only_selected, row.names = FALSE)
cat("\nWhole-corpus-only terms LASSO selected (", nrow(wholecorpus_only_selected), "of", length(wholecorpus_only_raw), "):\n")
print(wholecorpus_only_selected, row.names = FALSE)

write.csv(data.frame(term = minority_only_raw),    file.path(DIR_MAIN, "rq1a_minority_only_candidate_terms.csv"),    row.names = FALSE)
write.csv(data.frame(term = wholecorpus_only_raw), file.path(DIR_MAIN, "rq1a_wholecorpus_only_candidate_terms.csv"), row.names = FALSE)
write.csv(minority_only_selected,    file.path(DIR_MAIN, "rq1a_minority_only_selected_by_lasso.csv"),    row.names = FALSE)
write.csv(wholecorpus_only_selected, file.path(DIR_MAIN, "rq1a_wholecorpus_only_selected_by_lasso.csv"), row.names = FALSE)

# --- 9. Word clouds ---

make_wordcloud <- function(terms_df, fname, title_txt, scale_vals) {
  png(file.path(DIR_FIG, fname), width = 1400, height = 900, res = 200)
  par(mar = c(0, 0, 2, 0))
  set.seed(5710)
  wordcloud(terms_df$term, terms_df$coef, colors = brewer.pal(8, "OrRd")[3:8],
            scale = scale_vals, min.freq = 0, random.order = FALSE)
  title(title_txt, font.main = 2, cex.main = 1.0)
  dev.off()
}

make_wordcloud(top25.skip.min,  "rq1a_removed_wordcloud_top25_minority.png",
               "Top 25 terms predicting removal - minority-conditional floor (PRIMARY)", c(5.2, 0.8))
make_wordcloud(top50.skip.min,  "rq1a_removed_wordcloud_top50_minority.png",
               "Top 50 terms predicting removal - minority-conditional floor (PRIMARY)", c(4.8, 0.55))
make_wordcloud(top25.skip.full, "rq1a_removed_wordcloud_top25_wholecorpus.png",
               "Top 25 terms predicting removal - whole-corpus 1% floor (sensitivity)", c(5.2, 0.8))
make_wordcloud(top50.skip.full, "rq1a_removed_wordcloud_top50_wholecorpus.png",
               "Top 50 terms predicting removal - whole-corpus 1% floor (sensitivity)", c(4.8, 0.55))

# --- 10. Relaxed LASSO (triangulation) ---
# Refit an unregularized logistic GLM on the LASSO-selected text terms (plus
# year FEs) to get less-biased, more interpretable log-odds and Wald SEs.
# Positive estimate for a term term raises the odds the comment was removed.
run_relaxed_lasso <- function(coef_df, dtm, outcome_vec, year) {
  relax_terms <- make.names(top_terms(coef_df, n = nrow(coef_df))$term)
  mat <- as.matrix(dtm)
  colnames(mat) <- make.names(colnames(mat), unique = TRUE)   # <-- added unique = TRUE
  relax_terms <- intersect(relax_terms, colnames(mat))
  relax_df <- data.frame(removed = as.integer(outcome_vec == "removed"), year = factor(year),
                         mat[, relax_terms, drop = FALSE], check.names = FALSE)
  fit <- glm(removed ~ ., data = relax_df, family = binomial())
  broom::tidy(fit) %>% filter(!grepl("^\\(Intercept\\)|^year", term)) %>% arrange(desc(estimate))
}

relax_out.min  <- run_relaxed_lasso(coef.skip.min,  dtm.skip.min,  outcome_vec, ce$year)
relax_out.full <- run_relaxed_lasso(coef.skip.full, dtm.skip.full, outcome_vec, ce$year)

write.csv(relax_out.min,  file.path(DIR_MAIN, "rq1a_removed_skip_min_relaxed.csv"),  row.names = FALSE)
write.csv(relax_out.full, file.path(DIR_MAIN, "rq1a_removed_skip_full_relaxed.csv"), row.names = FALSE)

cat("\nTop 10 relaxed-LASSO terms [minority-conditional, PRIMARY]:\n");  print(head(relax_out.min, 10), row.names = FALSE)
cat("\nTop 10 relaxed-LASSO terms [whole-corpus, sensitivity]:\n");      print(head(relax_out.full, 10), row.names = FALSE)

# Within-scheme triangulation (LASSO vs relaxed), PRIMARY model
triangulation.min <- top25.skip.min %>%
  select(term, lasso_coef = coef) %>% mutate(term = make.names(term)) %>%
  left_join(relax_out.min %>% select(term, relaxed_estimate = estimate, relaxed_p = p.value), by = "term")
write.csv(triangulation.min, file.path(DIR_MAIN, "rq1a_removed_triangulation_minority.csv"), row.names = FALSE)

# Cross-scheme comparison: this is the table that directly answers the
# sparsity question - which top-25 terms are unique to the minority-
# conditional floor (i.e. would have been missed at the old 1%-of-corpus floor)?
# This is in addition to the full comparison of vocabulary differences documented earlier.
scheme_compare <- full_join(
  top25.skip.min  %>% select(term, coef_minority    = coef),
  top25.skip.full %>% select(term, coef_wholecorpus = coef),
  by = "term"
)
write.csv(scheme_compare, file.path(DIR_MAIN, "rq1a_sparsity_scheme_comparison_top25.csv"), row.names = FALSE)

cat("\nTerms in minority-conditional top 25 NOT in whole-corpus top 25:\n")
print(scheme_compare %>% filter(!is.na(coef_minority), is.na(coef_wholecorpus)) %>% pull(term))



# --- 11. Full model dumps ---
sink(file.path(DIR_TXT, "rq1a_removed_comment_language_dump.txt"))
cat("RQ1a — removed comment language\nFamily:", FAMILY, "| seed: 5710\n")
cat("Minority-conditional floor: >=", min_docs_minority, "of", n_removed, "removed comments\n")
cat("Whole-corpus floor (sensitivity): >=", ceiling(SPARSITY_FULL * nrow(ce)), "of", nrow(ce), "comments\n")

cat("\n=== PRIMARY: minority-conditional floor ===\n")
cat("skip-gram lambda.1se:", lasso.skip.min$cvfit$lambda.1se, "| text terms selected:", n_text(coef.skip.min), "\n\n")
print(coef.skip.min, row.names = FALSE)
cat("\n--- relaxed LASSO ---\n"); print(as.data.frame(relax_out.min), row.names = FALSE)

cat("\n\n=== SENSITIVITY CHECK: whole-corpus 1% floor ===\n")
cat("skip-gram lambda.1se:", lasso.skip.full$cvfit$lambda.1se, "| text terms selected:", n_text(coef.skip.full), "\n\n")
print(coef.skip.full, row.names = FALSE)
cat("\n--- relaxed LASSO ---\n"); print(as.data.frame(relax_out.full), row.names = FALSE)

cat("\n\n=== ROBUSTNESS: n-gram and stem (both schemes) ===\n")
cat("\n-- n-gram [minority] --\n");      print(coef.ngram.min,  row.names = FALSE)
cat("\n-- n-gram [whole-corpus] --\n");  print(coef.ngram.full, row.names = FALSE)
cat("\n-- stem [minority] --\n");        print(coef.stem.min,   row.names = FALSE)
cat("\n-- stem [whole-corpus] --\n");    print(coef.stem.full,  row.names = FALSE)
sink()

cat("\nDone. Exhibits in:", DIR_MAIN, "| figures in:", DIR_FIG, "\n")

