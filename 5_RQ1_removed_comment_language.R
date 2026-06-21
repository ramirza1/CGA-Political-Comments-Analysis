# removed_comment_language.R
# RQ1a: What is linguistically distinctive about the final, moderator-removed
# comment in awry conversations, relative to the rest of the corpus? Is its
# vocabulary disproportionately political?
#
# PRIMARY MODEL: skip-gram DTM, sparsity floor calibrated to the minority
# (removed-comment) class -> binomial LASSO (alpha = 0.99) with year fixed
# effects forced in -> relaxed LASSO to triangulate.
#
# SENSITIVITY CHECKS (gated by RUN_SENSITIVITY_CHECKS, run at the end so the
# primary model can be iterated on quickly without re-fitting all six):
#   - whole-corpus 1% sparsity floor (the original spec) for skip-grams
#   - n-gram and stem representations, under both sparsity schemes

# --- 0. Packages ---
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyverse, stringr, tm, SnowballC, tidytext,
               NLP, RColorBrewer, wordcloud, glmnet, Matrix, tictoc)

set.seed(5710)

RUN_SENSITIVITY_CHECKS <- TRUE
# FALSE = only (re)build/fit the primary minority-conditional skip-gram model.
# Skips corpus_stem, the n-gram/stem raw DTMs, and five LASSO fits entirely.

# --- 1. Paths ---
PREPPED <- file.path("prepped_data")

RESULTS_ROOT    <- file.path("results", "removed_comment_language")
DIR_PRIMARY     <- file.path(RESULTS_ROOT, "primary")
DIR_SENSITIVITY <- file.path(RESULTS_ROOT, "sensitivity_checks")
DIR_DIAGNOSTICS <- file.path(RESULTS_ROOT, "diagnostics")
DIR_FIG <- file.path("graphical_output", "removed_comment_language")
DIR_TXT <- file.path("text_outputs", "removed_comment_language")

for (d in c(DIR_PRIMARY, DIR_SENSITIVITY, DIR_DIAGNOSTICS, DIR_FIG, DIR_TXT)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}
# --- 2. Load prepped data ---
ce   <- readRDS(file.path(PREPPED, "comments_enriched.rds"))
conv <- readRDS(file.path(PREPPED, "conversations.rds"))
spkr <- readRDS(file.path(PREPPED, "speakers.rds"))
stopifnot(all(c("text", "is_removed_comment", "year") %in% names(ce)))

# --- 3. Outcome variable ---
ce <- ce %>%
  mutate(text = as.character(text), year = as.integer(year),
         is_removed_comment = as.logical(is_removed_comment)) %>%
  filter(!is.na(text), trimws(text) != "", !is.na(year))

cat("Comments total:        ", nrow(ce), "\n")
cat("Removed (outcome=TRUE): ", sum(ce$is_removed_comment), "\n")
cat("Not removed:            ", sum(!ce$is_removed_comment), "\n")

outcome_vec <- factor(ifelse(ce$is_removed_comment, "removed", "not_removed"),
                      levels = c("not_removed", "removed"))
is_removed <- ce$is_removed_comment
n_removed  <- sum(is_removed)

# --- 4. Build text corpus ---
mycorpus <- VCorpus(VectorSource(ce$text))

strip_reddit <- content_transformer(function(x) {
  x <- gsub("&gt;|&lt;|&amp;", " ", x)
  x <- gsub("http\\S+|www\\.\\S+", " ", x)
  x <- gsub("/?u/\\S+|/?r/\\S+", " ", x)
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

tic("Corpus cleaning (skip/ngram)")
corpus_clean <- clean_corpus(mycorpus, remove_punct = FALSE)
toc()

# Stem corpus only needed for the stem sensitivity check.
if (RUN_SENSITIVITY_CHECKS) {
  tic("Corpus cleaning (stem)")
  corpus_stem <- clean_corpus(mycorpus, remove_punct = TRUE)
  corpus_stem <- tm_map(corpus_stem, stemDocument, lazy = TRUE)
  toc()
}

# --- 5. Tokenizers ---
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

# --- 6. Shared cleanup helpers (used by primary and sensitivity DTMs alike) ---
# Minority-conditional floor: a term must appear in >= 1% of REMOVED comments
# specifically. A corpus-wide floor is a high bar for any single term to be
# informative about a class with only 9,789 positive examples.
TARGET_PCT_MINORITY <- 0.01
min_docs_minority   <- ceiling(TARGET_PCT_MINORITY * n_removed)
cat("\nMinority-conditional floor: term must appear in >=", min_docs_minority,
    "of the", n_removed, "removed comments\n")

filter_by_minority_doc_freq <- function(dtm, is_minority, min_docs) {
  dtm_minority      <- dtm[is_minority, ]
  doc_freq_minority <- slam::col_sums(dtm_minority > 0)
  dtm[, doc_freq_minority >= min_docs, drop = FALSE]
}

remove_terms <- c("year", "years", as.character(2015:2022))
drop_terms <- function(dtm) dtm[, !colnames(dtm) %in% remove_terms, drop = FALSE]
drop_nonalpha_terms <- function(dtm) {
  has_letter <- grepl("[a-z]", colnames(dtm))
  dtm[, has_letter, drop = FALSE]
}
clean_dtm <- function(dtm) drop_nonalpha_terms(drop_terms(dtm))

# PRIMARY MODEL: skip-gram, minority-conditional sparsity floor

# --- 7. Build PRIMARY DTM ---
# --- 7. Build PRIMARY DTM ---
tic("Skip-gram DTM (raw)")
dtm.skip.raw <- DocumentTermMatrix(
  corpus_clean,
  control = list(tokenize = function(x) skipgram_tokenizer(x, n = 2, k = 2))
)
toc()
stopifnot(nrow(dtm.skip.raw) == length(is_removed))

dtm.skip.min <- clean_dtm(filter_by_minority_doc_freq(dtm.skip.raw, is_removed, min_docs_minority))
cat("\nPRIMARY DTM term count: skip-gram [minority floor] =", ncol(dtm.skip.min), "\n")

write.csv(data.frame(representation = "skip-gram", scheme = "minority", n_terms = ncol(dtm.skip.min)),
          file.path(DIR_DIAGNOSTICS, "diagnostic_dtm_term_counts.csv"), row.names = FALSE)

# --- 8. LASSO variable selection ---
# alpha = 0.99 (elastic net, ~1% ridge) to reduce collinearity risk in skip-
# gram features. Year forced in with zero penalty -> text coefficients are
# within-year. lambda.1se for parsimony. Binary family (logistic); can set
# FAMILY <- "multinomial" if a multi-class outcome gets added later.
FAMILY <- "binomial"

# Class imbalance note: removed comments are ~8.4% of the corpus. type.measure
# = "class" optimizes misclassification, which has a degenerate optimum here
# (predict "not_removed" for everyone). type.measure = "deviance" optimizes
# the likelihood instead and doesn't share that failure mode.
# Escalation path if deviance still selects a thin/empty model:
#   1. obs_weights below (upweight minority class, preserves N and base rate)
#   2. last resort: downsample non-removed comments, repeated across draws
LASSO_TYPE_MEASURE <- "deviance"

fit_lasso <- function(dtm, outcome, year, family = FAMILY, seed = 5710,
                      type_measure = LASSO_TYPE_MEASURE, use_weights = FALSE) {
  dtm_mat <- as.matrix(dtm)
  colnames(dtm_mat) <- make.names(colnames(dtm_mat), unique = TRUE)
  model_data <- data.frame(outcome = outcome, year = factor(year), dtm_mat, check.names = FALSE)
  X <- sparse.model.matrix(outcome ~ ., data = model_data)[, -1]
  year_cols <- grep("^year", colnames(X))
  penalty   <- rep(1, ncol(X)); penalty[year_cols] <- 0
  
  obs_weights <- NULL
  if (use_weights) {
    tab <- table(outcome)
    w   <- ifelse(outcome == names(tab)[which.max(tab)], 1, max(tab) / min(tab))
    obs_weights <- w / mean(w)
  }
  
  set.seed(seed)
  cvfit <- cv.glmnet(x = X, y = model_data$outcome, alpha = 0.99, family = family,
                     type.measure = type_measure, penalty.factor = penalty, weights = obs_weights)
  list(cvfit = cvfit, X = X, y = model_data$outcome, penalty = penalty)
}

tic("LASSO: skip-gram [PRIMARY: minority floor]")
lasso.skip.min <- fit_lasso(dtm.skip.min, outcome_vec, ce$year)
toc()

# --- 8b. LASSO diagnostics ---
png(file.path(DIR_FIG, "rq1a_removed_skip_cv_minority.png"), width = 1200, height = 900, res = 150)
plot(lasso.skip.min$cvfit)
title("LASSO CV: comment removal (skip-grams, minority floor)", line = 2.5)
dev.off()

# --- 9. Extract coefficients ---
extract_coefs <- function(lasso_obj, s = "lambda.1se") {
  cc <- coef(lasso_obj$cvfit, s = s)
  if (is.list(cc)) cc <- cc[["removed"]]
  idx   <- which(cc[, 1] != 0)
  terms <- rownames(cc)[idx]; vals <- cc[idx, 1]
  keep  <- !grepl("^\\(Intercept\\)$", terms)
  data.frame(term = terms[keep], coef = as.numeric(vals[keep]),
             is_year = grepl("^year[0-9]{4}$", terms[keep]),
             row.names = NULL) %>% arrange(desc(coef))
}

coef.skip.min <- extract_coefs(lasso.skip.min)

n_text <- function(cdf) sum(!cdf$is_year)
cat("\nPRIMARY non-zero TEXT terms selected at lambda.1se: skip-gram [minority] =", n_text(coef.skip.min), "\n")

top_terms <- function(cdf, n = 50) cdf %>% filter(!is_year, coef > 0) %>% slice_head(n = n)
top10.skip.min <- top_terms(coef.skip.min, 10)
top25.skip.min <- top_terms(coef.skip.min, 25)
top50.skip.min <- top_terms(coef.skip.min, 50)

cat("\nTop 10 skip-gram terms predicting REMOVAL [PRIMARY: minority floor]:\n")
print(top10.skip.min, row.names = FALSE)

write.csv(coef.skip.min,  file.path(DIR_PRIMARY, "primary_skip_min_coefs.csv"), row.names = FALSE)
write.csv(top50.skip.min, file.path(DIR_PRIMARY, "primary_skip_min_top50.csv"), row.names = FALSE)

# --- 10. Word clouds ---
make_wordcloud <- function(terms_df, fname, title_txt, scale_vals) {
  png(file.path(DIR_FIG, fname), width = 1400, height = 900, res = 200)
  par(mar = c(0, 0, 2, 0))
  set.seed(5710)
  wordcloud(terms_df$term, terms_df$coef, colors = brewer.pal(8, "OrRd")[3:8],
            scale = scale_vals, min.freq = 0, random.order = FALSE)
  title(title_txt, font.main = 2, cex.main = 1.0)
  dev.off()
}

make_wordcloud(top25.skip.min, "rq1a_removed_wordcloud_top25_minority.png",
               "Top 25 terms predicting removal - minority-conditional floor (PRIMARY)", c(5.2, 0.8))
make_wordcloud(top50.skip.min, "rq1a_removed_wordcloud_top50_minority.png",
               "Top 50 terms predicting removal - minority-conditional floor (PRIMARY)", c(4.8, 0.55))

# --- 11. Relaxed LASSO (triangulation) ---
run_relaxed_lasso <- function(coef_df, dtm, outcome_vec, year) {
  relax_terms <- make.names(top_terms(coef_df, n = nrow(coef_df))$term)
  mat <- as.matrix(dtm)
  colnames(mat) <- make.names(colnames(mat), unique = TRUE)
  relax_terms <- intersect(relax_terms, colnames(mat))
  relax_df <- data.frame(removed = as.integer(outcome_vec == "removed"), year = factor(year),
                         mat[, relax_terms, drop = FALSE], check.names = FALSE)
  fit <- glm(removed ~ ., data = relax_df, family = binomial())
  broom::tidy(fit) %>% filter(!grepl("^\\(Intercept\\)|^year", term)) %>% arrange(desc(estimate))
}

relax_out.min <- run_relaxed_lasso(coef.skip.min, dtm.skip.min, outcome_vec, ce$year)
write.csv(relax_out.min, file.path(DIR_PRIMARY, "primary_skip_min_relaxed.csv"), row.names = FALSE)

cat("\nTop 10 relaxed-LASSO terms [PRIMARY: minority floor]:\n")
print(head(relax_out.min, 10), row.names = FALSE)


triangulation.min <- top25.skip.min %>%
  select(term, lasso_coef = coef) %>% mutate(term = make.names(term)) %>%
  left_join(relax_out.min %>% select(term, relaxed_estimate = estimate, relaxed_p = p.value), by = "term")
write.csv(triangulation.min, file.path(DIR_DIAGNOSTICS, "diagnostic_triangulation_minority.csv"), row.names = FALSE)

# --- 12. Primary model dump ---
sink(file.path(DIR_TXT, "rq1a_removed_comment_language_dump.txt"))
cat("RQ1a — removed comment language\nFamily:", FAMILY, "| seed: 5710\n")
cat("Minority-conditional floor: >=", min_docs_minority, "of", n_removed, "removed comments\n")
cat("\n=== PRIMARY: skip-gram, minority-conditional floor ===\n")
cat("lambda.1se:", lasso.skip.min$cvfit$lambda.1se, "| text terms selected:", n_text(coef.skip.min), "\n\n")
print(coef.skip.min, row.names = FALSE)
cat("\n--- relaxed LASSO ---\n"); print(as.data.frame(relax_out.min), row.names = FALSE)
sink()

cat("\nPrimary model done. Exhibits in:", DIR_PRIMARY, "| figures in:", DIR_FIG, "\n")


# SENSITIVITY CHECKS (gated): whole-corpus skip-gram floor; n-gram and stem
# representations under both sparsity schemes

if (RUN_SENSITIVITY_CHECKS) {
  
  SPARSITY_FULL <- 0.01
  cat("\nWhole-corpus floor (sensitivity): >=", ceiling(SPARSITY_FULL * nrow(ce)),
      "of", nrow(ce), "comments\n\n")
  
  # --- 13. Build remaining DTMs ---
  tic("N-gram DTM (raw)")
  dtm.ngram.raw <- DocumentTermMatrix(corpus_clean, control = list(tokenize = function(x) ngram_tokenizer(x, 2)))
  toc()
  
  tic("Stem DTM (raw)")
  dtm.stem.raw <- DocumentTermMatrix(corpus_stem)
  toc()
  
  dtm.ngram.min  <- clean_dtm(filter_by_minority_doc_freq(dtm.ngram.raw, is_removed, min_docs_minority))
  dtm.stem.min   <- clean_dtm(filter_by_minority_doc_freq(dtm.stem.raw,  is_removed, min_docs_minority))
  dtm.skip.full  <- clean_dtm(removeSparseTerms(dtm.skip.raw,  1 - SPARSITY_FULL))
  dtm.ngram.full <- clean_dtm(removeSparseTerms(dtm.ngram.raw, 1 - SPARSITY_FULL))
  dtm.stem.full  <- clean_dtm(removeSparseTerms(dtm.stem.raw,  1 - SPARSITY_FULL))
  
  cat("\nSensitivity DTM term counts:\n")
  cat("  [minority floor]  ngram:", ncol(dtm.ngram.min), " stem:", ncol(dtm.stem.min), "\n")
  cat("  [whole-corpus 1%]  skip:", ncol(dtm.skip.full), " ngram:", ncol(dtm.ngram.full), " stem:", ncol(dtm.stem.full), "\n")
  
  dtm_term_counts <- data.frame(
    representation = c("skip-gram", "ngram", "stem", "skip-gram", "ngram", "stem"),
    scheme          = c("minority", "minority", "minority", "whole-corpus", "whole-corpus", "whole-corpus"),
    n_terms         = c(ncol(dtm.skip.min), ncol(dtm.ngram.min), ncol(dtm.stem.min),
                        ncol(dtm.skip.full), ncol(dtm.ngram.full), ncol(dtm.stem.full))
  )
  write.csv(dtm_term_counts, file.path(DIR_DIAGNOSTICS, "diagnostic_dtm_term_counts.csv"), row.names = FALSE)
  
  # --- 14. Vocabulary overlap diagnostic ---
  terms_min  <- colnames(dtm.skip.min)
  terms_full <- colnames(dtm.skip.full)
  cat("\nSkip-gram vocab overlap: in both =", length(intersect(terms_min, terms_full)),
      " | minority-only =", length(setdiff(terms_min, terms_full)),
      " | whole-corpus-only =", length(setdiff(terms_full, terms_min)), "\n")
  
  write.csv(
    data.frame(representation = "skip-gram",
               in_both = length(intersect(terms_min, terms_full)),
               minority_only = length(setdiff(terms_min, terms_full)),
               wholecorpus_only = length(setdiff(terms_full, terms_min))),
    file.path(DIR_DIAGNOSTICS, "diagnostic_vocab_overlap_summary.csv"), row.names = FALSE
  )
  
  # --- 15. LASSO variable selection (5 remaining models) ---
  tic("LASSO: n-gram [minority]");        lasso.ngram.min  <- fit_lasso(dtm.ngram.min,  outcome_vec, ce$year); toc()
  tic("LASSO: stem [minority]");          lasso.stem.min   <- fit_lasso(dtm.stem.min,   outcome_vec, ce$year); toc()
  tic("LASSO: skip-gram [whole-corpus]"); lasso.skip.full  <- fit_lasso(dtm.skip.full,  outcome_vec, ce$year); toc()
  tic("LASSO: n-gram [whole-corpus]");    lasso.ngram.full <- fit_lasso(dtm.ngram.full, outcome_vec, ce$year); toc()
  tic("LASSO: stem [whole-corpus]");      lasso.stem.full  <- fit_lasso(dtm.stem.full,  outcome_vec, ce$year); toc()
  
  # --- 15b. LASSO diagnostics ---
  png(file.path(DIR_FIG, "rq1a_removed_skip_cv_wholecorpus.png"), width = 1200, height = 900, res = 150)
  plot(lasso.skip.full$cvfit)
  title("LASSO CV: comment removal (skip-grams, whole-corpus floor)", line = 2.5)
  dev.off()
  
  # --- 16. Extract coefficients ---
  coef.ngram.min  <- extract_coefs(lasso.ngram.min)
  coef.stem.min   <- extract_coefs(lasso.stem.min)
  coef.skip.full  <- extract_coefs(lasso.skip.full)
  coef.ngram.full <- extract_coefs(lasso.ngram.full)
  coef.stem.full  <- extract_coefs(lasso.stem.full)
  
  cat("\nSensitivity non-zero TEXT terms selected at lambda.1se:\n")
  cat("  [minority]      ngram:", n_text(coef.ngram.min),  " stem:", n_text(coef.stem.min),  "\n")
  cat("  [whole-corpus]  skip:", n_text(coef.skip.full), " ngram:", n_text(coef.ngram.full), " stem:", n_text(coef.stem.full), "\n")
  
  top10.skip.full <- top_terms(coef.skip.full, 10)
  top25.skip.full <- top_terms(coef.skip.full, 25)
  top50.skip.full <- top_terms(coef.skip.full, 50)
  
  cat("\nTop 10 skip-gram terms predicting REMOVAL [whole-corpus, sensitivity]:\n")
  print(top10.skip.full, row.names = FALSE)
  
  write.csv(coef.ngram.min,  file.path(DIR_SENSITIVITY, "sensitivity_ngram_min_coefs.csv"),  row.names = FALSE)
  write.csv(coef.stem.min,   file.path(DIR_SENSITIVITY, "sensitivity_stem_min_coefs.csv"),   row.names = FALSE)
  write.csv(coef.skip.full,  file.path(DIR_SENSITIVITY, "sensitivity_skip_full_coefs.csv"),  row.names = FALSE)
  write.csv(coef.ngram.full, file.path(DIR_SENSITIVITY, "sensitivity_ngram_full_coefs.csv"), row.names = FALSE)
  write.csv(coef.stem.full,  file.path(DIR_SENSITIVITY, "sensitivity_stem_full_coefs.csv"),  row.names = FALSE)
  write.csv(top50.skip.full, file.path(DIR_SENSITIVITY, "sensitivity_skip_full_top50.csv"),  row.names = FALSE)
  
  # --- 17. Distinct vocabulary: candidates vs. what LASSO actually selected ---
  name_map_min  <- setNames(make.names(colnames(dtm.skip.min),  unique = TRUE), colnames(dtm.skip.min))
  name_map_full <- setNames(make.names(colnames(dtm.skip.full), unique = TRUE), colnames(dtm.skip.full))
  
  minority_only_raw    <- sort(setdiff(terms_min, terms_full))
  wholecorpus_only_raw <- sort(setdiff(terms_full, terms_min))
  
  minority_only_selected <- coef.skip.min %>%
    filter(term %in% name_map_min[minority_only_raw]) %>% arrange(desc(coef))
  wholecorpus_only_selected <- coef.skip.full %>%
    filter(term %in% name_map_full[wholecorpus_only_raw]) %>% arrange(desc(coef))
  
  cat("\nMinority-only terms LASSO selected (", nrow(minority_only_selected), "of", length(minority_only_raw), "):\n")
  print(minority_only_selected, row.names = FALSE)
  cat("\nWhole-corpus-only terms LASSO selected (", nrow(wholecorpus_only_selected), "of", length(wholecorpus_only_raw), "):\n")
  print(wholecorpus_only_selected, row.names = FALSE)
  
  write.csv(data.frame(term = minority_only_raw),    file.path(DIR_DIAGNOSTICS, "diagnostic_minority_only_candidate_terms.csv"),    row.names = FALSE)
  write.csv(data.frame(term = wholecorpus_only_raw), file.path(DIR_DIAGNOSTICS, "diagnostic_wholecorpus_only_candidate_terms.csv"), row.names = FALSE)
  write.csv(minority_only_selected,    file.path(DIR_DIAGNOSTICS, "diagnostic_minority_only_selected_by_lasso.csv"),    row.names = FALSE)
  write.csv(wholecorpus_only_selected, file.path(DIR_DIAGNOSTICS, "diagnostic_wholecorpus_only_selected_by_lasso.csv"), row.names = FALSE)
  
  # --- 18. Word clouds (whole-corpus skip-gram) ---
  make_wordcloud(top25.skip.full, "rq1a_removed_wordcloud_top25_wholecorpus.png",
                 "Top 25 terms predicting removal - whole-corpus 1% floor (sensitivity)", c(5.2, 0.8))
  make_wordcloud(top50.skip.full, "rq1a_removed_wordcloud_top50_wholecorpus.png",
                 "Top 50 terms predicting removal - whole-corpus 1% floor (sensitivity)", c(4.8, 0.55))
  
  # --- 19. Relaxed LASSO + cross-scheme comparison ---
  relax_out.full <- run_relaxed_lasso(coef.skip.full, dtm.skip.full, outcome_vec, ce$year)
  write.csv(relax_out.full, file.path(DIR_SENSITIVITY, "sensitivity_skip_full_relaxed.csv"), row.names = FALSE)
  cat("\nTop 10 relaxed-LASSO terms [whole-corpus, sensitivity]:\n")
  print(head(relax_out.full, 10), row.names = FALSE)
  
  # This is the top-25-only LASSO-positive comparison; the broader vocabulary-
  # level comparison (all candidates, both signs) is the pair of *_selected
  # tables in step 17 above.
  scheme_compare <- full_join(
    top25.skip.min  %>% select(term, coef_minority    = coef),
    top25.skip.full %>% select(term, coef_wholecorpus = coef),
    by = "term"
  )
  write.csv(scheme_compare, file.path(DIR_SENSITIVITY, "sensitivity_sparsity_scheme_comparison_top25.csv"), row.names = FALSE)
  cat("\nTerms in minority-conditional top 25 NOT in whole-corpus top 25:\n")
  print(scheme_compare %>% filter(!is.na(coef_minority), is.na(coef_wholecorpus)) %>% pull(term))
  
  # --- 20. Append sensitivity results to the model dump ---
  sink(file.path(DIR_TXT, "rq1a_removed_comment_language_dump.txt"), append = TRUE)
  cat("\n\n=== SENSITIVITY CHECK: whole-corpus 1% floor ===\n")
  cat("Whole-corpus floor (sensitivity): >=", ceiling(SPARSITY_FULL * nrow(ce)), "of", nrow(ce), "comments\n")
  cat("skip-gram lambda.1se:", lasso.skip.full$cvfit$lambda.1se, "| text terms selected:", n_text(coef.skip.full), "\n\n")
  print(coef.skip.full, row.names = FALSE)
  cat("\n--- relaxed LASSO ---\n"); print(as.data.frame(relax_out.full), row.names = FALSE)
  
  cat("\n\n=== ROBUSTNESS: n-gram and stem (both schemes) ===\n")
  cat("\n-- n-gram [minority] --\n");      print(coef.ngram.min,  row.names = FALSE)
  cat("\n-- n-gram [whole-corpus] --\n");  print(coef.ngram.full, row.names = FALSE)
  cat("\n-- stem [minority] --\n");        print(coef.stem.min,   row.names = FALSE)
  cat("\n-- stem [whole-corpus] --\n");    print(coef.stem.full,  row.names = FALSE)
  sink()
  
  cat("\nSensitivity checks done. Exhibits in:", DIR_SENSITIVITY, "and", DIR_DIAGNOSTICS, "| figures in:", DIR_FIG, "\n")
  
} else {
  cat("\nRUN_SENSITIVITY_CHECKS is FALSE - skipped whole-corpus/ngram/stem sensitivity checks.\n")
}