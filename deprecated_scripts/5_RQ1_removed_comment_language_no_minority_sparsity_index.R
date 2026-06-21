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

# --- 6. Build DTMs ---
# Sparsity at 1% (terms must appear in >= 1% of documents). With ~117k docs
# that 1% floor (~1,168 docs) is a strict filter - a term must appear in at least
# ~1,000 docs to be included. This it yields a compact, stable feature set. 
# Bump SPARSITY down (e.g. 0.005) if you want a richer vocabulary

SPARSITY <- 0.01 # Potentially changed to 0.005

tic("Skip-gram DTM")
dtm.skip <- DocumentTermMatrix(
  corpus_clean,
  control = list(tokenize = function(x) skipgram_tokenizer(x, n = 2, k = 2))
) |> removeSparseTerms(1 - SPARSITY)
toc()

tic("N-gram DTM")
dtm.ngram <- DocumentTermMatrix(
  corpus_clean,
  control = list(tokenize = function(x) ngram_tokenizer(x, 2))
) |> removeSparseTerms(1 - SPARSITY)
toc()

tic("Stem DTM")
dtm.stem <- DocumentTermMatrix(corpus_stem) |> removeSparseTerms(1 - SPARSITY)
toc()

# Remove, e.g. "2016" from the text corpus so are not misinterpreted
# as year fixed effects.
remove_terms <- c("year", "years", as.character(2015:2022))
drop_terms <- function(dtm) dtm[, !colnames(dtm) %in% remove_terms]
dtm.skip  <- drop_terms(dtm.skip)
dtm.ngram <- drop_terms(dtm.ngram)
dtm.stem  <- drop_terms(dtm.stem)

cat("\nDTM term counts (sparsity =", SPARSITY, "):\n")
cat("  skip-gram:", ncol(dtm.skip),  "terms\n")
cat("  n-gram:   ", ncol(dtm.ngram), "terms\n")
cat("  stem:     ", ncol(dtm.stem),  "terms\n")


# Drop punctuation-only tokens (e.g. a bare "..." from retained ellipses).
# These aren't meaningful vocabulary, and some (literally "...", "..1", "..2")
# are reserved symbols in R that break formula parsing if left as column names.
drop_nonalpha_terms <- function(dtm) {
  has_letter <- grepl("[a-z]", colnames(dtm))
  dtm[, has_letter, drop = FALSE]
}
dtm.skip  <- drop_nonalpha_terms(dtm.skip)
dtm.ngram <- drop_nonalpha_terms(dtm.ngram)
dtm.stem  <- drop_nonalpha_terms(dtm.stem)

cat("\nDTM term counts after dropping punctuation-only tokens:\n")
cat("  skip-gram:", ncol(dtm.skip),  "terms\n")
cat("  n-gram:   ", ncol(dtm.ngram), "terms\n")
cat("  stem:     ", ncol(dtm.stem),  "terms\n")


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

# Run LASSO 
tic("LASSO: skip-gram")
lasso.skip  <- fit_lasso(dtm.skip,  outcome_vec, ce$year)
toc()

tic("LASSO: n-gram")
lasso.ngram <- fit_lasso(dtm.ngram, outcome_vec, ce$year)
toc()

tic("LASSO: stem")
lasso.stem  <- fit_lasso(dtm.stem,  outcome_vec, ce$year)
toc()


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

coef.skip  <- extract_coefs(lasso.skip)
coef.ngram <- extract_coefs(lasso.ngram)
coef.stem  <- extract_coefs(lasso.stem)

n_text <- function(cdf) sum(!cdf$is_year)
cat("\nNon-zero TEXT terms selected at lambda.1se:\n")
cat("  skip-gram:", n_text(coef.skip),  "\n")
cat("  n-gram:   ", n_text(coef.ngram), "\n")
cat("  stem:     ", n_text(coef.stem),  "\n")

# Top-N tables (primary = skip-gram). Positive coef = predicts REMOVAL.
top_terms <- function(cdf, n = 50) {
  cdf %>% filter(!is_year, coef > 0) %>% slice_head(n = n)
}
top10.skip <- top_terms(coef.skip, 10)
top25.skip <- top_terms(coef.skip, 25)
top50.skip <- top_terms(coef.skip, 50)

cat("\nTop 10 skip-gram terms predicting REMOVAL:\n")
print(top10.skip, row.names = FALSE)

# Save coefficient exhibits
write.csv(coef.skip,  file.path(DIR_MAIN, "rq1a_removed_skip_coefs.csv"),  row.names = FALSE)
write.csv(coef.ngram, file.path(DIR_MAIN, "rq1a_removed_ngram_coefs.csv"), row.names = FALSE)
write.csv(coef.stem,  file.path(DIR_MAIN, "rq1a_removed_stem_coefs.csv"),  row.names = FALSE)
write.csv(top50.skip, file.path(DIR_MAIN, "rq1a_removed_skip_top50.csv"),  row.names = FALSE)

# --- 9. Word clouds ---

# Top 25 removed comments
png(file.path(DIR_FIG, "rq1a_removed_wordcloud_top25.png"),
    width = 1400, height = 900, res = 200)
par(mar = c(0, 0, 2, 0))
set.seed(5710)
wordcloud(
  words      = top25.skip$term,
  freq       = top25.skip$coef,
  colors     = brewer.pal(8, "OrRd")[3:8],
  scale = c(5.2, 0.8),
  min.freq   = 0,
  random.order = FALSE
)
title("Top 25 skip-gram terms predicting comment removal (Rule 2)",
      font.main = 2, cex.main = 1.0)
dev.off()

# Top 50-terms cloud
png(file.path(DIR_FIG, "rq1a_removed_wordcloud_top50.png"),
    width = 1400, height = 900, res = 200)
par(mar = c(0, 0, 2, 0))
set.seed(5710)
wordcloud(top50.skip$term, top50.skip$coef,
          colors = brewer.pal(8, "OrRd")[3:8],
          scale = c(4.8, 0.55), min.freq = 0, random.order = FALSE)
title("Top 50 skip-gram terms predicting comment removal (Rule 2)",
      font.main = 2, cex.main = 1.0)
dev.off()

# CV plot for the primary model
png(file.path(DIR_FIG, "rq1a_removed_skip_cv.png"),
    width = 1200, height = 900, res = 150)
plot(lasso.skip$cvfit)
title("LASSO CV: comment removal (skip-grams)", line = 2.5)
dev.off()

# --- 10. Relaxed LASSO (triangulation) ---
# Refit an unregularized logistic GLM on the LASSO-selected text terms (plus
# year FEs) to get less-biased, more interpretable log-odds and Wald SEs.
# Positive estimate for a term term raises the odds the comment was removed.
relax_terms <- top_terms(coef.skip, n = nrow(coef.skip))$term   # all selected text terms
relax_terms <- make.names(relax_terms)                          # safe column names

skip_mat <- as.matrix(dtm.skip)
colnames(skip_mat) <- make.names(colnames(skip_mat))
relax_terms <- intersect(relax_terms, colnames(skip_mat))

relax_df <- data.frame(
  removed = as.integer(outcome_vec == "removed"),
  year    = factor(ce$year),
  skip_mat[, relax_terms, drop = FALSE],
  check.names = FALSE
)

relax_fit <- glm(removed ~ ., data = relax_df, family = binomial())

relax_out <- broom::tidy(relax_fit) %>%
  filter(!grepl("^\\(Intercept\\)|^year", term)) %>%
  arrange(desc(estimate))

write.csv(relax_out, file.path(DIR_MAIN, "rq1a_removed_skip_relaxed.csv"), row.names = FALSE)

cat("\nTop 10 relaxed-LASSO terms (log-odds of removal):\n")
print(head(relax_out, 10), row.names = FALSE)

# Triangulation check: do LASSO and relaxed agree on the headline terms?
triangulation <- top25.skip %>%
  select(term, lasso_coef = coef) %>%
  mutate(term = make.names(term)) %>%
  left_join(relax_out %>% select(term, relaxed_estimate = estimate, relaxed_p = p.value),
            by = "term")
write.csv(triangulation, file.path(DIR_MAIN, "rq1a_removed_triangulation.csv"), row.names = FALSE)



# --- 11. Full model dumps ---
sink(file.path(DIR_TXT, "rq1a_removed_comment_language_dump.txt"))
cat("RQ1a — removed comment language\n")
cat("Family:", FAMILY, "| sparsity:", SPARSITY, "| seed: 5710\n")
cat("\n--- skip-gram lambda.1se:", lasso.skip$cvfit$lambda.1se, "---\n")
cat("text terms selected:", n_text(coef.skip), "\n\n")
print(coef.skip, row.names = FALSE)
cat("\n--- relaxed LASSO (skip-gram terms) ---\n")
print(as.data.frame(relax_out), row.names = FALSE)   # was: print(relax_out, row.names = FALSE)
sink()

cat("\nDone. Exhibits in:", DIR_MAIN, "| figures in:", DIR_FIG, "\n")


