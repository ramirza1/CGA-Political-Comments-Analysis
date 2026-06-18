# Conversations Gone Awry (CMV) — Political Comments Analysis

Observational analysis of the ConvoKit **Conversations Gone Awry – CMV (Large)** corpus, examining whether political conversations on Reddit's r/ChangeMyView are disproportionately associated with moderator-identified derailment (removal of the final comment for a Rule 2 civility violation).

This sits within a broader programme of work on Reddit moderation and discourse quality. It is the observational companion to a survey-experimental moderation study; see the concept note for how the two relate.

---

## Research question

Are political conversations more likely than non-political conversations to end in moderator-identified derailment, and is any such association explained by observable toxicity?

The analytical strategy, in brief:
1. Identify the words/phrases most predictive of a conversation going awry (text mining + LASSO), mirroring the approach developed in the *partisan disliking* project.
2. Assess how many of those predictive terms are political in nature.
3. (Downstream, not in initial scope) Model whether political content raises the odds of Rule 2 removal, with and without controlling for toxicity scores.

Findings are descriptive, not causal. The corpus is observational, somewhat dated (≈2015–2022), and lacks key confounders (user history, moderator attention, reporting behaviour, moderator identity/affiliation).

---

## Data

Source corpus: `conversations-gone-awry-cmv-corpus-large` (ConvoKit / Cornell).
- ~19,578 conversations (paired 50/50: 9,789 awry, 9,789 on-track)
- ~116,793 comments
- ~24,555 speakers

The corpus was downloaded and flattened to three CSVs in a prior Python/ConvoKit stage (see the separate extraction repo). Those CSVs are the **input** to this repo and live in `Input_data_from_python/`:

| File | Grain | Key columns |
|---|---|---|
| `comments.csv` | one row per comment | `comment_id`, `conversation_id`, `speaker`, `reply_to`, `timestamp`, `text`, `score`, `top_level_comment`, `gilded`, `stickied`, `author_flair_text` |
| `conversations.csv` | one row per conversation | `conversation_id`, `pair_id`, `has_removed_comment`, `split`, `n_comments`, `has_human_summary`, `has_machine_summary` |
| `speakers.csv` | one row per user | `speaker_id`, `n_comments` |

Grouping keys: `conversation_id` groups all comments in a conversation; `reply_to` gives the direct parent comment (thread tree); `pair_id` links each awry conversation to its matched on-track conversation. `has_removed_comment` is the primary outcome variable.

> **Terminology:** ConvoKit calls comments "utterances" internally. This repo uses "comments" throughout for readability in the Reddit context.

The raw CSVs and prepped `.rds` files are **gitignored** (large, and reproducible from the Python export plus these scripts). The folder structure is preserved via `.gitkeep`.

---

## Repository structure

```
.
├── CGA_CMV_Pol_Comments.Rproj
├── .gitignore
├── README.md
│
├── Input_data_from_python/      # comments.csv, conversations.csv, speakers.csv (gitignored)
├── Derived_data/                # joined/analysis-ready .rds tables built by script 1 (gitignored)
│
├── 1. Data_prep.R               # load CSVs, join, build prepped tables, cache to prepped_data/
├── 2. Descriptives.R            # descriptive tables + figures, by theme
├── 3. Text_mining.R             # skip-grams + LASSO: terms predictive of "awry"
├── 4. Political_tagging.R       # classify predictive terms as political / non-political
├── 5. Network_structure.R       # (optional) user-to-user reply-tie statistics
│
├── Descriptive_results/         # csv tables, one subfolder per theme
│   ├── basic_corpus/  temporal/  user_participation/  thread_structure/
│   ├── scores/  text_length/  missingness/  removed_content/  network/
│
├── Graphical_output/            # figures, mirroring the descriptive themes
├── Main_results/          # csv exhibits destined for the writeup
├── Text_outputs/                # full model dumps (.txt)
├── Study_exhibits/              # schematics, info tables (not from analysis)
└── Deprecated_scripts/          # retired code kept for reference
```

Script numbering and the root-level `.Rproj` follow the convention from the Reddit moderation study repo. The eventual writeup / Overleaf (`.tex`) architecture will also live at root, produced by a later script.

---

## Planned pipeline

`1. Data_prep.R` — read the three CSVs, coerce types (timestamp → datetime, logicals, factors), join into the dataframes each downstream stage needs (e.g. comments enriched with conversation-level outcome; conversation-level summary table). Cache everything to `prepped_data/*.rds` so later scripts load instantly and never re-join.

`2. Descriptives.R` — the descriptive sweep, organised by theme (basic corpus, temporal coverage, user participation, thread structure, scores, text length, missingness, removed/stickied content). Tables to `Descriptive_results/<theme>/`, figures to `Graphical_output/<theme>/`. Not all of this feeds the political-comments paper; the aim is a reusable picture of the corpus.

`3. Text_mining.R` — port the skip-gram + multinomial/binary LASSO approach from the *partisan disliking* project. Here the outcome is `has_removed_comment` (awry vs on-track) rather than a feeling thermometer. Output: ranked terms most predictive of derailment.

`4. Political_tagging.R` — classify the predictive terms as political vs non-political and quantify how much of the "awry" signal is political. Tagging method (keyword list / political lexicon / LLM-assisted coding) to be decided.

`5. Network_structure.R` — optional/standalone: reply-tie counts, unique repliers/replied-to, top users by ties made/received.

---

## Environment

Analysis is in **R**, run locally (RStudio). Package management via `pacman::p_load(...)` at the top of each script, mirroring prior projects. Anticipated core packages: `tidyverse`, `data.table`, `tidytext`, `tm`, `SnowballC`, `glmnet`, `nnet`, `sandwich`, `ggplot2`, `wordcloud`. (A `renv` lockfile may be added later for reproducibility.)

Downstream work (not in initial scope) will add toxicity scores via Jigsaw's Perspective API or an alternative classifier.

---

## Ethics & data handling

The corpus contains **real Reddit usernames**, not anonymised IDs. All outputs are reported in **aggregate only**; individual usernames are never surfaced in tables, figures, or the writeup. The dataset is public and was collected under Reddit's terms; analysis is limited to patterns across conversations, not profiling of individuals.

---

## References

- Zhang et al. (2018). *Conversations Gone Awry: Detecting Early Signs of Conversational Failure.* ACL. https://arxiv.org/abs/1805.05345
- Chang & Danescu-Niculescu-Mizil (2019). *Trouble on the Horizon: Forecasting the Derailment of Online Conversations as they Develop.* EMNLP.
- Hua et al. (2024). *How Did We Get Here? Summarizing Conversation Dynamics.* NAACL.
