# Political incivility and moderator removal on r/ChangeMyView

Observational analysis of the ConvoKit **Conversations Gone Awry – CMV (large)** corpus. The project asks whether moderators on r/ChangeMyView remove political incivility at a lower threshold than non-political incivility, using Rule 2 removals ("Don't be rude or hostile to other users") as the outcome.

It is the observational companion to a survey experiment (N = 254 U.S. partisans). In that experiment, political content was judged a violation far more often than non-political content at the civil and borderline civility levels, but not when content was clearly uncivil. This repo tests whether the same pattern shows up in real moderation decisions.

**Status (October 2026):** descriptive analysis of the corpus is complete (scripts 1–6). The next phase builds comment-level measures of incivility and political content, validated against human coders.

---

## Research questions

| | Question |
|---|---|
| **RQ1** | What topics are discussed on r/CMV? (Maps the topic domains that RQ3 needs.) |
| **RQ2** | What explains which comments are removed? (Factors that vary within a pair: user tenure and reputation, depth, timing, speakers, length, quoting.) |
| **RQ3** | Is political incivility removed at a lower threshold than non-political incivility? |
| **RQ4** | Is removal driven by content, thread dynamics, users or the network? |

RQ3 hypotheses:

- **H3a:** removed comments in political contexts are less uncivil (by a content-based civility measure) than removed comments elsewhere.
- **H3b (within pairs):** the incivility gap between a removed comment and its on-track partner is smaller in explicitly political contexts.
- **H3c:** the political effect is concentrated in civil and borderline comments; clearly uncivil comments are removed regardless of topic.

---

## Data

**Source:** `conversations-gone-awry-cmv-corpus-large` (ConvoKit, Cornell), the corpus used by Tran et al. (2025). It was exported to CSV in a separate extraction repo (`01_export_to_csv.py`).

| | Full corpus | Analysis sample (post ID) |
|---|---|---|
| Period | 2015–2022 | Aug 2017–Dec 2022 |
| Pairs | 9,789 | 8,150 |
| Paths | 19,578 | 16,300 |
| Comments | 116,793 | 96,203 |
| Threads | not identifiable | 6,440 |
| Identifiable users | 24,554 | 20,462 |

**How the corpus is built**

- **Paths.** Each path is a linear chain of comments, starting from a first reply to a post. Paths come in pairs from the same thread:
  - one path is **awry**: its final comment was removed under Rule 2;
  - the other is **on-track**: no comment was removed.
- **Pairing.** The pairing holds the thread (and so its topic) constant. Pairs are also roughly matched on length.
- **The removed comment** is always the final comment of an awry path, and its text is intact.
- **Thread IDs** are recovered from comment permalinks, which only carry them from August 2017. Analyses that group by thread or topic therefore use the post-ID sample. Most scripts default to it.

**Terminology:** thread, first reply, challenger, path, pair, path ending. These are defined at the top of `2_descriptives_conversations.R`. ConvoKit's "utterances" are called "comments" throughout.

---

## Pipeline

Run the scripts from the project folder (where `CGA_CMV_Pol_Comments.Rproj` is), in this order:

| Script | What it does | Reads | Writes |
|---|---|---|---|
| `1_data_prep.R` | Loads the exported CSVs, fixes types, derives variables (post ID, depth, word counts, sample flags). | `input_data_from_python/*.csv` | `prepped_data/*.rds` |
| `1b_pseudonymize_users.R` | Replaces usernames with ranked IDs (`user_00001` = most active) in every file and in `u/name` mentions in the text. | `input_data_from_python/`, `prepped_data/` | `input_data_anonymized/`, `prepped_data_anonymized/`, private key |
| `2_descriptives_conversations.R` | Structure of threads, pairs and paths; within-pair differences; timing; participation; who writes the final comment. | `prepped_data_anonymized/` | `*/conversations/` |
| `3_descriptives_users.R` | Activity concentration, challengers, removal concentration, removal rates (users with 25+ comments), delta flair, `[deleted]` accounts. Saves the user table. | `prepped_data_anonymized/` | `*/users/`, `input_data_anonymized/user_table_postid.csv` |
| `3b_descriptives_removal_authors.R` | Opportunity-adjusted removal: the removed share of each user's path endings against the 50% expected by design. | `prepped_data_anonymized/` | `*/removal_authors/` |
| `4_descriptives_content.R` | Text cleaning (HTML entities, quoted paragraphs and their sources), length, quoting, deltas in the text. | `prepped_data_anonymized/` | `*/content/`, `prepped_data_anonymized/comments_text_clean.rds` |
| `5_topics.py` | Inductive thread topics: post-title slugs embedded (all-mpnet-base-v2), reduced with UMAP and clustered with k-means. Uses k = 15, chosen from a k = 5–30 sweep by silhouette and stability across seeds. Writes a blank coding sheet. | `input_data_anonymized/` | `prepped_data_anonymized/thread_topics.csv`, `*/topics/` |
| `5b_descriptives_topics.R` | Topic sizes, topics over time, within-pair differences by topic and political coding, and where high-removal users post. | thread topics, filled coding sheet, user table | `*/topics/` |
| `6_descriptives_networks.R` | Reply network (repeat encounters within and across threads), user × topic specialisation, and topic × topic links, each against a shuffled baseline. | `prepped_data_anonymized/`, thread topics | `*/networks/` |

`*/` stands for both `descriptive_results/` (CSV tables) and `graphical_output/` (PNG figures).

**Dependencies between scripts**

- `3b` does not depend on `3`.
- `5b` needs `3` (for the user table), `4` and `5`.
- `6` needs `5` for sections 2–3.

**Topic coding (manual step between `5_topics.py` and `5b`)**

1. Copy `descriptive_results/topics/topic_coding_sheet_k15_BLANK.csv` to `topic_coding_sheet_k15.csv` (or `.xlsx`) in the same folder.
2. Fill in `topic_name`, `political` (Explicit / Implicit) and `political_subtype` (Actors and institutions / Identity-based / Policy issues / Moral and lifestyle).
3. The blank sheet is overwritten on every run of `5_topics.py`; your filled copy is never touched.
4. `5b` and `6` find the filled sheet automatically.

**Tables and figures.** Figures group long tails for readability, and every grouped figure has a matching `*_uncapped` or `*_detailed` table. Most file names end in `_postid` (the analysis sample) or `_full` (the full corpus).

---

## Folder structure

```
.
├── CGA_CMV_Pol_Comments.Rproj
├── README.md
├── .gitignore
├── 1_data_prep.R ... 6_descriptives_networks.R, 5_topics.py
│
├── input_data_from_python/     # LOCAL ONLY: exported CSVs with usernames, private ID key
├── prepped_data/               # LOCAL ONLY: prepped .rds with usernames
├── input_data_anonymized/      # shareable: CSVs with user IDs, user table
├── prepped_data_anonymized/    # shareable: prepped .rds with user IDs, cleaned text, thread topics
│
├── descriptive_results/        # CSV tables, one subfolder per script
│   ├── conversations/  users/  removal_authors/  content/  topics/  networks/
├── graphical_output/           # PNG figures, same subfolders
│
├── study_exhibits/             # schematics and tables not produced by the scripts
└── deprecated_scripts/         # retired code kept for reference
```

---

## Data handling

- **Pseudonymisation, not anonymisation.** Usernames are replaced with ranked IDs before any shareable file is written. Comment text, comment and conversation IDs, and permalinks still link back to Reddit, so users can still be re-identified.
- **Kept out of the repo:**
  - `input_data_from_python/`, `prepped_data/` and `PRIVATE_user_id_key.csv` (the username ↔ ID key);
  - `.env` (an optional OpenAI key for `5_topics.py`);
  - the embedding cache in `prepped_data_anonymized/topic_embeddings/`.
- **Aggregate reporting only.** Results are reported for groups. No individual user is named or singled out in any table, figure or write-up.

---

## Setup

**R** (RStudio). Packages load through `pacman::p_load()` at the top of each script:

- `tidyverse`, `lubridate`, `ggpubr`, `patchwork`, `scales`, `igraph`, `Matrix`
- `readxl`, if the coding sheet is saved as `.xlsx`

**Python** (for `5_topics.py`). Use a virtual environment:

```bash
pip install pandas numpy scikit-learn umap-learn sentence-transformers matplotlib scipy
pip install wordcloud   # optional
python 5_topics.py
```

- The first run downloads the embedding model and is slow without a GPU. Later runs reuse the cached embeddings.
- Set `CONFIG["run_sweep"] = False` to skip the k sweep when comparing k values.

---

## Design notes and caveats

- **Pair design.** Both paths in a pair share a thread, so thread-level factors such as topic cannot predict which path is removed. They can only enter as interactions.
  - The corpus contains a removal in every thread by construction, so it cannot show whether political threads produce more removals.
  - It can show whether removed comments in political contexts are less uncivil: the threshold question.
- **Only final comments can be removed.** Removal rates per comment partly reflect how often a user writes a path's final comment. `3b` adjusts for this.
- **Removal rates** are only reported for users with 25+ comments. At that volume, an average user has about a 5% chance of showing double the average rate by chance.
- **Comment scores** are set aside: they were collected at different comment ages, cut short by removal, skewed and post-treatment.
- **Only sampled paths are observed.** Unsampled branches, the original post and its author are not in the data, so repeat encounters between users are lower bounds.
- **Timing of pairs.** The papers describe pairing on post, length and class balance but do not report a timing criterion. Start-time differences are therefore interpreted within pairs only.
- **`[deleted]`** covers an unknown number of accounts and is excluded from user-level analyses (described separately in `3`).

---

## References

- Zhang, J., Chang, J. P., Danescu-Niculescu-Mizil, C., Dixon, L., Hua, Y., Taraborelli, D., & Thain, N. (2018). Conversations gone awry: Detecting early signs of conversational failure. *ACL*.
- Chang, J. P., & Danescu-Niculescu-Mizil, C. (2019). Trouble on the horizon: Forecasting the derailment of online conversations as they develop. *EMNLP*.
- Altarawneh, E., Agrawal, A., Jenkin, M., & Papagelis, M. (2023). Conversation derailment forecasting with graph convolutional networks. *WOAH*.
- Hua, Y., Chernogor, N., Gu, Y., Jeong, S., Luo, M., & Danescu-Niculescu-Mizil, C. (2024). How did we get here? Summarizing conversation dynamics. *NAACL*.
- Tran, S. Q., Gangavarapu, T., Chernogor, N., Chang, J. P., & Danescu-Niculescu-Mizil, C. (2025). Conversations gone awry, but then? Evaluating conversational forecasting models.
- Tan, C., Niculae, V., Danescu-Niculescu-Mizil, C., & Lee, L. (2016). Winning arguments: Interaction dynamics and persuasion strategies in good-faith online discussions. *WWW*.
- Burnham, M., Kahn, K., Wang, R., & Peng, R. (2026). Political DEBATE: Efficient zero-shot and few-shot classifiers for political text. *Political Analysis*, 34(3).
- Gervais, B.T., Dye, C. and Chin, A. (2025) ‘Incivility or Invalidity? Evaluating Perspective API Scores as a Measure of Political Incivility’, American Politics Research, 53(3), pp. 266–274. 