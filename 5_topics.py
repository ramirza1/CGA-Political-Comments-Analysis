"""
5_topics.py for Conversations Gone Awry (CMV) - Political Comments Analysis
==========================================================================

Inductive thread topics: cluster the post titles (URL slugs) of the threads in the
corpus. Adapted from map_the_field.py (COMM 5000): embed -> UMAP -> k-means with a
k sweep -> label clusters with their top terms -> read and name them by hand.

Unit: one row per thread (post), not per pair or path, so threads that contribute
several pairs are not counted more than once. Pair and path counts are kept as columns.

Title text comes from the permalink slug: /r/changemyview/comments/<post_id>/<slug>/...
Reddit lowercases slugs, replaces punctuation with "_" and cuts long titles short, so
this is a shortened title. Slugs exist only for paths with a post ID (Aug 2017 onward).

Changes from map_the_field.py:
  - Input: one short title per thread, so no chunking.
  - "CMV:" prefix stripped; meta posts flagged (prefix column).
  - Stopwords: CMV title boilerplate instead of seminar filler (used for labels only;
    the full title is embedded).
  - Posts whose title was replaced on deletion ("deleted_by_user") are set aside and counted.
  - Cluster labels use class-based TF-IDF (terms distinctive of a cluster compared with
    the other clusters), which works better than averaged TF-IDF on very short texts.
  - k sweep reports silhouette AND stability: the clustering is re-run with several
    UMAP seeds and the agreement between runs (adjusted Rand index) is averaged.
    Silhouette on UMAP output tends to look better than the clustering really is, and
    short texts cluster less cleanly, so a k that is not stable across seeds should not
    be trusted even if its silhouette is high.
  - Representative titles per cluster (closest to the cluster centre) plus random
    titles, written to a coding sheet for naming clusters and marking them political.
  - Dendrogram of cluster centres, to see which clusters could merge into broader themes.
  - Embeddings are cached, so changing k does not re-embed.

Workflow:
  1. Run once with the default k and look at topic_k_sweep.png.
  2. Set CONFIG["k_clusters"] to a k that is both stable and readable; re-run. To compare
     a few k values quickly, set CONFIG["run_sweep"] = False (the sweep only needs to run
     once per set of titles).
  3. Copy topic_coding_sheet_k<K>_BLANK.csv to topic_coding_k<K>.csv (or .xlsx) and fill in
     topic_name, political (e.g. Explicit / Implicit) and political_subtype. The blank
     sheet is overwritten on every run; the filled copy is never touched.
  4. Run 5b_descriptives_topics.R (and 6_descriptives_networks.R).

Run from the project folder (where CGA_CMV_Pol_Comments.Rproj is):
    pip install pandas numpy scikit-learn umap-learn sentence-transformers matplotlib scipy
    pip install wordcloud            # optional
    python 5_topics.py

Inputs:   input_data_anonymized/comments.csv, input_data_anonymized/conversations.csv
Outputs:  prepped_data_anonymized/thread_topics.csv        (one row per thread, for R)
          descriptive_results/topics/*.csv, *.txt
          graphical_output/topics/*.png
"""

from __future__ import annotations

import hashlib
import re
import sys
import warnings
from itertools import combinations
from pathlib import Path
from typing import Dict, List, Tuple

import numpy as np
import pandas as pd

warnings.filterwarnings("ignore", category=FutureWarning)
warnings.filterwarnings("ignore", category=UserWarning)

# -----------------------------------------------------------------------------
# CONFIG
# -----------------------------------------------------------------------------
BASE_DIR = Path(__file__).resolve().parent

CONFIG = {
    # I/O (relative to the project folder)
    "comments_csv": BASE_DIR / "input_data_anonymized" / "comments.csv",
    "conversations_csv": BASE_DIR / "input_data_anonymized" / "conversations.csv",
    "topics_out": BASE_DIR / "prepped_data_anonymized" / "thread_topics.csv",
    "cache_dir": BASE_DIR / "prepped_data_anonymized" / "topic_embeddings",
    "results_dir": BASE_DIR / "descriptive_results" / "topics",
    "figures_dir": BASE_DIR / "graphical_output" / "topics",

    # Embeddings
    # "sentence_transformers" (local, free; downloads the model once)
    # "openai" (paid; needs OPENAI_API_KEY in the environment or a .env file - keep .env out of git)
    # "tfidf_svd" (no download; for testing the pipeline only - not for results)
    "embedding_backend": "sentence_transformers",
    "st_model": "all-mpnet-base-v2",
    "openai_model": "text-embedding-3-large",
    "embedding_batch_size": 64,

    # UMAP
    "umap_n_neighbors": 15,
    "umap_min_dist": 0.0,          # 0.0 packs clusters tighter for clustering; the 2-D plot uses 0.1
    "umap_min_dist_visual": 0.1,
    "umap_metric": "cosine",
    "umap_dim_intermediate": 20,
    "umap_dim_visual": 2,

    # Clustering
    "k_clusters": 15,              # <- primary k; set after reading the sweep
    "run_sweep": True,             # False = skip the k sweep (keeps the last sweep files); for quick k changes
    "k_sweep_min": 5,
    "k_sweep_max": 30,
    "n_stability_runs": 5,         # UMAP + k-means re-runs per k for the stability score
    "silhouette_sample": 5000,     # silhouette on a sample when there are more threads than this

    # Labels
    "top_terms": 12,
    "n_closest_titles": 12,
    "n_random_titles": 8,
    "ngram_range": (1, 2),
    "min_df": 2,
    # CMV title boilerplate. Slugs split contractions ("don't" -> "don t"), so the
    # fragments are listed too.
    "extra_stopwords": [
        "cmv", "view", "views", "change", "changed", "changing", "think", "thinks",
        "believe", "believes", "belief", "opinion", "opinions", "argue", "argument",
        "people", "person", "someone", "everyone", "anyone", "thing", "things",
        "really", "actually", "just", "like", "good", "bad", "better", "worse", "best",
        "worst", "way", "lot", "make", "makes", "making", "made", "need", "needs",
        "want", "wants", "shouldn", "don", "doesn", "isn", "aren", "wasn", "weren",
        "wouldn", "couldn", "didn", "won", "ve", "ll", "re", "im", "ive", "dont",
        "doesnt", "isnt", "arent", "shouldnt", "wouldnt", "cant", "wont",
        "pretty", "generally", "overall", "inherently", "actually", "basically",
        "literally", "simply", "truly",
    ],

    # Figures
    "figure_dpi": 300,
    "annotate_terms_per_cluster": 3,

    "random_seed": 42,
}

PERMALINK_RE = re.compile(r"/comments/([a-z0-9]+)/([^/]+)/")
DELETED_TITLES = {"deleted by user", "removed by user", "deleted", "removed",
                  "deleted by moderator", "removed by moderator"}


# -----------------------------------------------------------------------------
# 1. Threads and titles
# -----------------------------------------------------------------------------
def slug_to_title(slug: str) -> Tuple[str, str]:
    """Return (prefix, title_text) from a slug.

    prefix: "cmv" for normal posts, "meta" for meta/mod posts, "deleted" for posts whose
            title was replaced on deletion/removal, "other" otherwise.
    title_text: slug words with the CMV prefix removed.
    """
    words = [w for w in str(slug).lower().split("_") if w]
    if not words:
        return "other", ""
    # Posts deleted or removed before collection: Reddit replaces the title, so the slug
    # carries no topic ("deleted_by_user"). These threads are set aside, not clustered.
    if " ".join(words) in DELETED_TITLES:
        return "deleted", ""
    if words[0] == "cmv":
        prefix, words = "cmv", words[1:]
    elif words[0] in {"meta", "mod", "announcement"}:
        prefix = "meta"
    else:
        prefix = "other"
    return prefix, " ".join(words)


def load_threads(config: dict) -> pd.DataFrame:
    print("Loading threads from permalinks...")
    for key in ("comments_csv", "conversations_csv"):
        if not config[key].exists():
            print(f"  ERROR: not found: {config[key]}")
            print("  Run from the project folder after 1_data_prep.R and 1b_pseudonymize_users.R.")
            sys.exit(1)

    comments = pd.read_csv(config["comments_csv"], dtype=str,
                           usecols=["conversation_id", "permalink", "timestamp"],
                           keep_default_na=False, na_values=[""])
    conversations = pd.read_csv(config["conversations_csv"], dtype=str,
                                usecols=["conversation_id", "pair_id"],
                                keep_default_na=False, na_values=[""])

    parsed = comments["permalink"].fillna("").str.extract(PERMALINK_RE)
    comments["post_id"] = parsed[0]
    comments["post_slug"] = parsed[1]
    comments["timestamp"] = pd.to_numeric(comments["timestamp"], errors="coerce")

    # Conversation -> post (own permalinks), then filled from the pair partner, as in 1_data_prep.R
    conv_post = (comments.dropna(subset=["post_id"])
                 .groupby("conversation_id")
                 .agg(post_id=("post_id", "first"), post_slug=("post_slug", "first"))
                 .reset_index())
    conversations["pair_key"] = np.where(
        conversations["conversation_id"] < conversations["pair_id"],
        conversations["conversation_id"] + "_" + conversations["pair_id"],
        conversations["pair_id"] + "_" + conversations["conversation_id"],
    )
    conversations = conversations.merge(conv_post, on="conversation_id", how="left")
    pair_post = (conversations.dropna(subset=["post_id"])
                 .groupby("pair_key")
                 .agg(pair_post_id=("post_id", "first"), pair_post_slug=("post_slug", "first"))
                 .reset_index())
    conversations = conversations.merge(pair_post, on="pair_key", how="left")
    conversations["post_id"] = conversations["post_id"].fillna(conversations["pair_post_id"])
    conversations["post_slug"] = conversations["post_slug"].fillna(conversations["pair_post_slug"])

    starts = comments.groupby("conversation_id")["timestamp"].min().rename("start_ts").reset_index()
    conversations = conversations.merge(starts, on="conversation_id", how="left")

    threads = (conversations.dropna(subset=["post_id"])
               .groupby("post_id")
               .agg(post_slug=("post_slug", "first"),
                    n_slugs=("post_slug", "nunique"),
                    n_pairs=("pair_key", "nunique"),
                    n_paths=("conversation_id", "nunique"),
                    first_path_ts=("start_ts", "min"))
               .reset_index())

    n_conflict = int((threads["n_slugs"] > 1).sum())
    if n_conflict:
        print(f"  WARNING: {n_conflict} threads have more than one slug; the first is used.")
    threads = threads.drop(columns="n_slugs")

    threads[["prefix", "title_text"]] = threads["post_slug"].apply(
        lambda s: pd.Series(slug_to_title(s)))
    threads["year"] = pd.to_datetime(threads["first_path_ts"], unit="s", utc=True).dt.year

    # Threads with no usable title (deleted posts, empty slugs): counted and saved, not clustered
    no_title = threads[threads["title_text"].str.len() == 0]
    no_title_summary = (no_title.assign(reason=np.where(no_title["prefix"] == "deleted",
                                                        "Post deleted/removed (title replaced)",
                                                        "No title words in slug"))
                        .groupby("reason")
                        .agg(n_threads=("post_id", "size"), n_pairs=("n_pairs", "sum"))
                        .reset_index())
    config["results_dir"].mkdir(parents=True, exist_ok=True)
    no_title_summary.to_csv(config["results_dir"] / "topic_threads_without_title.csv", index=False)
    threads = threads[threads["title_text"].str.len() > 0].reset_index(drop=True)

    print(f"  Threads with a usable title: {len(threads):,}")
    for _, r in no_title_summary.iterrows():
        print(f"  Set aside - {r['reason']}: {r['n_threads']:,} threads ({r['n_pairs']:,} pairs)")
    print(f"  Prefix: {threads['prefix'].value_counts().to_dict()}")
    words = threads["title_text"].str.split().str.len()
    print(f"  Title words: median={words.median():.0f}, min={words.min()}, max={words.max()}")
    print(f"  Pairs covered: {threads['n_pairs'].sum():,}")
    return threads


# -----------------------------------------------------------------------------
# 2. Embeddings (cached)
# -----------------------------------------------------------------------------
def embed_titles(texts: List[str], config: dict) -> np.ndarray:
    backend = config["embedding_backend"]
    model = {"sentence_transformers": config["st_model"],
             "openai": config["openai_model"],
             "tfidf_svd": "tfidf_svd"}[backend]

    digest = hashlib.md5(("\n".join(texts) + backend + model).encode("utf-8")).hexdigest()[:12]
    cache_dir: Path = config["cache_dir"]
    cache_dir.mkdir(parents=True, exist_ok=True)
    cache = cache_dir / f"emb_{backend}_{model.replace('/', '-')}_{digest}.npy"
    if cache.exists():
        print(f"Loading cached embeddings: {cache.name}")
        return np.load(cache)

    if backend == "sentence_transformers":
        emb = _embed_st(texts, config)
    elif backend == "openai":
        emb = _embed_openai(texts, config)
    elif backend == "tfidf_svd":
        emb = _embed_tfidf_svd(texts, config)
    else:
        raise ValueError(f"Unknown embedding_backend: {backend!r}")

    np.save(cache, emb)
    print(f"  Cached: {cache.name}")
    return emb


def _normalise(emb: np.ndarray) -> np.ndarray:
    norms = np.linalg.norm(emb, axis=1, keepdims=True)
    norms[norms == 0] = 1.0
    return (emb / norms).astype(np.float32)


def _embed_st(texts: List[str], config: dict) -> np.ndarray:
    print(f"Embedding with sentence-transformers: {config['st_model']}")
    from sentence_transformers import SentenceTransformer
    model = SentenceTransformer(config["st_model"])
    emb = model.encode(texts, batch_size=config["embedding_batch_size"],
                       show_progress_bar=True, convert_to_numpy=True,
                       normalize_embeddings=True)
    print(f"  Embedding shape: {emb.shape}")
    return emb.astype(np.float32)


def _embed_openai(texts: List[str], config: dict) -> np.ndarray:
    import os
    print(f"Embedding with OpenAI: {config['openai_model']}")
    api_key = os.environ.get("OPENAI_API_KEY")
    env_path = BASE_DIR / ".env"
    if not api_key and env_path.exists():
        for line in env_path.read_text().splitlines():
            if line.strip().startswith("OPENAI_API_KEY="):
                api_key = line.split("=", 1)[1].strip().strip('"').strip("'")
                break
    if not api_key:
        print("  ERROR: OPENAI_API_KEY not found in the environment or .env")
        sys.exit(1)
    from openai import OpenAI
    client = OpenAI(api_key=api_key)
    vecs: List[List[float]] = []
    for i in range(0, len(texts), 500):
        resp = client.embeddings.create(model=config["openai_model"], input=texts[i:i + 500])
        vecs.extend(item.embedding for item in resp.data)
        print(f"  {min(i + 500, len(texts)):,}/{len(texts):,}")
    return _normalise(np.array(vecs, dtype=np.float32))


def _embed_tfidf_svd(texts: List[str], config: dict) -> np.ndarray:
    print("Embedding with TF-IDF + SVD (testing only)")
    from sklearn.decomposition import TruncatedSVD
    from sklearn.feature_extraction.text import TfidfVectorizer
    X = TfidfVectorizer(ngram_range=(1, 2), min_df=1, sublinear_tf=True).fit_transform(texts)
    n_comp = max(2, min(100, X.shape[1] - 1, X.shape[0] - 1))
    emb = TruncatedSVD(n_components=n_comp, random_state=config["random_seed"]).fit_transform(X)
    return _normalise(emb)


# -----------------------------------------------------------------------------
# 3. UMAP
# -----------------------------------------------------------------------------
def run_umap(emb: np.ndarray, n_components: int, min_dist: float, seed: int,
             config: dict) -> np.ndarray:
    import umap
    reducer = umap.UMAP(n_neighbors=config["umap_n_neighbors"], min_dist=min_dist,
                        n_components=n_components, metric=config["umap_metric"],
                        random_state=seed)
    return reducer.fit_transform(emb)


# -----------------------------------------------------------------------------
# 4. k sweep: silhouette + stability
# -----------------------------------------------------------------------------
def sweep_k(emb: np.ndarray, mid_main: np.ndarray, config: dict) -> pd.DataFrame:
    from sklearn.cluster import KMeans
    from sklearn.metrics import adjusted_rand_score, silhouette_score

    seed = config["random_seed"]
    ks = list(range(config["k_sweep_min"], config["k_sweep_max"] + 1))
    n_runs = config["n_stability_runs"]
    sample = min(config["silhouette_sample"], len(mid_main))

    print(f"Stability runs: UMAP with {n_runs} seeds...")
    mids = [mid_main] + [
        run_umap(emb, config["umap_dim_intermediate"], config["umap_min_dist"], seed + r, config)
        for r in range(1, n_runs)
    ]

    rows = []
    print("Sweeping k (silhouette on the main run; stability = mean ARI across runs)...")
    for k in ks:
        labels_runs = [KMeans(n_clusters=k, random_state=seed, n_init=10).fit_predict(m) for m in mids]
        sil = silhouette_score(mid_main, labels_runs[0], sample_size=sample, random_state=seed)
        aris = [adjusted_rand_score(a, b) for a, b in combinations(labels_runs, 2)]
        sizes = np.bincount(labels_runs[0])
        rows.append({"k": k, "silhouette": sil, "stability_mean_ari": float(np.mean(aris)),
                     "stability_min_ari": float(np.min(aris)),
                     "smallest_cluster": int(sizes.min()), "largest_cluster": int(sizes.max())})
        print(f"  k={k:>2}  silhouette={sil:.3f}  stability(ARI)={np.mean(aris):.3f}  "
              f"sizes {sizes.min()}-{sizes.max()}")
    return pd.DataFrame(rows)


# -----------------------------------------------------------------------------
# 5. Labels: class-based TF-IDF
# -----------------------------------------------------------------------------
def label_clusters(texts: List[str], labels: np.ndarray, config: dict) -> Dict[int, List[Tuple[str, float]]]:
    """Top terms per cluster by class-based TF-IDF.

    All titles in a cluster are treated as one document. A term scores highly when it is
    frequent in the cluster and rare across clusters:
        score = (term count in cluster / words in cluster) * log(1 + mean words per cluster / term count overall)
    """
    from sklearn.feature_extraction import text as sk_text
    from sklearn.feature_extraction.text import CountVectorizer

    stop = list(sk_text.ENGLISH_STOP_WORDS.union(config["extra_stopwords"]))
    cv = CountVectorizer(ngram_range=tuple(config["ngram_range"]), stop_words=stop,
                         min_df=config["min_df"], token_pattern=r"(?u)\b[a-z][a-z0-9]+\b")
    X = cv.fit_transform(texts)
    terms = np.array(cv.get_feature_names_out())

    clusters = sorted(np.unique(labels))
    counts = np.vstack([np.asarray(X[labels == c].sum(axis=0)).ravel() for c in clusters])
    tf = counts / np.maximum(counts.sum(axis=1, keepdims=True), 1)
    avg_words = counts.sum() / len(clusters)
    idf = np.log(1 + avg_words / np.maximum(counts.sum(axis=0), 1))
    ctfidf = tf * idf

    out = {}
    top_n = config["top_terms"]
    for i, c in enumerate(clusters):
        order = np.argsort(ctfidf[i])[::-1]
        slate = [(terms[j], float(ctfidf[i, j])) for j in order[:top_n * 3] if ctfidf[i, j] > 0]
        out[int(c)] = _dedupe_ngrams(slate, top_n)
    return out


def _dedupe_ngrams(term_scores: List[Tuple[str, float]], top_n: int) -> List[Tuple[str, float]]:
    """Prefer a bigram over its own words.

    When a bigram is kept, its single words are removed from the list (even if they scored
    higher) and not added later, so "donald trump" replaces "donald" and "trump", and
    "morally wrong" replaces "morally" and "wrong". The bigram takes the highest score of
    itself and the words it replaced, so it keeps their place in the ranking.
    """
    kept: List[Tuple[str, float]] = []
    covered: set = set()
    for term, score in term_scores:
        parts = term.split()
        if len(parts) == 1:
            if term in covered:
                continue
            kept.append((term, score))
        else:
            replaced = [s for t, s in kept if t in parts]
            kept = [(t, s) for t, s in kept if t not in parts]
            kept.append((term, max([score] + replaced)))
            covered.update(parts)
        if len(kept) >= top_n:
            break
    return sorted(kept, key=lambda ts: ts[1], reverse=True)


def representative_titles(threads: pd.DataFrame, emb: np.ndarray, labels: np.ndarray,
                          config: dict) -> Tuple[Dict[int, List[str]], Dict[int, List[str]], np.ndarray]:
    """Titles closest to each cluster's centre (cosine, in embedding space), plus a random draw.
    Also returns each thread's similarity to its own cluster centre."""
    rng = np.random.default_rng(config["random_seed"])
    closest, random_draw = {}, {}
    sim_to_centre = np.zeros(len(labels))
    for c in sorted(np.unique(labels)):
        idx = np.where(labels == c)[0]
        centre = emb[idx].mean(axis=0)
        centre /= max(np.linalg.norm(centre), 1e-12)
        sims = emb[idx] @ centre
        sim_to_centre[idx] = sims
        order = idx[np.argsort(sims)[::-1]]
        closest[int(c)] = threads["title_text"].iloc[order[:config["n_closest_titles"]]].tolist()
        n_rand = min(config["n_random_titles"], len(idx))
        random_draw[int(c)] = threads["title_text"].iloc[rng.choice(idx, n_rand, replace=False)].tolist()
    return closest, random_draw, sim_to_centre


# -----------------------------------------------------------------------------
# 6. Figures
# -----------------------------------------------------------------------------
def plot_sweep(sweep: pd.DataFrame, k_main: int, path: Path, config: dict) -> None:
    import matplotlib.pyplot as plt
    fig, axes = plt.subplots(1, 2, figsize=(12, 4.5))
    for ax, col, title in [(axes[0], "silhouette", "Silhouette (higher = better separated)"),
                           (axes[1], "stability_mean_ari", "Stability across UMAP seeds (mean ARI)")]:
        ax.plot(sweep["k"], sweep[col], marker="o", color="#2b6cb0", linewidth=2)
        ax.axvline(k_main, color="orange", linestyle=":", label=f"Configured k = {k_main}")
        ax.set_title(title, fontsize=11)
        ax.set_xlabel("k")
        ax.set_xticks(sweep["k"])
        ax.tick_params(axis="x", labelsize=7)
        ax.grid(alpha=0.3)
        ax.legend(fontsize=8)
    axes[1].set_ylim(0, 1)
    fig.suptitle("Choosing k: thread topics from post title slugs", fontweight="bold")
    fig.text(0.01, 0.01, "Stability = adjusted Rand index between k-means solutions on "
             f"{config['n_stability_runs']} UMAP runs with different seeds (1 = identical clusters).",
             fontsize=8, color="#666666")
    fig.tight_layout(rect=(0, 0.04, 1, 1))
    fig.savefig(path, dpi=config["figure_dpi"])
    plt.close(fig)
    print(f"  Saved: {path.name}")


def plot_umap(two_d: np.ndarray, labels: np.ndarray, top: dict, path: Path, config: dict) -> None:
    import matplotlib.pyplot as plt
    from matplotlib.patheffects import withStroke
    fig, ax = plt.subplots(figsize=(13, 9))
    clusters = sorted(np.unique(labels))
    cmap = plt.get_cmap("tab20" if len(clusters) > 10 else "tab10")
    for i, c in enumerate(clusters):
        m = labels == c
        ax.scatter(two_d[m, 0], two_d[m, 1], s=8, alpha=0.55, color=cmap(i % cmap.N),
                   edgecolors="none", label=f"#{c}")
    n_terms = config["annotate_terms_per_cluster"]
    for c in clusters:
        m = labels == c
        cx, cy = np.median(two_d[m, 0]), np.median(two_d[m, 1])
        text = f"#{c}\n" + "\n".join(t for t, _ in top.get(int(c), [])[:n_terms])
        ax.text(cx, cy, text, fontsize=8, fontweight="bold", ha="center", va="center",
                path_effects=[withStroke(linewidth=3, foreground="white")], zorder=5)
    ax.set_title(f"Thread topics from post title slugs (UMAP of title embeddings, k={len(clusters)})",
                 fontsize=12, fontweight="bold")
    ax.set_xticks([])
    ax.set_yticks([])
    ax.legend(loc="center left", bbox_to_anchor=(1.0, 0.5), fontsize=7, markerscale=2, frameon=False)
    fig.text(0.01, 0.01, f"One point per thread. Embeddings: {config['embedding_backend']}. "
             "Labels = top class-based TF-IDF terms. Positions in 2-D are approximate.",
             fontsize=8, color="#666666")
    fig.tight_layout(rect=(0, 0.03, 1, 1))
    fig.savefig(path, dpi=config["figure_dpi"])
    plt.close(fig)
    print(f"  Saved: {path.name}")


def plot_sizes(labels: np.ndarray, top: dict, path: Path, config: dict) -> None:
    import matplotlib.pyplot as plt
    clusters, counts = np.unique(labels, return_counts=True)
    order = np.argsort(counts)
    names = [f"#{clusters[i]}  " + ", ".join(t for t, _ in top.get(int(clusters[i]), [])[:3]) for i in order]
    fig, ax = plt.subplots(figsize=(9, 0.35 * len(clusters) + 1.5))
    ax.barh(names, counts[order], color="#5C85D6")
    for y, v in enumerate(counts[order]):
        ax.text(v, y, f" {v:,} ({100 * v / counts.sum():.1f}%)", va="center", fontsize=8)
    ax.set_xlim(0, counts.max() * 1.25)
    ax.set_xlabel("Threads")
    ax.set_title("Threads per topic cluster", fontweight="bold")
    ax.grid(axis="x", alpha=0.3)
    fig.tight_layout()
    fig.savefig(path, dpi=config["figure_dpi"])
    plt.close(fig)
    print(f"  Saved: {path.name}")


def plot_dendrogram(emb: np.ndarray, labels: np.ndarray, top: dict, path: Path, config: dict) -> None:
    """Hierarchical clustering of cluster centres (cosine, average linkage)."""
    import matplotlib.pyplot as plt
    from scipy.cluster.hierarchy import dendrogram, linkage
    clusters = sorted(np.unique(labels))
    centres = np.vstack([emb[labels == c].mean(axis=0) for c in clusters])
    centres /= np.linalg.norm(centres, axis=1, keepdims=True)
    Z = linkage(centres, method="average", metric="cosine")
    names = [f"#{c}  " + ", ".join(t for t, _ in top.get(int(c), [])[:3]) for c in clusters]
    fig, ax = plt.subplots(figsize=(10, 0.35 * len(clusters) + 1.5))
    dendrogram(Z, labels=names, orientation="right", ax=ax, color_threshold=None,
               above_threshold_color="#555555")
    ax.set_xlabel("Cosine distance between cluster centres")
    ax.set_title("Which topic clusters sit close together", fontweight="bold")
    ax.tick_params(axis="y", labelsize=8)
    fig.tight_layout()
    fig.savefig(path, dpi=config["figure_dpi"])
    plt.close(fig)
    print(f"  Saved: {path.name}")


def plot_wordclouds(top: dict, path: Path, config: dict) -> None:
    try:
        from wordcloud import WordCloud
    except ImportError:
        print("  (wordcloud not installed - skipping word clouds)")
        return
    import matplotlib.pyplot as plt
    k = len(top)
    cols = 4 if k > 9 else 3
    rows = int(np.ceil(k / cols))
    fig, axes = plt.subplots(rows, cols, figsize=(cols * 4.2, rows * 3.2))
    axes = np.array(axes).reshape(-1)
    for ax, (c, ts) in zip(axes, top.items()):
        ax.axis("off")
        if ts:
            wc = WordCloud(width=600, height=400, background_color="white", colormap="viridis",
                           prefer_horizontal=0.9, random_state=config["random_seed"])
            ax.imshow(wc.generate_from_frequencies(dict(ts)), interpolation="bilinear")
            ax.set_title(f"Cluster {c}", fontsize=11)
    for ax in axes[k:]:
        ax.axis("off")
    fig.suptitle("Top terms by topic cluster (class-based TF-IDF)", fontsize=14)
    fig.tight_layout()
    fig.savefig(path, dpi=config["figure_dpi"], bbox_inches="tight")
    plt.close(fig)
    print(f"  Saved: {path.name}")


# -----------------------------------------------------------------------------
# 7. Main
# -----------------------------------------------------------------------------
def main() -> None:
    cfg = CONFIG
    for key in ("results_dir", "figures_dir"):
        cfg[key].mkdir(parents=True, exist_ok=True)
    cfg["topics_out"].parent.mkdir(parents=True, exist_ok=True)
    k_main = cfg["k_clusters"]
    seed = cfg["random_seed"]

    threads = load_threads(cfg)
    texts = threads["title_text"].tolist()
    emb = embed_titles(texts, cfg)

    print("Running UMAP (clustering space and 2-D)...")
    mid = run_umap(emb, cfg["umap_dim_intermediate"], cfg["umap_min_dist"], seed, cfg)
    two_d = run_umap(emb, cfg["umap_dim_visual"], cfg["umap_min_dist_visual"], seed, cfg)

    if cfg["run_sweep"]:
        sweep = sweep_k(emb, mid, cfg)
        sweep.round(4).to_csv(cfg["results_dir"] / "topic_k_sweep.csv", index=False)
        plot_sweep(sweep, k_main, cfg["figures_dir"] / "topic_k_sweep.png", cfg)
    else:
        print("Skipping k sweep (run_sweep = False).")

    from sklearn.cluster import KMeans
    labels = KMeans(n_clusters=k_main, random_state=seed, n_init=10).fit_predict(mid)

    top = label_clusters(texts, labels, cfg)
    closest, random_draw, sim = representative_titles(threads, emb, labels, cfg)

    # Thread table for R
    out = threads.copy()
    out["k"] = k_main
    out["topic"] = labels
    out["topic_terms"] = [", ".join(t for t, _ in top[int(c)][:3]) for c in labels]
    out["similarity_to_topic_centre"] = sim.round(4)
    out["umap_x"] = two_d[:, 0].round(4)
    out["umap_y"] = two_d[:, 1].round(4)
    out["embedding_backend"] = cfg["embedding_backend"]
    out.to_csv(cfg["topics_out"], index=False, encoding="utf-8")
    out.to_csv(cfg["results_dir"] / f"thread_topics_k{k_main}.csv", index=False, encoding="utf-8")
    print(f"  Saved: {cfg['topics_out']}")

    # Cluster summary + coding sheet
    sizes = out.groupby("topic").agg(n_threads=("post_id", "size"), n_pairs=("n_pairs", "sum")).reset_index()
    sheet = sizes.assign(
        pct_threads=lambda d: (100 * d["n_threads"] / d["n_threads"].sum()).round(2),
        top_terms=lambda d: d["topic"].map(lambda c: ", ".join(t for t, _ in top[int(c)])),
        closest_titles=lambda d: d["topic"].map(lambda c: " | ".join(closest[int(c)])),
        random_titles=lambda d: d["topic"].map(lambda c: " | ".join(random_draw[int(c)])),
        topic_name="", political="", political_subtype="", notes="",
    )
    sheet.to_csv(cfg["results_dir"] / f"topic_coding_sheet_k{k_main}_BLANK.csv", index=False, encoding="utf-8")
    print(f"  Saved: topic_coding_sheet_k{k_main}_BLANK.csv  (copy to topic_coding_k{k_main}.csv to fill in)")

    lines = ["=" * 72, f"TOPIC CLUSTERS (k={k_main}) - top terms and titles closest to the centre", "=" * 72, ""]
    for _, r in sheet.iterrows():
        c = int(r["topic"])
        head = f"Cluster {c}  ({r['n_threads']:,} threads, {r['pct_threads']}%; {r['n_pairs']:,} pairs)"
        lines += [head, "-" * len(head), "  Terms: " + r["top_terms"], "  Closest titles:"]
        lines += [f"    - {t}" for t in closest[c]]
        lines += ["  Random titles:"] + [f"    - {t}" for t in random_draw[c]] + [""]
    (cfg["results_dir"] / f"topic_labels_k{k_main}.txt").write_text("\n".join(lines), encoding="utf-8")
    print(f"  Saved: topic_labels_k{k_main}.txt")

    plot_umap(two_d, labels, top, cfg["figures_dir"] / f"topic_umap_k{k_main}.png", cfg)
    plot_sizes(labels, top, cfg["figures_dir"] / f"topic_sizes_k{k_main}.png", cfg)
    plot_dendrogram(emb, labels, top, cfg["figures_dir"] / f"topic_dendrogram_k{k_main}.png", cfg)
    plot_wordclouds(top, cfg["figures_dir"] / f"topic_wordclouds_k{k_main}.png", cfg)

    print("\nDone. Read topic_labels_k*.txt first - naming the clusters is the interpretive step.")


if __name__ == "__main__":
    main()