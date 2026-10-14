# sync_overleaf.R for Conversations Gone Awry (CMV) - Political Comments Analysis
#
# Copies the generated LaTeX inputs and figures into one or more Overleaf
# projects, then (optionally) commits and pushes them so Overleaf picks them up.
#
# What is copied (each folder is mirrored: changed files are updated, files that
# no longer exist in the repo are removed from the project folder):
#
#   tex_outputs/               ->  <project>/tex_outputs/
#   graphical_output/<sub>/    ->  <project>/figures/<sub>/      (deleted_output/ skipped)
#   study_exhibits/            ->  <project>/figures/study_exhibits/
#   BIB_SOURCE (optional)      ->  <project>/references.bib
#
# Nothing else in the project is touched, so your .tex files and notes are safe.
# Do NOT edit files inside tex_outputs/ or figures/ on Overleaf: the next sync
# overwrites them. Change the R scripts instead.
#
# ONE-TIME SETUP PER OVERLEAF PROJECT (needs Overleaf Git integration or GitHub
# sync -- premium, or included in many institutional licences):
#   Option A, Overleaf Git (simplest):
#     1. In Overleaf: Menu > Integrations > Git. Copy the clone URL
#        (https://git.overleaf.com/<project-id>) and create a Git authentication
#        token under Account Settings (git asks for it as the password; the
#        username is "git").
#     2. git clone https://git.overleaf.com/<project-id> "C:/Users/<you>/Overleaf/<name>"
#     3. Add that folder to OVERLEAF_PROJECTS below.
#   Option B, GitHub sync: link the Overleaf project to its own small GitHub repo
#     (not this analysis repo -- it is too large for Overleaf), clone that repo
#     locally, add the folder below, run this script, then press
#     "Pull GitHub changes into Overleaf" in Overleaf.
#   No premium? Set PUSH <- FALSE, point a project at any local folder, then zip
#     and upload the synced folders to Overleaf (Upload > overwrite) when needed.
#
# Run from the project folder (where CGA_CMV_Pol_Comments.Rproj is), after
# 7_export_tex.R.


# === SECTION 0: SETTINGS ===

# Local clones of each Overleaf project (name = anything memorable).
# Add a line per write-up; later papers reuse the same tex_outputs/ and figures.
OVERLEAF_PROJECTS <- list(
  writeups = "../CMV-CGA-analysis-writeup"
)

SYNC_TARGETS <- names(OVERLEAF_PROJECTS)   # or e.g. c("project_update_oct2026")
PUSH         <- TRUE                        # FALSE = copy only, no git
COMMIT_MSG   <- sprintf("Sync analysis outputs (%s)", format(Sys.time(), "%Y-%m-%d %H:%M"))

# Optional: a Zotero (Better BibTeX "Keep updated") export to copy as references.bib
BIB_SOURCE   <- NULL                        # e.g. "C:/Users/rmirza/Zotero/Reddit_CMV_Analysis.bib"

# Folders mirrored into every project
MIRRORS <- c(
  "tex_outputs"    = "tex_outputs",
  "study_exhibits" = file.path("figures", "study_exhibits")
)
FIG_SUBDIRS <- setdiff(list.dirs("graphical_output", full.names = FALSE, recursive = FALSE),
                       "deleted_output")
MIRRORS <- c(MIRRORS, setNames(file.path("figures", FIG_SUBDIRS),
                               file.path("graphical_output", FIG_SUBDIRS)))
KEEP_EXT <- "\\.(tex|png|pdf|jpe?g|csv|txt)$"   # file types to copy


# === SECTION 1: HELPERS ===

git <- function(dir, ...) {
  out <- suppressWarnings(system2("git", c("-C", shQuote(dir), ...), stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status"); if (is.null(status)) status <- 0
  list(ok = status == 0, out = out)
}

# Mirror one folder: copy new/changed files, remove files no longer in the source.
mirror_dir <- function(src, dest) {
  if (!dir.exists(src)) {
    message(sprintf("  skip (missing): %s", src)); return(c(copied = 0, removed = 0))
  }
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  src_files  <- list.files(src,  pattern = KEEP_EXT, recursive = TRUE, ignore.case = TRUE)
  dest_files <- list.files(dest, pattern = KEEP_EXT, recursive = TRUE, ignore.case = TRUE)

  copied <- 0
  for (f in src_files) {
    s <- file.path(src, f); d <- file.path(dest, f)
    if (!file.exists(d) || unname(tools::md5sum(s)) != unname(tools::md5sum(d))) {
      dir.create(dirname(d), recursive = TRUE, showWarnings = FALSE)
      file.copy(s, d, overwrite = TRUE, copy.date = TRUE)
      copied <- copied + 1
    }
  }
  stale <- setdiff(dest_files, src_files)
  if (length(stale)) file.remove(file.path(dest, stale))
  c(copied = copied, removed = length(stale))
}

sync_project <- function(name, dir) {
  message(sprintf("\n== %s  (%s)", name, dir))
  if (!dir.exists(dir)) stop(sprintf("Project folder not found: %s (clone it first, see header).", dir))
  is_git <- dir.exists(file.path(dir, ".git"))
  use_git <- PUSH && is_git
  if (PUSH && !is_git) message("  not a git clone: copying only")

  # 1. Pull first, so edits made on Overleaf come down before we commit
  if (use_git) {
    r <- git(dir, "pull", "--ff-only")
    if (!r$ok) stop("git pull failed:\n", paste(r$out, collapse = "\n"),
                    "\nResolve it in that folder (e.g. commit or stash local edits), then re-run.")
  }

  # 2. Mirror folders
  totals <- c(copied = 0, removed = 0)
  for (src in names(MIRRORS)) {
    res <- mirror_dir(src, file.path(dir, MIRRORS[[src]]))
    if (sum(res) > 0) message(sprintf("  %-34s -> %-30s %3d updated, %d removed",
                                      src, MIRRORS[[src]], res[["copied"]], res[["removed"]]))
    totals <- totals + res
  }
  if (!is.null(BIB_SOURCE)) {
    if (file.exists(BIB_SOURCE)) {
      file.copy(BIB_SOURCE, file.path(dir, "references.bib"), overwrite = TRUE)
      message("  references.bib updated from BIB_SOURCE")
    } else message(sprintf("  BIB_SOURCE not found: %s", BIB_SOURCE))
  }
  message(sprintf("  total: %d files updated, %d removed", totals[["copied"]], totals[["removed"]]))

  # 3. Commit and push only the synced paths
  if (use_git) {
    paths <- unique(c(sub("/.*", "", unname(MIRRORS)), if (!is.null(BIB_SOURCE)) "references.bib"))
    git(dir, "add", "-A", "--", paths)
    staged <- git(dir, "diff", "--cached", "--quiet")
    if (staged$ok) {
      message("  nothing new to commit")
    } else {
      r <- git(dir, "commit", "-m", shQuote(COMMIT_MSG))
      if (!r$ok) stop("git commit failed:\n", paste(r$out, collapse = "\n"))
      r <- git(dir, "push")
      if (!r$ok) stop("git push failed:\n", paste(r$out, collapse = "\n"))
      message("  pushed to Overleaf")
    }
  }
  invisible(totals)
}


# === SECTION 2: RUN ===

stopifnot(dir.exists("tex_outputs"))   # run 7_export_tex.R first
for (nm in SYNC_TARGETS) sync_project(nm, OVERLEAF_PROJECTS[[nm]])
message("\nDone.")
