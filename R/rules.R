# Shared helpers of tue_datamanager_check: the rule files, the checklist rows
# and findings every check returns, and small text helpers.

# The rule files in data/, read once per run.
.tdc_rules <- function(mod_dir) {
  read <- function(f) jsonlite::fromJSON(file.path(mod_dir, "data", f),
                                         simplifyVector = FALSE)
  sev <- read("severity.json")
  formats <- read("formats.json")
  list(
    formats = formats$formats,
    open_formats = unlist(formats$open_formats$exts),
    layouts = read("layouts_tue.json")$layouts,
    components = read("components_tue.json")$components,
    readme = read("readme_tue.json"),
    personal = read("personal_data.json"),
    severity = unlist(sev$severity),
    limits = sev$limits,
    dir = mod_dir
  )
}

# The status a finding of kind `id` gives, from severity.json.
.tdc_sev <- function(rules, id) {
  s <- unname(rules$severity[id])
  if (is.na(s)) stop("No severity for '", id, "' in data/severity.json")
  s
}

# The status order, worst first; the worst status of an item's findings is the
# item's status.
.tdc_status_levels <- c("fail", "warn", "manual", "pass", "na")
.tdc_worst <- function(status) {
  status <- status[!is.na(status)]
  if (length(status) == 0) return("pass")
  .tdc_status_levels[min(match(status, .tdc_status_levels))]
}

# One checklist row. `section` is the section of the RDLC document
# ("1.1", "1.2", "1.3", "2"). What was found, what to do, why and a note are
# kept apart, for metacheck's checklist report; `detail` is all four in one
# text, as the module's own report shows it. Sentences are joined with a
# space; pieces that start with "- " become a list (markdown) after them.
.tdc_item <- function(id, section, title, status, found, todo = NULL, why = NULL, note = NULL) {
  one <- function(x) {
    x <- as.character(unlist(x))
    x <- x[!is.na(x) & nzchar(x)]
    bullet <- startsWith(x, "- ")
    out <- c(paste(x[!bullet], collapse = " "), paste(x[bullet], collapse = "\n"))
    paste(out[nzchar(out)], collapse = "\n\n")
  }
  parts <- c(one(found), one(todo), one(why), one(note))
  data.frame(item = id, section = section, title = title, status = status,
             found = parts[[1]], todo = parts[[2]], why = parts[[3]], note = parts[[4]],
             detail = paste(parts[nzchar(parts)], collapse = if (any(grepl("\n", parts))) "\n\n" else " "))
}

# The row of a check that stopped with an error (a file the readers cannot
# handle, or a mistake in the check): decide by hand, with the error, so the
# report still shows every other check. `what` names what failed when that
# is not the check itself (such as metacheck's data_check).
.tdc_check_failed <- function(id, section, title, error, what = "This check") {
  msg <- if (inherits(error, "condition")) conditionMessage(error) else as.character(error)
  msg <- sub("[.[:space:]]+$", "", trimws(gsub("[[:space:]]+", " ", paste(msg, collapse = " "))))
  .tdc_item(id, section, title, "manual",
            sprintf("%s could not be completed, because of an error: %s.", what, msg),
            c("Check this by hand.",
              "Please report the error, with the package if you can share it, at https://github.com/Lakens/tue_datamanager_check/issues."))
}

# Findings: one row per file (or folder, or column) behind an item.
.tdc_findings <- function(item, path, problem, status) {
  if (length(path) == 0) {
    return(data.frame(item = character(0), path = character(0),
                      problem = character(0), status = character(0)))
  }
  data.frame(item = item, path = path, problem = problem, status = status)
}

# Folder and file names as the layout and component rules compare them:
# lower case, without a leading number ("1 ", "01_"), "&" as "and", and "_",
# "-" and "." as spaces ("3 Code & Script" -> "code and script", "Fig.1" ->
# "fig 1").
.tdc_norm_name <- function(x) {
  x <- tolower(x)
  x <- sub("^[0-9]+[[:space:]._-]*", "", x)
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("[_.-]+", " ", x)
  trimws(gsub("[[:space:]]+", " ", x))
}

# TRUE for each x that matches any of the regular expressions in `patterns`
# (case-insensitive, Perl syntax, as the rule files are written).
.tdc_match_any <- function(x, patterns) {
  if (length(x) == 0) return(logical(0))
  hit <- rep(FALSE, length(x))
  for (p in unlist(patterns)) {
    hit <- hit | grepl(p, x, ignore.case = TRUE, perl = TRUE)
  }
  hit
}

# "a, b, c and 4 more"
.tdc_list <- function(x, n = 5) {
  x <- unique(x[!is.na(x)])
  if (length(x) <= n) return(paste(x, collapse = ", "))
  paste0(paste(x[seq_len(n)], collapse = ", "), " and ", length(x) - n, " more")
}

# The text of a local file, or NULL when it cannot be read.
.tdc_read_text <- function(path, max_bytes = 2e6) {
  if (is.na(path) || !file.exists(path)) return(NULL)
  raw <- tryCatch(readBin(path, "raw", n = max_bytes), error = function(e) NULL)
  if (is.null(raw)) return(NULL)
  txt <- rawToChar(raw[raw != as.raw(0)])
  Encoding(txt) <- "UTF-8"
  if (!validUTF8(txt)) txt <- iconv(txt, "latin1", "UTF-8", sub = "")
  txt
}

# The folder levels of a relative path ("a/b/c.csv" has 2).
.tdc_depth <- function(path) {
  lengths(regmatches(path, gregexpr("/", path, fixed = TRUE)))
}
