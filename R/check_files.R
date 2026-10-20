# Section 2 of the RDLC document: file checks (SPECIFICATION.md, section 2).
#
# 1. Non-preferred and proprietary formats: each file's extension against
#    TU/e's table of preferred formats (data/formats.json), files inside zips
#    too; for PDF, MATLAB, SVG and HTML files also what is inside them.

# -- files inside zips --------------------------------------------------------

# The files inside the package's local zips, as rows like `files`: path
# "<zip path>/<member>", name, ext, location NA, plus `zip` (the zip's local
# path) and `member` (the name inside it). Folders and junk files inside are
# left out.
.tdc_zip_members <- function(files) {
  none <- data.frame(path = character(0), name = character(0), ext = character(0),
                     data_type = character(0), location = character(0),
                     zip = character(0), member = character(0))
  zips <- files[files$ext == "zip" & !is.na(files$location) & file.exists(files$location), , drop = FALSE]
  rows <- lapply(seq_len(nrow(zips)), function(i) {
    lst <- tryCatch(utils::unzip(zips$location[[i]], list = TRUE), error = function(e) NULL)
    if (is.null(lst) || !nrow(lst)) return(NULL)
    m <- lst$Name[!grepl("/$", lst$Name)]
    m <- m[!metacheck:::.is_useless_file(m) & !grepl("(^|/)__MACOSX/", m)]
    if (!length(m)) return(NULL)
    data.frame(path = paste0(zips$path[[i]], "/", m), name = basename(m),
               ext = tolower(tools::file_ext(m)), data_type = NA_character_,
               location = NA_character_, zip = zips$location[[i]], member = m)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) none else do.call(rbind, rows)
}

# A local copy of a file to look inside: its own location, or a zip member
# unpacked into a temporary folder. NULL when there is none.
.tdc_local_copy <- function(f) {
  if (!is.na(f$location) && file.exists(f$location)) return(f$location)
  if (is.null(f$zip) || is.na(f$zip)) return(NULL)
  dest <- file.path(tempdir(), "tdc_zip_members")
  out <- tryCatch(utils::unzip(f$zip, files = f$member, exdir = dest, junkpaths = FALSE),
                  error = function(e) character(0), warning = function(w) character(0))
  if (length(out) && file.exists(out[[1]])) out[[1]] else NULL
}

# -- what is inside a file ----------------------------------------------------

# The bytes of a file: all of it up to `max_bytes`, else its first and last
# `max_bytes / 2` bytes.
.tdc_bytes <- function(path, max_bytes = 5e7) {
  size <- file.size(path)
  con <- file(path, "rb")
  on.exit(close(con))
  if (is.na(size) || size <= max_bytes) return(readBin(con, "raw", n = max(size, 0, na.rm = TRUE)))
  head <- readBin(con, "raw", n = max_bytes / 2)
  seek(con, size - max_bytes / 2)
  c(head, readBin(con, "raw", n = max_bytes / 2))
}

# Whether a file meets the requirement `req` of its format (formats.json):
# TRUE, FALSE, or NA when it could not be checked.
#   pdfa                 a PDF with a PDF/A identification in its metadata
#                        (pdfaid:part, which PDF/A requires uncompressed)
#   mat-v7.3             a MATLAB file whose header says version 7.3
#   svg-no-script        an SVG without scripts (<script, javascript:, onload=, ...)
#   html-self-contained  an HTML file that links to or loads no file online
#                        (href or src pointing to http(s)://)
.tdc_requirement_met <- function(req, path) {
  if (is.null(path)) return(NA)
  bytes <- tryCatch(.tdc_bytes(path), error = function(e) NULL)
  if (is.null(bytes)) return(NA)
  if (req == "pdfa") return(length(grepRaw("pdfaid:part", bytes, fixed = TRUE)) > 0)
  if (req == "mat-v7.3") {
    header <- rawToChar(bytes[seq_len(min(length(bytes), 116))][bytes[seq_len(min(length(bytes), 116))] != as.raw(0)])
    return(grepl("^MATLAB 7\\.3 MAT-file", header))
  }
  text <- rawToChar(bytes[bytes != as.raw(0)])
  Encoding(text) <- "UTF-8"
  text <- iconv(text, "UTF-8", "UTF-8", sub = "")
  if (req == "svg-no-script") {
    return(!grepl("<script|javascript:|\\son[a-z]+\\s*=", text, ignore.case = TRUE, perl = TRUE))
  }
  if (req == "html-self-contained") {
    return(!grepl("(?:href|src)\\s*=\\s*[\"']?\\s*https?://", text, ignore.case = TRUE, perl = TRUE))
  }
  NA
}

# -- 2.1 formats --------------------------------------------------------------

# Archives: their files are checked instead (zip), and a package should hold
# no archives at all (the zipped-package check).
.tdc_archive_exts <- c("zip", "tar", "gz", "tgz", "bz2", "xz", "7z", "rar")

# One row per file with an extension (files inside zips too): its result and
# what to do, from TU/e's table. Files without an extension and archives are
# skipped.
.tdc_format_findings <- function(files, members, rules) {
  cols <- c("path", "name", "ext", "data_type", "location")
  all <- files[nzchar(files$ext) & !files$ext %in% .tdc_archive_exts, cols, drop = FALSE]
  all$zip <- rep(NA_character_, nrow(all))
  all$member <- rep(NA_character_, nrow(all))
  if (nrow(members)) {
    all <- rbind(all, members[nzchar(members$ext) & !members$ext %in% .tdc_archive_exts,
                              c(cols, "zip", "member"), drop = FALSE])
  }
  if (!nrow(all)) {
    return(data.frame(path = character(0), ext = character(0), label = character(0),
                      status = character(0), todo = character(0)))
  }
  rows <- lapply(seq_len(nrow(all)), function(i) {
    f <- all[i, ]
    fmt <- rules$formats[[f$ext]]
    if (is.null(fmt)) {
      if (f$ext %in% rules$open_formats || f$data_type %in% "code") {
        return(c("Open format (code or plain text)", "pass", ""))
      }
      return(c("Not on TU/e's list", .tdc_sev(rules, "format-not-listed"),
               "Check that this is an open, well-documented format; if not, convert it to one of TU/e's preferred formats."))
    }
    if (fmt$level == "non-preferred") {
      return(c(fmt$label, .tdc_sev(rules, "format-not-preferred"),
               sprintf("Convert to %s.", fmt$alternative)))
    }
    if (is.null(fmt$requires)) return(c(fmt$label, "pass", ""))
    met <- .tdc_requirement_met(fmt$requires, .tdc_local_copy(f))
    if (is.na(met)) return(c(fmt$label, "pass", "Not checked inside: the file was not downloaded."))
    if (met) return(c(fmt$label, "pass", ""))
    c(fmt$unmet_label, .tdc_sev(rules, "format-requirement-unmet"), sprintf("Save it as %s.", fmt$alternative))
  })
  m <- do.call(rbind, rows)
  data.frame(path = all$path, ext = all$ext, label = m[, 1], status = m[, 2], todo = m[, 3])
}

# The checklist row and the report table (one row per extension and result:
# Extension | Files | Result | What to do, with the first 3 file names).
.tdc_check_formats <- function(files, members, rules) {
  fnd <- .tdc_format_findings(files, members, rules)
  title <- "File formats"
  if (!nrow(fnd)) {
    return(list(item = .tdc_item("formats", "2", title, "na", "The package has no files with an extension."),
                table = NULL, findings = fnd))
  }
  key <- paste(fnd$ext, fnd$label, fnd$status)
  groups <- split(fnd, factor(key, unique(key)))
  tbl <- do.call(rbind, lapply(groups, function(g) {
    data.frame(Extension = paste0(".", g$ext[[1]]), Format = g$label[[1]],
               Files = sprintf("%s: %s", format(nrow(g), big.mark = ","),
                               .tdc_list(paste0("`", g$path, "`"), 3)),
               Result = .tdc_result_words[[g$status[[1]]]], status = g$status[[1]],
               `What to do` = g$todo[[1]], check.names = FALSE)
  }))
  tbl <- tbl[order(match(tbl$status, .tdc_status_levels), tbl$Extension), , drop = FALSE]
  rownames(tbl) <- NULL
  n <- table(factor(fnd$status, .tdc_status_levels))
  n_ext <- length(unique(fnd$ext))
  in_zip <- if (nrow(members)) sprintf(" (%s inside zips)", format(nrow(members), big.mark = ",")) else ""
  # per result: "9 `.npy`, 6 `.pdf`"; and what to do, per advice: "`.npy`, `.npz`: Convert ..."
  by_ext <- function(status) {
    f <- fnd[fnd$status == status, , drop = FALSE]
    k <- table(factor(f$ext, unique(f$ext)))
    paste(sprintf("%s `.%s`", format(as.integer(k), big.mark = ",", trim = TRUE), names(k)), collapse = ", ")
  }
  flagged <- tbl[tbl$status %in% c("warn", "manual") & nzchar(tbl$`What to do`), , drop = FALSE]
  advice <- split(flagged$Extension, factor(flagged$`What to do`, unique(flagged$`What to do`)))
  todo <- sprintf("%s: %s", vapply(advice, function(e) paste0("`", unique(e), "`", collapse = ", "), character(1)), names(advice))
  if (length(todo) > 1) todo <- paste0("- ", todo)
  found <- c(sprintf("%s files%s in %d formats were checked against TU/e's preferred formats.",
                     format(nrow(fnd), big.mark = ","), in_zip, n_ext),
             if (n[["warn"]]) sprintf("- %s in a format to improve: %s.", format(n[["warn"]], big.mark = ","), by_ext("warn")),
             if (n[["manual"]]) sprintf("- %s in a format not on TU/e's list, to decide by hand: %s.", format(n[["manual"]], big.mark = ","), by_ext("manual")),
             if (!n[["warn"]] && !n[["manual"]]) "All are in preferred or open formats.")
  list(item = .tdc_item("formats", "2", title, .tdc_worst(fnd$status), found, todo),
       table = tbl[, setdiff(names(tbl), "status")], findings = fnd)
}

# -- 2.2 forbidden and special characters, spaces -------------------------------

# The characters TU/e's guidelines say not to use in a name ("Do not use").
.tdc_forbidden_re <- "[~!@#$%^&*:?]"

# Every file and folder name of the package: path, kind ("file"/"folder"),
# name. Folder paths end in "/".
.tdc_names <- function(files, folders) {
  # paste0() gives "/" for no folders at all, hence the if
  data.frame(path = c(if (length(folders)) paste0(folders, "/"), files$path),
             kind = c(rep("folder", length(folders)), rep("file", nrow(files))),
             name = c(basename(folders), basename(files$path)))
}

# A space or a forbidden character fails; any other character metacheck's
# naming check flags (outside letters, digits, "_", "-" and "."; accented
# letters among them) is a warn.
.tdc_check_name_characters <- function(files, folders, rules) {
  nm <- .tdc_names(files, folders)
  title <- "No spaces or special characters in names"
  rows <- lapply(seq_len(nrow(nm)), function(i) {
    name <- nm$name[[i]]
    flagged <- metacheck:::.file_naming_check_one(name)$rule
    if (!length(flagged)) return(NULL)
    chars <- unique(strsplit(gsub("[A-Za-z0-9._ -]", "", name), "")[[1]])
    forbidden <- chars[grepl(.tdc_forbidden_re, chars)]
    other <- setdiff(chars, forbidden)
    problems <- c(if ("spaces" %in% flagged) "a space",
                  if (length(forbidden)) sprintf("forbidden character%s %s", if (length(forbidden) > 1) "s" else "",
                                                 paste0("`", forbidden, "`", collapse = " ")),
                  if (length(other)) sprintf("other special character%s %s", if (length(other) > 1) "s" else "",
                                             paste0("`", other, "`", collapse = " ")))
    status <- .tdc_worst(c(if ("spaces" %in% flagged) .tdc_sev(rules, "name-spaces"),
                           if (length(forbidden)) .tdc_sev(rules, "name-special-characters"),
                           if (length(other)) .tdc_sev(rules, "name-other-characters")))
    data.frame(path = nm$path[[i]], problem = paste(problems, collapse = "; "), status = status)
  })
  fnd <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(fnd)) {
    return(list(item = .tdc_item("name_characters", "2", title, "pass",
                                 sprintf("None of the %s file and folder names has a space or a special character.",
                                         format(nrow(nm), big.mark = ","))),
                table = NULL, findings = fnd))
  }
  n_fail <- sum(fnd$status == "fail")
  n_warn <- sum(fnd$status == "warn")
  found <- c(if (n_fail) sprintf("%s name%s with a space or a character TU/e says not to use (~ ! @ # $ %% ^ & * : ?).",
                                 format(n_fail, big.mark = ","), if (n_fail == 1) "" else "s"),
             if (n_warn) sprintf("%s name%s with another special character (such as an accented letter or a bracket).",
                                 format(n_warn, big.mark = ","), if (n_warn == 1) "" else "s"))
  todo <- "Use only letters, digits, underscores and dashes, for example `raw_data` instead of `Raw data`."
  tbl <- data.frame(Name = paste0("`", fnd$path, "`"), Problem = fnd$problem,
                    Result = unname(.tdc_result_words[fnd$status]))
  list(item = .tdc_item("name_characters", "2", title, .tdc_worst(fnd$status), found, todo),
       table = tbl, findings = fnd)
}

# -- 2.3 naming conventions -----------------------------------------------------

# Dates in a name that are not written as YYYY-MM-DD, and the date
# placeholders of templates (YYYYMMDD, DD-MM-YYYY).
.tdc_date_patterns <- c(
  "YYYYMMDD" = "(?<![0-9])(?:19|20)[0-9]{2}(?:0[1-9]|1[0-2])(?:0[1-9]|[12][0-9]|3[01])(?![0-9])",
  "YYYY_MM_DD or YYYY.MM.DD" = "(?<![0-9])(?:19|20)[0-9]{2}[_.](?:0[1-9]|1[0-2])[_.](?:0[1-9]|[12][0-9]|3[01])(?![0-9])",
  "DD-MM-YYYY" = "(?<![0-9])(?:0[1-9]|[12][0-9]|3[01])[-_.](?:0[1-9]|1[0-2])[-_.](?:19|20)[0-9]{2}(?![0-9])",
  "placeholder" = "(?<![A-Za-z])(?:YYYYMMDD|DDMMYYYY|MMDDYYYY|YYYY_MM_DD|DD-MM-YYYY|MM-DD-YYYY)(?![A-Za-z])")

# The naming styles a name uses: snake_case ("_"), kebab-case ("-") and
# camelCase/PascalCase (a capital after a small letter). A leading number
# ("01_"), leading and trailing underscores ("__init__") and dates (in any
# format; a date not written as YYYY-MM-DD is reported on its own) do not
# count.
.tdc_name_styles <- function(name) {
  stem <- tools::file_path_sans_ext(name)
  for (p in c("(?:19|20)[0-9]{2}-[0-9]{2}-[0-9]{2}", .tdc_date_patterns[names(.tdc_date_patterns) != "placeholder"])) {
    stem <- gsub(p, "", stem, perl = TRUE)
  }
  stem <- sub("^[0-9]+[ _.-]+", "", stem)
  stem <- gsub("^_+|_+$", "", stem)
  c(if (grepl("_", stem, fixed = TRUE)) "snake_case",
    if (grepl("-", stem, fixed = TRUE)) "kebab-case",
    if (grepl("[a-z][A-Z]", stem)) "camelCase/PascalCase")
}

# Dates not as YYYY-MM-DD (file and folder names), numbered files without
# leading zeros (metacheck's padding check), and folders whose names mix
# naming styles: each a warn.
.tdc_check_name_conventions <- function(files, folders, rules) {
  nm <- .tdc_names(files, folders)
  title <- "Naming conventions (dates, numbering, one style per folder)"
  # dates
  date_problem <- vapply(nm$name, function(n) {
    for (k in names(.tdc_date_patterns)) {
      m <- regmatches(n, regexpr(.tdc_date_patterns[[k]], n, perl = TRUE))
      if (length(m)) {
        return(if (k == "placeholder") sprintf("date placeholder `%s`; write YYYY-MM-DD", m)
               else sprintf("date `%s` written as %s; write YYYY-MM-DD", m, k))
      }
    }
    NA_character_
  }, character(1), USE.NAMES = FALSE)
  has_date <- !is.na(date_problem)
  dates <- data.frame(path = nm$path[has_date], problem = date_problem[has_date],
                      status = rep(.tdc_sev(rules, "name-date-format"), sum(has_date)))
  # zero padding (metacheck)
  pad <- metacheck:::.file_naming_check_padding(files$path)
  pad <- data.frame(path = pad$file_name, problem = pad$detail,
                    status = rep(.tdc_sev(rules, "name-zero-padding"), nrow(pad)))
  # styles per folder
  bare <- sub("/$", "", nm$path)
  parent <- ifelse(grepl("/", bare), dirname(bare), "")
  styles <- lapply(nm$name, .tdc_name_styles)
  mixed <- do.call(rbind, lapply(split(seq_len(nrow(nm)), factor(parent, unique(parent))), function(idx) {
    used <- unique(unlist(styles[idx]))
    if (length(used) < 2) return(NULL)
    examples <- unique(vapply(used, function(s) {
      nm$name[idx][vapply(styles[idx], function(x) s %in% x, logical(1))][[1]]
    }, character(1)))
    problem <- if (length(examples) == 1) {
      sprintf("`%s` mixes %s", examples, paste(used, collapse = " and "))
    } else {
      sprintf("names mix %s (for example %s)", paste(used, collapse = ", "),
              paste0("`", examples, "`", collapse = ", "))
    }
    data.frame(path = if (nzchar(parent[idx[[1]]])) paste0(parent[idx[[1]]], "/") else "(top of the package)",
               problem = problem, status = .tdc_sev(rules, "name-style-mixed"))
  }))
  if (!is.null(mixed)) rownames(mixed) <- NULL
  fnd <- rbind(dates, pad, mixed)
  if (is.null(fnd) || !nrow(fnd)) {
    return(list(item = .tdc_item("name_conventions", "2", title, "pass",
                                 "Dates are written as YYYY-MM-DD, numbered files have leading zeros, and each folder uses one naming style."),
                table = NULL, findings = fnd))
  }
  n_mixed <- if (is.null(mixed)) 0 else nrow(mixed)
  found <- c(if (nrow(dates)) sprintf("%s name%s with a date not written as YYYY-MM-DD.", format(nrow(dates), big.mark = ","), if (nrow(dates) == 1) "" else "s"),
             if (nrow(pad)) sprintf("%s numbered file%s without leading zeros (so they do not sort in order).", format(nrow(pad), big.mark = ","), if (nrow(pad) == 1) "" else "s"),
             if (n_mixed) sprintf("%s folder%s whose names mix snake_case, kebab-case and camelCase.", format(n_mixed, big.mark = ","), if (n_mixed == 1) "" else "s"))
  todo <- c(if (nrow(dates)) "Write dates as YYYY-MM-DD.",
            if (nrow(pad)) "Number files with leading zeros (01, 02, ..., 10).",
            if (n_mixed) "Use one naming style per folder.")
  shown <- ifelse(fnd$path == "(top of the package)", fnd$path, paste0("`", fnd$path, "`"))
  tbl <- data.frame(`Name or folder` = shown, Problem = fnd$problem,
                    Result = unname(.tdc_result_words[fnd$status]), check.names = FALSE)
  list(item = .tdc_item("name_conventions", "2", title, .tdc_worst(fnd$status), found, todo),
       table = tbl, findings = fnd)
}

# -- 2.5 personal data ------------------------------------------------------------

# The Dutch "11-test" of a citizen service number (BSN): 8 or 9 digits (8
# with a leading zero added), 9*d1 + 8*d2 + ... + 2*d8 - d9 divisible by 11.
.tdc_elfproef <- function(x) {
  x <- ifelse(nchar(x) == 8, paste0("0", x), x)
  vapply(x, function(s) {
    if (!grepl("^[0-9]{9}$", s) || s == "000000000") return(FALSE)
    d <- as.integer(strsplit(s, "")[[1]])
    sum(d * c(9:2, -1)) %% 11 == 0
  }, logical(1), USE.NAMES = FALSE)
}

# A column's values that match a value pattern of personal_data.json (BSN,
# phone numbers, TU/e numbers): TRUE when at least `min_share` of its
# non-empty values match.
.tdc_column_matches <- function(values, pat) {
  v <- trimws(as.character(values))
  v <- v[!is.na(v) & nzchar(v)]
  if (length(v) < 3) return(FALSE)
  hit <- grepl(pat$pattern, v, perl = TRUE)
  if (identical(pat$validate, "elfproef")) hit[hit] <- .tdc_elfproef(v[hit])
  mean(hit) >= (pat$min_share %||% 0.5)
}

# Personal data in the package: data_check's findings (values, column
# names, free text, coordinates), the module's value patterns (BSN, phone
# numbers, TU/e numbers) on the tables data_check read, and files named as
# pseudonymisation keys. data.frame(path, column, kind, status).
.tdc_personal_data <- function(dc, files, rules) {
  out <- list()
  f <- dc$findings
  pii_checks <- c("Personal info (values)" = "values that look like personal information",
                  "Personal info (column name)" = "a column name that suggests personal information",
                  "Free-text (may hold PII)" = "free text that may hold personal details",
                  "Geographic coordinates" = "geographic coordinates")
  if (!is.null(f) && nrow(f) && all(c("check", "source_file", "column") %in% names(f))) {
    f <- f[f$check %in% names(pii_checks), , drop = FALSE]
    if (nrow(f)) out[[1]] <- data.frame(path = f$source_file, column = f$column,
                                        kind = unname(pii_checks[f$check]),
                                        status = .tdc_sev(rules, "personal-data"))
  }
  pats <- c(rules$personal$value_patterns, rules$personal$tue_id_patterns)
  for (file in names(dc$previews)) {
    df <- dc$previews[[file]]
    if (!is.data.frame(df)) next
    for (col in names(df)) {
      for (p in pats) {
        if (.tdc_column_matches(df[[col]], p)) {
          out[[length(out) + 1]] <- data.frame(path = file, column = col, kind = p$label,
                                               status = .tdc_sev(rules, "personal-data"))
        }
      }
    }
  }
  keys <- files$path[.tdc_match_any(tolower(basename(files$path)), rules$personal$key_file_names)]
  if (length(keys)) {
    out[[length(out) + 1]] <- data.frame(path = keys, column = "", kind = "a pseudonymisation key file",
                                         status = .tdc_sev(rules, "key-file"))
  }
  res <- do.call(rbind, out)
  if (is.null(res)) res <- data.frame(path = character(0), column = character(0),
                                      kind = character(0), status = character(0))
  res <- res[!duplicated(res[c("path", "column", "kind")]), , drop = FALSE]
  rownames(res) <- NULL
  res
}

.tdc_check_personal_data <- function(pd, n_tables) {
  title <- "No personal data"
  if (!nrow(pd)) {
    return(list(item = .tdc_item("personal_data", "2", title, "pass",
                                 sprintf("No personal data was found in the %d table%s read, and no file is named as a pseudonymisation key.",
                                         n_tables, if (n_tables == 1) "" else "s"),
                                 note = "The check looks for patterns (e-mail addresses, names, BSN and phone numbers, free text, coordinates); it cannot rule out personal data."),
                table = NULL, findings = pd))
  }
  keys <- pd$path[pd$kind == "a pseudonymisation key file"]
  cols <- pd[pd$kind != "a pseudonymisation key file", , drop = FALSE]
  n_cols <- nrow(unique(cols[c("path", "column")]))
  found <- c(if (n_cols) sprintf("%d column%s may hold personal data (see the table).", n_cols, if (n_cols == 1) "" else "s"),
             if (length(keys)) sprintf("%s look%s like a pseudonymisation key.",
                                       .tdc_list(paste0("`", keys, "`"), 5), if (length(keys) == 1) "s" else ""))
  todo <- c(if (n_cols) "Remove direct identifiers or replace them by pseudonyms before archiving, as TU/e's de-identification guidelines describe.",
            if (length(keys)) "Never archive a pseudonymisation key with the data: keep keys encrypted and apart from the data, and delete them when they are no longer needed.")
  # one row per column (or key file), with everything found in it
  key <- paste(pd$path, pd$column, sep = "\r")
  tbl <- do.call(rbind, lapply(split(pd, factor(key, unique(key))), function(g) {
    data.frame(File = paste0("`", g$path[[1]], "`"),
               Column = if (nzchar(g$column[[1]])) paste0("`", g$column[[1]], "`") else "",
               `What was found` = paste(unique(g$kind), collapse = "; "),
               Result = .tdc_result_words[[.tdc_worst(g$status)]], check.names = FALSE)
  }))
  rownames(tbl) <- NULL
  list(item = .tdc_item("personal_data", "2", title, .tdc_worst(pd$status), found, todo),
       table = tbl, findings = pd)
}

# -- data files that cannot be opened -------------------------------------------

# Why a data file could not be opened, in plain words, from data_check's
# warning (`reason`, which can hold the file's path on this computer) and the
# file's first bytes: a text file saved with a spreadsheet's extension (as
# ImageJ saves its Results.xls), or a spreadsheet that is damaged.
.tdc_unreadable_reason <- function(name, location, reason) {
  ext <- tolower(tools::file_ext(name))
  head <- if (!is.na(location) && file.exists(location)) {
    tryCatch(readBin(location, "raw", 4096), error = function(e) raw(0))
  } else raw(0)
  # an Excel or OpenDocument file always has zero bytes near its start
  is_text <- length(head) > 0 && !any(head == as.raw(0))
  if (ext %in% c("xls", "xlsx", "xlsm", "ods") && is_text) {
    first <- strsplit(rawToChar(head), "\r?\n")[[1]][1]
    if (is.na(first)) first <- ""
    sep <- if (grepl("\t", first)) "tsv" else if (grepl("[,;]", first)) "csv" else "txt"
    what <- c(tsv = "tab-separated values", csv = "comma-separated values", txt = "plain text")[[sep]]
    return(sprintf("It is a text file (%s) with the extension .%s, not a spreadsheet; rename it to .%s.", what, ext, sep))
  }
  if (ext %in% c("xlsx", "xlsm") && grepl("zip file .* cannot be opened", reason)) {
    return(sprintf("It is not a valid .%s file: it may be damaged, or another kind of file with this extension.", ext))
  }
  if (ext == "xls" && grepl("libxls error", reason)) {
    return("It is not a valid .xls file: it may be damaged or password-protected, or another kind of file with this extension.")
  }
  # data_check's own words, without paths on this computer
  reason <- gsub("[[:space:]]+", " ", reason)
  reason <- gsub("filepath: [^ ]+", "", reason)
  if (!is.na(location)) reason <- gsub(location, name, reason, fixed = TRUE)
  # any other path, on Linux (/tmp/x/a.csv) or Windows (C:\Users\x\a.csv)
  reason <- gsub("'?(?:[A-Za-z]:)?[\\\\/][^ ']*[\\\\/]([^\\\\/ ']+)'?", "\\1", reason, perl = TRUE)
  reason <- trimws(reason)
  if (!nzchar(reason)) "The reader gave no reason." else reason
}

# The data files data_check could not open: `unreadable` holds data_check's
# own warnings for them ("<file name>: <reason>"), `dc_error` the error when
# data_check itself stopped; `files` is the package's file table, to look
# inside each file. A file that cannot be opened here may also not open for
# whoever reuses the data (damaged, empty, password-protected, or text with a
# spreadsheet's extension), so a person must look: manual.
.tdc_check_data_readable <- function(unreadable, dc_error, rules, files = NULL) {
  title <- "Data files can be opened"
  if (!is.null(dc_error)) {
    return(list(item = .tdc_check_failed("data_readable", "2", title, dc_error,
                                         what = "Reading the data files (metacheck's data_check)"),
                table = NULL, findings = NULL))
  }
  if (!length(unreadable)) {
    return(list(item = .tdc_item("data_readable", "2", title, "pass",
                                 "Every data file the check reads (tables, spreadsheets, statistics files) could be opened."),
                table = NULL, findings = NULL))
  }
  file <- sub(":.*$", "", unreadable)
  why <- trimws(sub("^[^:]*:", "", unreadable))
  # data_check names a file by its name only; its path in the package and its
  # location come from the file table
  at <- if (!is.null(files)) match(file, files$name) else rep(NA_integer_, length(file))
  path <- ifelse(is.na(at), file, files$path[at])
  location <- if (!is.null(files)) files$location[at] else rep(NA_character_, length(file))
  why <- vapply(seq_along(file), function(i) .tdc_unreadable_reason(file[[i]], location[[i]], why[[i]]), character(1))
  file <- path
  text_file <- grepl("^It is a text file", why)
  tbl <- data.frame(File = paste0("`", file, "`"), `Why it could not be opened` = why, check.names = FALSE)
  list(item = .tdc_item("data_readable", "2", title, .tdc_sev(rules, "data-unreadable"),
                        sprintf("%d data file%s could not be opened: %s (see the table). %s not checked for personal data, missing values or the codebook.",
                                length(file), if (length(file) == 1) "" else "s", .tdc_list(paste0("`", file, "`"), 5),
                                if (length(file) == 1) "It was" else "They were"),
                        c(if (any(text_file)) "Give text files the extension of what they hold (.tsv, .csv or .txt).",
                          if (!all(text_file)) sprintf("Open %s yourself. If it is damaged, empty or password-protected, replace it with a working copy, preferably in an open format such as CSV.",
                                                       if (any(text_file)) "the other files" else if (length(file) == 1) "the file" else "each file"))),
       table = tbl, findings = data.frame(path = file, problem = why))
}

# -- 2.6 columns against the codebook -------------------------------------------

# codebook_check's matching: every column of a table that no codebook or
# README variable list describes is a warn; without any codebook, not
# applicable (the components table says whether there is one).
.tdc_check_codebook_columns <- function(cb, rules) {
  title <- "Columns are described in the codebook"
  n_vars <- cb$summary_table$codebook_var_n %||% 0
  tbl_cb <- cb$table
  if (is.null(cb) || !sum(n_vars, na.rm = TRUE) || is.null(tbl_cb) || !nrow(tbl_cb)) {
    return(list(item = .tdc_item("codebook_columns", "2", title, "na",
                                 "No codebook or variable list was found to compare the columns with (see \"Data dictionary or codebook\" in the components table)."),
                table = NULL, findings = NULL))
  }
  undescribed <- tbl_cb[!tbl_cb$label_status %in% c("labelled", "conflicted"), , drop = FALSE]
  if (!nrow(undescribed)) {
    return(list(item = .tdc_item("codebook_columns", "2", title, "pass",
                                 sprintf("All %d columns are described in %s.", nrow(tbl_cb),
                                         .tdc_list(paste0("`", unique(stats::na.omit(tbl_cb$label_source)), "`"), 3))),
                table = NULL, findings = undescribed))
  }
  by_file <- split(undescribed$column_name, factor(undescribed$source_file, unique(undescribed$source_file)))
  tbl <- data.frame(File = paste0("`", names(by_file), "`"),
                    `Columns not described` = vapply(by_file, function(x) .tdc_list(paste0("`", x, "`"), 12), character(1)),
                    check.names = FALSE)
  rownames(tbl) <- NULL
  list(item = .tdc_item("codebook_columns", "2", title, .tdc_sev(rules, "codebook-columns-undocumented"),
                        sprintf("%d of the %d columns are not described in the codebook or the README's variable list (see the table).",
                                nrow(undescribed), nrow(tbl_cb)),
                        "Describe these columns in the codebook or the README."),
       table = tbl, findings = undescribed[, c("source_file", "column_name")])
}

# -- 2.7 missing values and units -------------------------------------------------

# The notations for a missing value a column uses: empty cells, NA, NaN, NULL,
# N/A, "." and "-" as written, and the codes -9, -99, -999 and -9999 in a
# numeric column whose other values are all zero or more (so a real negative
# measurement is not taken for a code).
.tdc_missing_notations <- function(x) {
  v <- if (is.character(x)) trimws(x) else ifelse(is.na(x), NA, trimws(format(x, scientific = FALSE, trim = TRUE)))
  found <- character(0)
  if (any(is.na(v) | v == "")) found <- c(found, "empty cells")
  up <- toupper(v)
  words <- c("NA" = "NA", "NAN" = "NaN", "NULL" = "NULL", "N/A" = "N/A", "." = ".", "-" = "-")
  found <- c(found, unname(words[names(words) %in% up]))
  rest <- v[!is.na(v) & v != "" & !up %in% names(words)]
  num <- suppressWarnings(as.numeric(rest))
  if (length(rest) && all(!is.na(num))) {
    codes <- c(-9, -99, -999, -9999)
    used <- codes[codes %in% num]
    if (length(used) && all(num[!num %in% codes] >= 0)) found <- c(found, as.character(used))
  }
  found
}

# A table as text, every cell as written: CSV and TSV read again from the
# file (data_check's tables turn "NA" and empty cells into the same missing
# value), other tables from data_check's copy.
.tdc_raw_table <- function(file_row, preview) {
  loc <- file_row$location
  if (!is.na(loc) && file.exists(loc) && file_row$ext %in% c("csv", "tsv")) {
    sep <- if (file_row$ext == "tsv") "\t" else ","
    first <- tryCatch(readLines(loc, n = 1, warn = FALSE), error = function(e) "")
    if (file_row$ext == "csv" && length(first) && lengths(regmatches(first, gregexpr(";", first))) >
        lengths(regmatches(first, gregexpr(",", first)))) sep <- ";"
    raw <- tryCatch(utils::read.table(loc, sep = sep, header = TRUE, colClasses = "character",
                                      na.strings = character(0), quote = "\"", comment.char = "",
                                      check.names = FALSE, nrows = 1e5, fill = TRUE, encoding = "UTF-8"),
                    error = function(e) NULL)
    if (!is.null(raw)) {
      # a file in the older Windows encoding (as Excel often saves CSV) is
      # not valid UTF-8, which text functions such as trimws() refuse;
      # read as Latin-1 instead, as .tdc_read_text() does
      to_utf8 <- function(x) {
        bad <- !is.na(x) & !validUTF8(x)
        x[bad] <- iconv(x[bad], "latin1", "UTF-8", sub = "")
        x
      }
      raw[] <- lapply(raw, to_utf8)
      names(raw) <- to_utf8(names(raw))
      return(raw)
    }
  }
  preview
}

.tdc_unit_tokens <- c("ms", "s", "sec", "secs", "min", "h", "hr", "hz", "khz", "mhz", "ghz", "nm", "um",
                      "mm", "cm", "m", "km", "mg", "g", "kg", "ml", "l", "mv", "v", "kv", "ma", "a", "mw",
                      "w", "kw", "j", "kj", "k", "c", "degc", "pa", "kpa", "hpa", "bar", "db", "px", "deg",
                      "rad", "mol", "mmol", "pct", "percent", "ppm", "lux", "rpm", "bpm", "kcal", "years", "yrs")

# Mixed notations for missing values (a warn per table), and, as information
# only, the numeric columns whose unit is not recognised from the column name
# (data_check's unit, or a unit at the end of the name such as "_cm"), the
# codebook or the README's variable list.
.tdc_check_missing_units <- function(dc, files, doc_text, rules) {
  title <- "One notation for missing values per table"
  previews <- dc$previews %||% list()
  data_files <- files[files$data_type %in% "data", , drop = FALSE]
  rows <- list()
  for (file in names(previews)) {
    fr <- data_files[data_files$name == file | data_files$path == file, , drop = FALSE]
    tbl <- .tdc_raw_table(if (nrow(fr)) fr[1, ] else data.frame(location = NA, ext = ""), previews[[file]])
    if (!is.data.frame(tbl) || !ncol(tbl)) next
    per_col <- lapply(tbl, .tdc_missing_notations)
    used <- unique(unlist(per_col))
    if (length(used) >= 2) {
      cols <- vapply(used, function(u) .tdc_list(paste0("`", names(per_col)[vapply(per_col, function(p) u %in% p, logical(1))], "`"), 4), character(1))
      rows[[length(rows) + 1]] <- data.frame(path = file, notations = paste(sprintf("%s (%s)", used, cols), collapse = "; "))
    }
  }
  mixed <- do.call(rbind, rows)
  # units, as information
  ct <- dc$table
  no_unit <- character(0)
  if (!is.null(ct) && nrow(ct)) {
    num <- ct[ct$representation %in% "numeric" & !ct$role %in% "identifier" & is.na(ct$unit), , drop = FALSE]
    suffix <- tolower(sub("^.*[_.-]", "", num$column_name))
    by_name <- grepl("[_.-]", num$column_name) & suffix %in% .tdc_unit_tokens
    doc_lines <- tolower(unlist(strsplit(doc_text %||% "", "\r?\n")))
    unit_re <- paste0("(?<![a-z])(?:unit|", paste(.tdc_unit_tokens, collapse = "|"), ")(?![a-z])|[%°]")
    in_docs <- vapply(tolower(num$column_name), function(cn) {
      l <- doc_lines[grepl(cn, doc_lines, fixed = TRUE)]
      length(l) > 0 && any(grepl(unit_re, l, perl = TRUE))
    }, logical(1))
    left <- num[!by_name & !in_docs, , drop = FALSE]
    no_unit <- sprintf("`%s` (`%s`)", left$column_name, left$source_file)
  }
  units <- if (length(no_unit)) {
    list(found = sprintf("%d numeric column%s %s no recognisable unit in the column name, the codebook or the README: %s.",
                         length(no_unit), if (length(no_unit) == 1) "" else "s", if (length(no_unit) == 1) "has" else "have", .tdc_list(no_unit, 5)),
         todo = "Where a column has a unit, name it in the codebook or the README.")
  }
  units_note <- if (!is.null(units)) paste("For information:", units$found, units$todo)
  if (!length(previews)) {
    return(list(item = .tdc_item("missing_values", "2", title, "na", "No tables were read."),
                table = NULL, findings = NULL, note = units_note, units = units))
  }
  if (is.null(mixed)) {
    return(list(item = .tdc_item("missing_values", "2", title, "pass",
                                 sprintf("Each of the %d table%s read uses at most one notation for missing values.",
                                         length(previews), if (length(previews) == 1) "" else "s")),
                table = NULL, findings = NULL, note = units_note, units = units))
  }
  tbl <- data.frame(File = paste0("`", mixed$path, "`"), `Notations for missing values (columns)` = mixed$notations,
                    check.names = FALSE)
  list(item = .tdc_item("missing_values", "2", title, .tdc_sev(rules, "missing-codes-mixed"),
                        sprintf("%d table%s use%s more than one notation for missing values (for example empty cells and -999).",
                                nrow(mixed), if (nrow(mixed) == 1) "" else "s", if (nrow(mixed) == 1) "s" else ""),
                        "Use one notation per table, and say which in the codebook or the README."),
       table = tbl, findings = mixed, note = units_note, units = units)
}

# -- 2.8 code check ----------------------------------------------------------------

# metacheck's code_check as it is: anything it flags is a warn, never a fail
# (TU/e only wants to experiment with it); its report is shown under the line.
.tdc_check_code <- function(cc, rules) {
  title <- "Code check (metacheck's code_check)"
  if (is.null(cc) || identical(cc$traffic_light, "na")) {
    return(list(item = .tdc_item("code_check", "2", title, "na", "The package has no code files."),
                table = NULL, note = NULL))
  }
  status <- if (cc$traffic_light %in% c("red", "yellow")) .tdc_sev(rules, "code-check-flagged") else "pass"
  fc <- .tdc_code_findings(cc)
  report <- gsub("^(#+) ", "\\1## ", unlist(strsplit(paste(cc$report %||% character(0), collapse = "\n"), "\n")))
  list(item = .tdc_item("code_check", "2", title, status,
                        c(fc$files, if (length(fc$found)) paste0("- ", fc$found) else "No problems were found."),
                        if (length(fc$todo) > 1) paste0("- ", fc$todo) else fc$todo),
       table = NULL,
       note = c("::: {.callout-note collapse=\"true\" title=\"code_check's report\"}", report, ":::"))
}

# What code_check found, per kind, from its table of code files (one row per
# file), with what to do about each: list(files, found, todo).
.tdc_code_findings <- function(cc) {
  tbl <- cc$table
  if (!is.data.frame(tbl)) tbl <- data.frame(file_name = character(0))
  col <- function(x) if (x %in% names(tbl)) tbl[[x]] else rep(NA, nrow(tbl))
  shown <- if ("file_path" %in% names(tbl)) ifelse(is.na(tbl$file_path), tbl$file_name, tbl$file_path) else tbl$file_name
  shown <- paste0("`", shown, "`")
  n <- function(k, one, more) sprintf("%d %s", k, if (k == 1) one else more)
  langs <- table(factor(col("language")))
  files <- if (nrow(tbl)) {
    sprintf("%s: %s.", n(nrow(tbl), "code file", "code files"),
            paste(sprintf("%d %s", as.integer(langs), names(langs)), collapse = ", "))
  }
  found <- todo <- character(0)
  add <- function(f, t) {
    found <<- c(found, f)
    todo <<- c(todo, t)
  }

  abs <- which(col("code_abs_path") > 0)
  if (length(abs)) {
    paths <- vapply(abs, function(i) {
      p <- trimws(strsplit(col("absolute_paths")[[i]] %||% "", ",", fixed = TRUE)[[1]])
      sprintf("%s in %s", .tdc_list(paste0("`", p[nzchar(p)], "`"), 2), shown[[i]])
    }, character(1))
    add(sprintf("%s absolute paths, which only exist on the author's computer: %s.",
                n(length(abs), "file has", "files have"), paste(paths, collapse = "; ")),
        "Replace the absolute paths with paths relative to the package.")
  }
  miss <- which(col("loaded_files_missing") > 0)
  if (length(miss)) {
    add(sprintf("%s files that are not in the package: %s.", n(length(miss), "file loads", "files load"),
                paste(sprintf("%s (in %s)", col("loaded_files_missing_names")[miss], shown[miss]), collapse = "; ")),
        "Add the missing files, or correct their names in the code.")
  }
  setwd <- which(col("code_setwd") > 0)
  if (length(setwd)) {
    add(sprintf("%s `setwd()`: %s.", n(length(setwd), "file calls", "files call"), .tdc_list(shown[setwd], 5)),
        "Remove `setwd()`, and use paths relative to the package.")
  }
  inst <- which(col("code_install_packages") > 0)
  if (length(inst)) {
    add(sprintf("%s `install.packages()`: %s.", n(length(inst), "file calls", "files call"), .tdc_list(shown[inst], 5)),
        "Remove `install.packages()`, and list the packages the code needs instead.")
  }
  none <- which(col("percentage_comment") == 0)
  if (length(none)) {
    add(sprintf("%s no comments: %s.", n(length(none), "file has", "files have"), .tdc_list(shown[none], 5)),
        "Add comments that explain what the code does.")
  }
  lib <- which(col("library_max_between") > 3)
  if (length(lib)) {
    add(sprintf("%s libraries in more than one place: %s.", n(length(lib), "file loads", "files load"), .tdc_list(shown[lib], 5)),
        "Load all libraries in one block near the top of each file.")
  }
  parse <- which(col("parse_error") %in% TRUE)
  if (length(parse)) {
    add(sprintf("%s could not be read as R code: %s.", n(length(parse), "file", "files"), .tdc_list(shown[parse], 5)),
        "Correct the code so it can be read.")
  }
  # whether the versions are written down, per language (metacheck 0.4.0.6
  # checks Python package lists too; before, only R's)
  vp <- cc$version_pin
  lang <- names(langs)
  env_pinned <- if (exists(".code_env_pinned", asNamespace("metacheck"))) {
    metacheck:::.code_env_pinned(vp, lang)
  } else isTRUE(vp$pinned)
  if (!is.null(vp) && !env_pinned) {
    r_missing <- "R" %in% lang && !isTRUE(vp$pinned)
    py_missing <- "Python" %in% lang && !isTRUE(vp$python_pinned)
    add("No list of the package versions the code used was found.",
        c(if (r_missing) "Add an `renv.lock` file or the output of `sessionInfo()`, with the R package versions used.",
          if (py_missing) "Add a `requirements.txt` with the Python package versions used.",
          if (!r_missing && !py_missing) "Write down the software and package versions used in the README."))
  }
  list(files = files, found = found, todo = todo)
}

# -- running the code -----------------------------------------------------------------

# metacheck's reproducibility_check, run on the code (`rp`: its output, an
# error, or NULL when the code was not run): every script that errored, timed
# out or could not be run is a warn, as for the code check; its report is
# shown under the line.
.tdc_check_code_runs <- function(rp, run_code, rules) {
  title <- "The code runs (metacheck's reproducibility_check)"
  none <- function(status, found, todo = NULL) {
    list(item = .tdc_item("code_runs", "2", title, status, found, todo), table = NULL, note = NULL)
  }
  if (!isTRUE(run_code)) return(none("na", "The code was not run (ask for it with `run_code = TRUE`)."))
  if (inherits(rp, "error") || identical(rp$traffic_light, "fail")) {
    why <- if (inherits(rp, "error")) conditionMessage(rp) else paste(rp$report %||% "", collapse = " ")
    return(none(.tdc_sev(rules, "code-run-unclear"), sprintf("The code could not be run: %s", why),
                "Check by hand whether the code runs."))
  }
  rr <- rp$run_results
  if (is.null(rp) || !is.data.frame(rr) || !nrow(rr) || !"outcome" %in% names(rr)) {
    return(none("na", "The package has no R or Python code that could be run."))
  }
  oc <- rr$outcome
  names_of <- function(o) .tdc_list(paste0("`", rr$file_name[oc %in% o], "`"), 5)
  n <- function(o) sum(oc %in% o)
  first_line <- function(x) {
    x <- trimws(strsplit(x %||% "", "\n", fixed = TRUE)[[1]])
    x <- x[nzchar(x)]
    if (length(x)) .tdc_short_quote(x[[length(x)]], 150) else ""
  }
  errors <- vapply(which(oc == "errored"), function(i) {
    e <- if ("error" %in% names(rr)) first_line(rr$error[[i]]) else ""
    sprintf("- `%s`%s", rr$file_name[[i]], if (nzchar(e)) sprintf(": \"%s\"", e) else "")
  }, character(1))
  ran <- n(c("ran_ok", "errored", "timed_out"))
  # files other files import or source are not scripts of their own
  own <- length(oc) - n(c("run_by_import", "run_by_source"))
  found <- c(sprintf("%d of %d script%s %s run: %d without an error, %d with an error, %d still running at the time limit.",
                     ran, own, if (own == 1) "" else "s", if (ran == 1) "was" else "were",
                     n("ran_ok"), n("errored"), n("timed_out")),
             if (n("skipped_missing_inputs")) sprintf("Not run, because files they read are not in the package: %s.", names_of("skipped_missing_inputs")),
             if (n("dependency_unavailable")) sprintf("Not run, because a package they need could not be installed: %s.", names_of("dependency_unavailable")),
             if (n("not_parsed")) sprintf("Not run, because the code could not be read: %s.", names_of("not_parsed")),
             if (n("timed_out")) sprintf("Still running at the time limit: %s.", names_of("timed_out")),
             if (n(c("run_by_import", "run_by_source"))) sprintf("Not run on their own, because other files import or source them: %s.",
                                                                names_of(c("run_by_import", "run_by_source"))),
             if (length(errors)) "The scripts with an error, and the last line of their error:",
             errors)
  todo <- c(if (n("errored")) "Make each script run from the package as it is, and correct the errors.",
            if (n("skipped_missing_inputs")) "Add the files the scripts read, or say in the README where they can be found.",
            if (n("dependency_unavailable")) "List the packages the code needs, with their versions.",
            if (n("not_parsed")) "Correct the code so it can be read.",
            if (n("timed_out")) "Say in the README how long the scripts take to run.")
  if (length(todo) > 1) todo <- paste0("- ", todo)
  status <- if (n(c("errored", "timed_out", "skipped_missing_inputs", "not_parsed", "dependency_unavailable"))) {
    .tdc_sev(rules, "code-run-problem")
  } else "pass"
  report <- gsub("^(#+) ", "\\1## ", unlist(strsplit(paste(rp$report %||% character(0), collapse = "\n"), "\n")))
  list(item = .tdc_item("code_runs", "2", title, status, found, todo,
                        note = "Run by metacheck's reproducibility_check; its report shows the output of each script."),
       table = NULL,
       note = c("::: {.callout-note collapse=\"true\" title=\"reproducibility_check's report\"}", report, ":::"))
}

# -- zipped packages ------------------------------------------------------------------

# "Data packages should not be zipped before submission for RAPS": a package
# handed in as a zip, and archives inside the package, fail.
.tdc_check_zipped <- function(pkg, files, rules) {
  title <- "Not zipped"
  archives <- files$path[files$ext %in% .tdc_archive_exts]
  if (!isTRUE(pkg$zipped) && !length(archives)) {
    return(list(item = .tdc_item("zipped", "2", title, "pass", "The package is not zipped and holds no archives."),
                table = NULL))
  }
  found <- c(if (isTRUE(pkg$zipped)) "The package was handed in as a zip.",
             if (length(archives)) sprintf("The package holds %d archive%s: %s.", length(archives),
                                           if (length(archives) == 1) "" else "s", .tdc_list(paste0("`", archives, "`"), 5)))
  list(item = .tdc_item("zipped", "2", title, .tdc_sev(rules, "package-zipped"), found,
                        "Hand in the folder, with its files unpacked.",
                        "Data packages should not be zipped before submission for RAPS."), table = NULL)
}

# -- 2.4 junk and temporary files -----------------------------------------------

# repo_check's junk files (useless_files), and junk beside a wrapper folder
# (`outer_junk`, see .tdc_package()): "problem" kinds fail, version control
# and editor settings warn.
.tdc_check_junk <- function(useless, outer_junk, rules) {
  title <- "No junk or temporary files"
  u <- if (!is.null(useless) && nrow(useless)) data.frame(path = useless$file_path, kind = useless$kind) else
    data.frame(path = character(0), kind = character(0))
  if (length(outer_junk)) {
    u <- rbind(u, data.frame(path = outer_junk, kind = metacheck:::.useless_file_kind(sub("/$", "", outer_junk))))
  }
  if (!nrow(u)) {
    return(list(item = .tdc_item("junk", "2", title, "pass", "The package has no junk or temporary files."),
                table = NULL, findings = u))
  }
  u$status <- ifelse(metacheck:::.useless_file_severity(u$kind) == "problem",
                     .tdc_sev(rules, "junk-problem"), .tdc_sev(rules, "junk-suggestion"))
  groups <- split(u, factor(u$kind, unique(u$kind)))
  tbl <- do.call(rbind, lapply(groups, function(g) {
    data.frame(Kind = g$kind[[1]],
               Files = sprintf("%s: %s", format(nrow(g), big.mark = ","), .tdc_list(paste0("`", g$path, "`"), 3)),
               Result = .tdc_result_words[[g$status[[1]]]], status = g$status[[1]])
  }))
  tbl <- tbl[order(match(tbl$status, .tdc_status_levels)), , drop = FALSE]
  rownames(tbl) <- NULL
  n_problem <- sum(u$status == .tdc_sev(rules, "junk-problem"))
  kinds <- vapply(groups, function(g) sprintf("%s %s", format(nrow(g), big.mark = ","), g$kind[[1]]), character(1))
  found <- sprintf("%s junk or temporary file%s: %s.", format(nrow(u), big.mark = ","), if (nrow(u) == 1) "" else "s",
                   paste(kinds, collapse = ", "))
  todo <- c(if (n_problem) "Remove these files before archiving.",
            if (any(u$status == .tdc_sev(rules, "junk-suggestion"))) "Keep version-control folders (.git) and editor settings (.idea, .vscode) only on purpose.")
  why <- if (n_problem) "Files the operating system or an editor adds, backups, temporary files and R history (which can hold passwords) do not belong in a data package."
  list(item = .tdc_item("junk", "2", title, .tdc_worst(u$status), found, todo, why),
       table = tbl[, setdiff(names(tbl), "status")], findings = u)
}
