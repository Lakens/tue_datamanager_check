# The package to check: where it is, and its file and folder tables.

# The package folder. A zip is unpacked into a temporary folder so its
# contents can be checked (RAPS asks for unzipped packages; section 2 reports
# it). A folder that holds only one folder (apart from junk files such as
# .DS_Store) is a wrapper: the inner folder is the package. Junk files beside
# a wrapper folder (__MACOSX/ and .DS_Store at the top of a zip) lie outside
# the package folder that repo_check lists, so they are kept in `outer_junk`
# (paths from the top of what was handed in) for the junk check.
# Returns list(path, zipped, wrapper, outer_junk, given); path is NULL without
# local_path.
.tdc_package <- function(local_path) {
  out <- list(path = NULL, zipped = FALSE, wrapper = NULL, outer_junk = character(0),
              given = local_path)
  if (is.null(local_path)) return(out)
  if (!file.exists(local_path)) stop("No folder or file found at ", local_path)
  path <- normalizePath(local_path, winslash = "/", mustWork = TRUE)
  if (!dir.exists(path)) {
    if (!grepl("\\.zip$", path, ignore.case = TRUE)) {
      stop("local_path must be a folder, or a zip of one: ", local_path)
    }
    tmp <- file.path(tempdir(), paste0("tdc_", tools::file_path_sans_ext(basename(path))))
    unlink(tmp, recursive = TRUE)
    dir.create(tmp)
    utils::unzip(path, exdir = tmp)
    out$zipped <- TRUE
    path <- tmp
  }
  prefix <- ""
  repeat {
    entries <- list.files(path, all.files = TRUE, no.. = TRUE)
    junk <- metacheck:::.is_useless_file(entries)
    real <- entries[!junk]
    if (length(real) == 1 && dir.exists(file.path(path, real))) {
      out$wrapper <- c(out$wrapper, real)
      out$outer_junk <- c(out$outer_junk, paste0(prefix, entries[junk],
                                                 ifelse(dir.exists(file.path(path, entries[junk])), "/", "")))
      prefix <- paste0(prefix, real, "/")
      path <- file.path(path, real)
    } else break
  }
  out$path <- path
  out
}

# The file table from repo_check's table: path (relative, "/"), name, ext,
# data_type, doc_role, size, location and repo_url. Junk files are not in it.
.tdc_files <- function(tbl) {
  if (is.null(tbl) || nrow(tbl) == 0) {
    return(data.frame(path = character(0), name = character(0), ext = character(0),
                      data_type = character(0), doc_role = character(0),
                      size = numeric(0), location = character(0), repo_url = character(0)))
  }
  col <- function(n) if (n %in% names(tbl)) tbl[[n]] else rep(NA, nrow(tbl))
  path <- col("file_path")
  path[is.na(path) | !nzchar(path)] <- col("file_name")[is.na(path) | !nzchar(path)]
  data.frame(path = gsub("\\\\", "/", path),
             name = col("file_name"),
             ext = tolower(tools::file_ext(col("file_name"))),
             data_type = col("data_type"),
             doc_role = col("doc_role"),
             size = suppressWarnings(as.numeric(col("file_size"))),
             location = col("file_location"),
             repo_url = col("repo_url"))
}

# A description of the package for the LLM (the revised README): the files
# at the top, and per folder (down to 3 levels) its own files and formats and
# what lies in its subfolders, so a package of thousands of files is
# described in a few dozen lines rather than file by file.
.tdc_package_info <- function(files, folders) {
  fmt_of <- function(ext) {
    e <- sort(table(ifelse(nzchar(ext), paste0(".", ext), "no extension")), decreasing = TRUE)
    paste(sprintf("%s %s", format(as.integer(e), big.mark = ",", trim = TRUE), names(e)), collapse = ", ")
  }
  dir_of <- ifelse(grepl("/", files$path), dirname(files$path), "")
  top_files <- files$path[dir_of == ""]
  shown <- folders[lengths(strsplit(folders, "/", fixed = TRUE)) <= 3]
  lines <- vapply(shown, function(f) {
    own <- dir_of == f
    below <- startsWith(files$path, paste0(f, "/")) & !own
    subs <- folders[dirname(folders) == f]
    paste0(f, "/: ",
           if (any(own)) sprintf("%d file%s (%s)", sum(own), if (sum(own) == 1) "" else "s", fmt_of(files$ext[own])) else "no files of its own",
           if (length(subs)) sprintf("; %d subfolder%s with %s file%s (%s)", length(subs), if (length(subs) == 1) "" else "s",
                                     format(sum(below), big.mark = ","), if (sum(below) == 1) "" else "s",
                                     if (any(below)) fmt_of(files$ext[below]) else "none") else "")
  }, character(1))
  paste0(
    "Files at the top: ", if (length(top_files)) paste(top_files, collapse = ", ") else "(none)", "\n",
    "All formats: ", fmt_of(files$ext), "\n",
    "Folders:\n", if (length(lines)) paste(lines, collapse = "\n") else "(none)")
}

# Every folder of the package (relative paths), without junk and hidden
# folders: for a local folder listed from disk (so empty folders are in it),
# else made from the files' paths.
.tdc_folders <- function(path, files) {
  if (!is.null(path)) {
    f <- list.dirs(path, recursive = TRUE, full.names = FALSE)
  } else {
    f <- unique(unlist(lapply(strsplit(files$path, "/", fixed = TRUE), function(s) {
      if (length(s) < 2) return(character(0))
      vapply(seq_len(length(s) - 1), function(i) paste(s[seq_len(i)], collapse = "/"), character(1))
    })))
  }
  f <- f[nzchar(f)]
  f[!metacheck:::.is_useless_file(f) & !grepl("(^|/)\\.", f)]
}
