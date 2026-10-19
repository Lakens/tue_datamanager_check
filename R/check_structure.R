# Section 1.1 of the RDLC document: components and structure
# (SPECIFICATION.md, section 1.1).
#
# `files` is the package's file table (see .tdc_files() in the module file):
# path (relative, "/"), name, ext, data_type, doc_role, size. Junk files are
# not in it (repo_check leaves them out). `folders` is every folder of the
# package (relative paths), empty ones included for a local folder, junk and
# hidden folders left out.

# The package components of TU/e's archiving guidelines ("Step 1: Check what
# you should archive"), as a table. Every component is optional: one that is
# not found is shown as such, without a warning. A component found in another
# format than the guidelines ask gives a warn; raw data that is not marked
# while other data exists is manual; a package without any data fails.
# `readme_sections` are the ids of the TU/e README template sections the
# README has (from section 1.2); a README section "Data-specific information"
# counts as the codebook. Returns list(item, table, paths), where `paths`
# holds every file of each component (by component id), for section 1.3.
.tdc_check_components <- function(files, readme_sections, rules) {
  base <- tolower(files$name)
  segs <- strsplit(tolower(files$path), "/", fixed = TRUE)
  in_folder <- function(patterns) {
    vapply(segs, function(s) {
      s <- s[-length(s)]
      length(s) > 0 && any(.tdc_match_any(s, patterns) | .tdc_match_any(.tdc_norm_name(s), patterns))
    }, logical(1))
  }
  is_code <- files$data_type %in% "code"
  is_readme <- files$doc_role %in% "readme"
  is_doc <- files$data_type %in% "documentation" & !is_readme
  document_groups <- c("Scientific documents", "Administrative documents")

  # Each data file belongs to ONE data component: the one whose folder rule
  # matches its top-most folder ("raw_data/fig5/x.tif" is raw data, although
  # "fig5" is also a figure folder), else the first whose name rule matches.
  data_comps <- Filter(function(c) c$group == "Data", rules$components)
  data_of <- vapply(seq_len(nrow(files)), function(i) {
    if (is_code[[i]] || is_readme[[i]]) return(NA_character_)
    s <- segs[[i]]
    for (seg in s[-length(s)]) {
      for (c in data_comps) {
        if (!is.null(c$match$folders) &&
            any(.tdc_match_any(c(seg, .tdc_norm_name(seg)), c$match$folders))) return(c$id)
      }
    }
    for (c in data_comps) {
      if (!is.null(c$match$names) && any(.tdc_match_any(base[[i]], c$match$names))) return(c$id)
    }
    NA_character_
  }, character(1))

  rows <- lapply(rules$components, function(comp) {
    m <- comp$match
    hit <- rep(FALSE, nrow(files))
    if (!is.null(m$names)) hit <- hit | .tdc_match_any(base, m$names)
    if (!is.null(m$folders)) hit <- hit | in_folder(m$folders)
    if (!is.null(m$doc_role)) hit <- hit | (files$doc_role %in% unlist(m$doc_role))
    if (!is.null(m$data_type)) hit <- hit | (files$data_type %in% unlist(m$data_type))
    if (!is.null(m$exts)) hit <- hit | (files$ext %in% tolower(unlist(m$exts)))
    # documents are found among the files metacheck classifies as documentation
    if (comp$group %in% document_groups) hit <- hit & is_doc
    # data is never code or the README, and each data file is in one component
    if (comp$group == "Data") hit <- data_of %in% comp$id
    # the README counts only at the top of the package (see section 1.2)
    if (comp$id == "readme") hit <- hit & is_readme & !grepl("/", files$path)
    in_readme <- !is.null(m$readme_sections) &&
      any(unlist(m$readme_sections) %in% readme_sections)
    matched <- files$path[hit]

    wrong_format <- character(0)
    if (length(comp$formats) && length(matched)) {
      wrong_format <- matched[!files$ext[hit] %in% tolower(unlist(comp$formats))]
    }
    found <- length(matched) > 0 || in_readme
    status <- if (comp$required == "not_expected" && !found) {
      "na"
    } else if (comp$required == "recommended" && !found) {
      .tdc_sev(rules, "component-recommended-missing")
    } else if (!found) {
      "not found"
    } else if (length(wrong_format)) {
      .tdc_sev(rules, "component-wrong-format")
    } else {
      "pass"
    }
    note <- if (length(wrong_format)) {
      sprintf("Asked: %s. In another format: %s.",
              paste(toupper(unlist(comp$formats)), collapse = " or "),
              .tdc_list(wrong_format, 3))
    } else if (comp$required == "not_expected" && !found) {
      "Filled in in the RAPS portal, so it need not be in the package."
    } else if (comp$required == "recommended" && !found) {
      comp$description
    } else if (!found || length(matched) == 0) {
      if (in_readme) "Described in the README." else ""
    } else ""
    list(row = data.frame(component = comp$id, group = comp$group, title = comp$title,
                          found = if (comp$required == "not_expected" && !found) "not expected"
                                  else if (found) "found" else "not found",
                          files = if (length(matched)) .tdc_list(matched, 3) else "",
                          n_files = length(matched), status = status, note = note),
         matched = matched)
  })
  tbl <- do.call(rbind, lapply(rows, `[[`, "row"))
  paths <- stats::setNames(lapply(rows, `[[`, "matched"), tbl$component)

  # Raw data: unmarked while other data exists is manual; no data at all fails.
  raw <- tbl$component == "raw_data"
  any_data <- any(files$data_type %in% "data") ||
    any(tbl$n_files[tbl$component %in% c("processed_data", "analysed_data")] > 0)
  if (any(raw) && tbl$found[raw] == "not found") {
    if (any_data) {
      tbl$status[raw] <- .tdc_sev(rules, "raw-data-unclear")
      tbl$note[raw] <- "No folder or file is marked as raw data, but the package holds other data. Is the raw data included under another name?"
    } else {
      tbl$status[raw] <- .tdc_sev(rules, "no-data")
      tbl$note[raw] <- "The package holds no data at all. RAPS archives the data behind a paper."
    }
  }

  counted <- tbl$status[!tbl$status %in% c("not found", "na")]
  status <- .tdc_worst(counted)
  n_found <- sum(tbl$found == "found")
  n_expected <- sum(tbl$found != "not expected")
  problems <- tbl$status %in% c("fail", "warn", "manual")
  found <- sprintf("%d of the %d components TU/e lists were found.", n_found, n_expected)
  if (any(problems)) found <- paste(found, sprintf("To look at: %s.", paste(tbl$title[problems], collapse = "; ")))
  todo <- if (any(problems)) paste(tbl$note[problems & nzchar(tbl$note)], collapse = " ")
  list(item = .tdc_item("components", "1.1", "Package components", status, found, todo,
                        note = if (!any(problems)) "Not every component applies to every study."),
       table = tbl, paths = paths)
}

# Folder depth: TU/e recommends at most `max_depth` folder levels; a file at a
# deeper level, or an empty folder that deep, is too deep. Junk folders do not
# count (they are not in `files` or `folders`), nor do files inside zips.
.tdc_check_depth <- function(files, folders, rules) {
  lim <- rules$limits
  max_depth <- lim$max_depth
  depth <- .tdc_depth(files$path)
  deep <- depth > max_depth
  # empty folders as deep as a file at the next level
  folder_depth <- lengths(strsplit(folders, "/", fixed = TRUE))
  has_file <- vapply(folders, function(f) any(startsWith(files$path, paste0(f, "/"))), logical(1))
  deep_empty <- folders[!has_file & folder_depth > max_depth]
  n_deep <- sum(deep)
  if (n_deep == 0 && length(deep_empty) == 0) {
    return(.tdc_item("folder_depth", "1.1", "Folder depth (up to 3 levels)", "pass",
                     sprintf("No file lies more than %d folder levels deep (deepest: %d).",
                             max_depth, if (length(depth)) max(depth) else 0)))
  }
  dirs <- dirname(files$path[deep])
  counts <- sort(table(dirs), decreasing = TRUE)
  counts <- counts[order(names(counts))]
  listed <- utils::head(names(counts), lim$max_folders_listed)
  more <- length(counts) - length(listed)
  folder_txt <- paste0("`", listed, "/` (", format(as.integer(counts[listed]), big.mark = ",", trim = TRUE),
                       " file", ifelse(counts[listed] == 1, "", "s"), ")")
  found <- c(
    if (n_deep) sprintf("%s file%s lie%s %s folder levels deep.",
                        format(n_deep, big.mark = ","), if (n_deep == 1) "" else "s",
                        if (n_deep == 1) "s" else "",
                        paste(sort(unique(depth[deep])), collapse = " or ")),
    if (n_deep) sprintf("The folders with these files: %s%s.", paste(folder_txt, collapse = ", "),
                        if (more > 0) sprintf(", … and %d more folder%s", more, if (more == 1) "" else "s") else ""),
    if (length(deep_empty)) sprintf("Empty folders that deep: %s.", .tdc_list(paste0("`", deep_empty, "/`"), lim$max_folders_listed))
  )
  .tdc_item("folder_depth", "1.1", "Folder depth (up to 3 levels)",
            .tdc_sev(rules, "too-deep"), found,
            sprintf("Bring these %s to at most %d folder levels deep, as TU/e recommends.",
                    if (n_deep && length(deep_empty)) "files and folders" else if (n_deep) "files" else "folders",
                    max_depth))
}

# The names that make a top folder a component folder (layout C): each may be
# plural, or have "data", "files" or "code" added ("raw_data", "code_files").
.tdc_component_folder_words <- c(
  "data", "dataset", "raw", "processed", "analy[sz]ed", "original", "cleaned", "derived", "final",
  "code", "script", "software", "src", "model", "notebook", "analysis", "analyses",
  "doc", "documentation", "document", "method", "protocol", "material", "manuscript", "paper", "supplementary",
  "admin", "administrative", "ethics", "consent", "agreement",
  "output", "result", "figure", "fig", "plot", "table"
)

# What to do when the package is not organised by component.
.tdc_layout_todo <- "Organise the package by component, as in TU/e's examples: numbered folders (1 Scientific documents, 2 Administrative documents, 3 Code & Script, 4 Data with Raw data and Processed data), one folder per figure (Fig.1, Fig.2, …), or folders such as data/raw, data/processed, code, docs and admin."

# Component-level organisation: do the top folders follow one of TU/e's
# example layouts (numbered component folders; one folder per figure) or
# folders by component? And are data or code files lying loose at the top?
.tdc_check_layout <- function(files, folders, rules) {
  lim <- rules$limits
  top_folders <- folders[!grepl("/", folders)]
  top_files <- files[!grepl("/", files$path), , drop = FALSE]
  loose <- top_files$path[top_files$data_type %in% c("data", "code")]

  if (length(top_folders) == 0) {
    ok <- nrow(files) <= lim$flat_max_files
    return(.tdc_item(
      "folder_layout", "1.1", "Component-level organisation",
      if (ok) "pass" else .tdc_sev(rules, "layout-none"),
      if (ok) sprintf("The package has no folders and only %d files, so it needs none.", nrow(files))
      else sprintf("All %s files lie in the top folder.", format(nrow(files), big.mark = ",")),
      if (!ok) .tdc_layout_todo))
  }

  norm <- .tdc_norm_name(top_folders)
  fits <- lapply(rules$layouts, function(layout) {
    part_of <- vapply(norm, function(f) {
      for (part in layout$parts) if (any(.tdc_match_any(f, part$match))) return(part$id)
      NA_character_
    }, character(1))
    required <- vapply(layout$parts, function(p) isTRUE(p$required), logical(1))
    list(title = layout$title, coverage = mean(!is.na(part_of)),
         required_ok = all(vapply(layout$parts[required], function(p) p$id %in% part_of, logical(1))),
         unmatched = top_folders[is.na(part_of)])
  })
  word_re <- sprintf("^(?:%s)s?(?: (?:data|files?|code))?$", paste(.tdc_component_folder_words, collapse = "|"))
  generic <- grepl(word_re, norm, perl = TRUE)
  fits[[length(fits) + 1]] <- list(title = "folders by component", coverage = mean(generic),
                                   required_ok = TRUE, unmatched = top_folders[!generic])
  ok <- vapply(fits, function(f) f$required_ok && f$coverage >= lim$min_layout_coverage, logical(1))

  if (any(ok)) {
    best <- fits[ok][[which.max(vapply(fits[ok], function(f) f$coverage, numeric(1)))]]
    status <- "pass"
    found <- sprintf("The top folders follow %s: %s.", sub("^TU/e package: ", "TU/e's example layout, ", best$title),
                     paste0("`", top_folders, "`", collapse = ", "))
    if (length(best$unmatched)) found <- paste(found, sprintf("Not part of it: %s.", paste0("`", best$unmatched, "`", collapse = ", ")))
    todo <- NULL
  } else {
    status <- .tdc_sev(rules, "layout-none")
    not_comp <- top_folders[!generic]
    todo <- .tdc_layout_todo
    found <- sprintf("The top folder%s %s %s not %s component folder%s.",
                      if (length(not_comp) == 1) "" else "s",
                      paste0("`", not_comp, "`", collapse = ", "),
                      if (length(not_comp) == 1) "is" else "are",
                      if (length(not_comp) == 1) "a" else "",
                      if (length(not_comp) == 1) "" else "s")
  }
  if (length(loose)) {
    status <- .tdc_worst(c(status, .tdc_sev(rules, "loose-files-top")))
    found <- paste(found, sprintf("Data or code files lie loose at the top: %s.",
                                  .tdc_list(paste0("`", loose, "`"), 5)))
    todo <- c(todo, "Move the loose data and code files into the component folders.")
  }
  .tdc_item("folder_layout", "1.1", "Component-level organisation", status, found, todo)
}

# File classification: every file as data, code, documentation (readme,
# codebook, supplemental, licence), materials, output or unknown, as
# metacheck's classifier (through repo_check) gives it.
.tdc_check_classification <- function(files, rules) {
  type <- ifelse(is.na(files$data_type) | !nzchar(files$data_type), "unknown", files$data_type)
  levels <- c("data", "code", "documentation", "materials", "output", "unknown")
  counts <- table(factor(type, levels))
  counts <- counts[counts > 0]
  fmt <- function(n) format(as.integer(n), big.mark = ",", trim = TRUE)
  parts <- vapply(names(counts), function(t) {
    txt <- sprintf("%s: %s", t, fmt(counts[[t]]))
    if (t == "documentation") {
      role <- files$doc_role[type == "documentation"]
      role[is.na(role)] <- "supplemental"
      role[role == "license"] <- "licence"
      r <- table(role)
      txt <- sprintf("%s (%s)", txt, paste(sprintf("%s %s", names(r), fmt(r)), collapse = ", "))
    }
    if (t == "unknown") {
      e <- table(ifelse(nzchar(files$ext[type == "unknown"]), paste0(".", files$ext[type == "unknown"]), "no extension"))
      txt <- sprintf("%s (%s)", txt, paste(sprintf("%s %s", fmt(e), names(e)), collapse = ", "))
    }
    txt
  }, character(1))
  n_unknown <- sum(type == "unknown")
  found <- paste0(paste(parts, collapse = "; "), ".")
  if (n_unknown) {
    found <- paste(found, sprintf("%s file%s could not be classified.",
                                  fmt(n_unknown), if (n_unknown == 1) "" else "s"))
  }
  .tdc_item("file_classification", "1.1", "File classification",
            if (n_unknown) .tdc_sev(rules, "files-unclassified") else "pass", found,
            if (n_unknown) sprintf("Describe %s in the README.", if (n_unknown == 1) "this file" else "these files"))
}

# Package size: over 2 TB, consult a RAPS data manager first.
.tdc_check_size <- function(files, rules) {
  total <- sum(as.numeric(files$size), na.rm = TRUE)
  over <- total > rules$limits$max_package_bytes
  size_txt <- format(structure(total, class = "object_size"), units = "auto", standard = "SI")
  .tdc_item("package_size", "1.1", "Package size",
            if (over) .tdc_sev(rules, "package-too-large") else "pass",
            sprintf("The package holds %s.", size_txt),
            if (over) "Consult a RAPS data manager (rdi-lab@tue.nl) before archiving: the package is over 2 TB.")
}
