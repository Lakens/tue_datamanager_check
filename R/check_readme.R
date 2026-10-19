# Section 1.2 of the RDLC document: the README (SPECIFICATION.md, section 1.2).
#
# - README existence: a README at the top of the package, as TXT or MD.
# - README content against the TU/e README template (Zenodo
#   10.5281/zenodo.19692008): judged per template line by the LLM (a local
#   Ollama model only), else by matching headings and labels; plus rule-based
#   checks for template help text, contacts, the file inventory, ORCIDs and
#   CRediT roles.
# - A revised README in the template's structure, written by the LLM.

.tdc_email_re <- "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"
.tdc_orcid_re <- "(?:https?://orcid\\.org/)?[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9Xx]"
.tdc_doi_re <- "10\\.[0-9]{4,9}/[^[:space:]\"<>]+"
.tdc_url_re <- "https?://[^[:space:]\"<>)\\]]+"

# -- the LLM: a local Ollama model only ---------------------------------------

# TRUE when metacheck's model is a local Ollama model: "ollama/<model>"
# (metacheck's Ollama route, at OLLAMA_BASE_URL), or "vllm/<model>" pointed at
# OLLAMA_BASE_URL, as the SURF server sets it.
.tdc_local_llm <- function() {
  model <- metacheck::llm_model()
  if (is.null(model) || !nzchar(model)) return(FALSE)
  if (grepl("^ollama", model)) return(TRUE)
  if (startsWith(model, "vllm/")) {
    base <- getOption("metacheck.llm.vllm.base_url", "")
    ollama <- Sys.getenv("OLLAMA_BASE_URL")
    return(nzchar(ollama) && startsWith(base, ollama))
  }
  FALSE
}

# One LLM call, with metacheck's LLM switched on for this call only, and only
# on a local model. Returns NULL (with the reason as attribute "why") when it
# cannot or does not answer.
.tdc_llm <- function(text, prompt, type = NULL, params = list()) {
  if (!.tdc_local_llm()) {
    return(structure(list(), why = sprintf("the LLM is not a local Ollama model (it is set to '%s'); set llm_model('ollama/qwen3.5:4b')", metacheck::llm_model())))
  }
  before <- metacheck::llm_use()
  metacheck::llm_use(TRUE)
  on.exit(metacheck::llm_use(before), add = TRUE)
  ans <- tryCatch(metacheck::llm(text, prompt, type = type, params = params), error = function(e) e)
  if (inherits(ans, "error")) return(structure(list(), why = conditionMessage(ans)))
  ans
}

# -- README existence -------------------------------------------------------

# The README: at the top of the package, as TXT or MD (or without an
# extension), else any README. NULL when there is none.
.tdc_find_readme <- function(files) {
  is_readme <- files$doc_role %in% "readme"
  if (!any(is_readme)) return(NULL)
  cand <- files[is_readme, , drop = FALSE]
  cand$at_top <- !grepl("/", cand$path)
  cand$text_format <- cand$ext %in% c("txt", "md", "markdown", "")
  cand <- cand[order(!(cand$at_top & cand$text_format), !cand$at_top), , drop = FALSE]
  list(path = cand$path[[1]], location = cand$location[[1]],
       ok = cand$at_top[[1]] && cand$text_format[[1]],
       at_top = cand$at_top[[1]], text_format = cand$text_format[[1]])
}

.tdc_check_readme_exists <- function(readme, rules) {
  title <- "README at the top (TXT or MD)"
  if (is.null(readme)) {
    return(.tdc_item("readme_exists", "1.2", title, .tdc_sev(rules, "readme-missing"),
                     "The package has no README.", "Add a README.txt or README.md at the top.",
                     "It is the first file others open to understand the package."))
  }
  if (readme$ok) {
    return(.tdc_item("readme_exists", "1.2", title, "pass",
                     sprintf("`%s` is at the top of the package.", readme$path)))
  }
  problems <- c(if (!readme$at_top) "not at the top of the package",
                if (!readme$text_format) "not plain text or Markdown")
  .tdc_item("readme_exists", "1.2", title, .tdc_sev(rules, "readme-not-top-or-text"),
            sprintf("The only README (`%s`) is %s.", readme$path, paste(problems, collapse = " and ")),
            "Put a README.txt or README.md at the top.")
}

# -- the TU/e template --------------------------------------------------------

# The template's lines: section, id, label, required.
.tdc_template_lines <- function(rules) {
  do.call(rbind, lapply(rules$readme$sections, function(s) {
    data.frame(section = s$id, section_title = s$title, section_hint = s$hint,
               id = vapply(s$fields, function(f) f$id, character(1)),
               label = vapply(s$fields, function(f) f$label, character(1)),
               required = vapply(s$fields, function(f) isTRUE(f$required), logical(1)))
  }))
}

# The template lines judged by the LLM: data.frame(id, status, evidence) with
# status "filled", "empty" or "missing"; an empty list (with attribute "why")
# without an answer.
.tdc_template_ai <- function(text, lines, template) {
  prompt <- paste(
    "You check the README of a research data package against the TU/e README template.",
    "For EVERY template line in the list, decide whether the README gives that information,",
    "under any heading or wording, in any language:",
    "'filled' = the README gives real information for it;",
    "'empty' = the README has the line or heading, but nothing real in it (left empty, or only",
    "template help text such as <...> or [NAME]);",
    "'missing' = the README does not give this information at all.",
    "For 'filled', copy a short quote (at most 15 words) from the README as evidence; otherwise leave evidence empty.",
    "Judge only from the README. Answer for every id in the list, using the ids exactly as given.")
  listing <- paste(sprintf("%s | %s | %s", lines$id, lines$section_hint, lines$label), collapse = "\n")
  input <- paste0("THE TU/e README TEMPLATE (what each line asks for):\n", template,
                  "\n\nTEMPLATE LINES TO JUDGE (id | section | line):\n", listing,
                  "\n\nREADME:\n", substr(text, 1, 40000))
  tp <- ellmer::type_object(lines = ellmer::type_array(ellmer::type_object(
    id = ellmer::type_string("the template line id, exactly as given"),
    status = ellmer::type_enum(c("filled", "empty", "missing"), "whether the README gives this information"),
    evidence = ellmer::type_string("a short quote from the README for 'filled', else empty"))))
  ans <- .tdc_llm(input, prompt, tp)
  if (!length(ans) || !"lines.id" %in% names(ans)) return(structure(list(), why = attr(ans, "why") %||% "the LLM gave no answer"))
  data.frame(id = as.character(ans$lines.id), status = as.character(ans$lines.status),
             evidence = as.character(ans$lines.evidence))
}

# For each data folder at the top: does the README have data-specific
# information (section 5: variable list, missing data codes) for it?
.tdc_section5_ai <- function(text, data_folders) {
  if (!length(data_folders)) return(data.frame(folder = character(0), has_section = logical(0)))
  prompt <- paste(
    "You check the README of a research data package. The TU/e README template asks for a section",
    "'DATA-SPECIFIC INFORMATION FOR: [FILENAME]' (a variable list with units, and missing data codes)",
    "for each dataset, folder or file. For EVERY folder in the list, say whether the README gives such",
    "data-specific information for that folder or for the data files in it. Judge only from the README.")
  input <- paste0("DATA FOLDERS:\n", paste(data_folders, collapse = "\n"),
                  "\n\nREADME:\n", substr(text, 1, 40000))
  tp <- ellmer::type_object(datasets = ellmer::type_array(ellmer::type_object(
    folder = ellmer::type_string("the folder, exactly as given"),
    has_section = ellmer::type_boolean("whether the README has data-specific information for it"))))
  ans <- .tdc_llm(input, prompt, tp)
  if (!length(ans) || !"datasets.folder" %in% names(ans)) return(NULL)
  data.frame(folder = as.character(ans$datasets.folder), has_section = as.logical(ans$datasets.has_section))
}

# The template lines judged by matching headings and labels (without a local
# LLM): data.frame(id, status, evidence).
.tdc_template_rules <- function(text, rules) {
  lines_txt <- strsplit(text, "\r?\n")[[1]]
  out <- lapply(rules$readme$sections, function(s) {
    lapply(s$fields, function(f) {
      hit <- which(.tdc_match_any(lines_txt, f$match))
      status <- if (!length(hit)) "missing" else {
        line <- lines_txt[hit[[1]]]
        after <- trimws(sub("^[^:]*:", "", line))
        after <- trimws(gsub("<[^>]*>|\\[[^]]*\\]", "", after))
        nxt <- lines_txt[seq(hit[[1]] + 1, length.out = min(2, length(lines_txt) - hit[[1]]))]
        nxt <- trimws(gsub("<[^>]*>|\\[[^]]*\\]", "", nxt))
        filled <- (grepl(":", line, fixed = TRUE) && nchar(after) >= 2) ||
          any(nchar(nxt) >= 2 & !grepl(":\\s*$", nxt))
        if (filled) "filled" else "empty"
      }
      data.frame(id = f$id, status = status,
                 evidence = if (length(hit)) trimws(lines_txt[hit[[1]]]) else "")
    })
  })
  do.call(rbind, unlist(out, recursive = FALSE))
}

# -- all README checks --------------------------------------------------------

# `files` and `folders` as in the module; `pkg_info` is a text describing the
# package for the revised README; `revised_file` is where to write it.
# Returns list(items, sections, found_sections, revised).
.tdc_check_readme <- function(readme, files, folders, rules, pkg_info, revised_file) {
  items <- list(.tdc_check_readme_exists(readme, rules))
  none <- list(items = do.call(rbind, items), sections = NULL,
               found_sections = character(0), revised = NULL)
  if (is.null(readme)) return(none)
  text <- .tdc_read_text(readme$location)
  if (is.null(text) || !nzchar(trimws(text))) {
    items[[2]] <- .tdc_item("readme_template", "1.2", "README follows the TU/e template", "manual",
                            "The README could not be read here (it is not a local text file), so its content was not checked.",
                            "Check the README against the TU/e template by hand.")
    none$items <- do.call(rbind, items)
    return(none)
  }
  lines <- .tdc_template_lines(rules)
  lines_txt <- strsplit(text, "\r?\n")[[1]]

  # the template, judged per line: the LLM, else headings and labels
  template <- paste(readLines(file.path(rules$dir, "data", "README_Template_TUe.txt"),
                              warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  ai <- .tdc_template_ai(text, lines, template)
  method <- if (is.data.frame(ai)) sprintf("Judged by the LLM (%s).", metacheck::llm_model()) else
    sprintf("Judged by matching headings and labels, because %s.", attr(ai, "why") %||% "the LLM did not answer")
  judged <- .tdc_template_rules(text, rules)
  if (is.data.frame(ai)) {
    ai <- ai[ai$id %in% lines$id & ai$status %in% c("filled", "empty", "missing"), , drop = FALSE]
    ai <- ai[!duplicated(ai$id), , drop = FALSE]
    use_ai <- judged$id %in% ai$id
    label_absent <- judged$status == "missing"
    judged[use_ai, c("status", "evidence")] <- ai[match(judged$id[use_ai], ai$id), c("status", "evidence")]
    # a line can only be left empty when it is there: when its label is not
    # in the README, the model's "empty" is "missing"
    judged$status[label_absent & judged$status == "empty"] <- "missing"
    # evidence is a quote for a filled line only
    judged$evidence[judged$status != "filled"] <- ""
  }
  # The summary is also filled when the README's first paragraph after its
  # title is a description of at least 20 words (a 4B model misses an
  # unlabelled first paragraph).
  paras <- trimws(strsplit(gsub("(?s)```.*?```", "", text, perl = TRUE), "\r?\n\\s*\r?\n")[[1]])
  paras <- paras[nzchar(paras) & !grepl("^#", paras)]
  first_para <- if (length(paras)) gsub("\\s+", " ", paras[[1]]) else ""
  is_summary <- judged$id == "summary"
  if (any(is_summary) && judged$status[is_summary] != "filled" &&
      lengths(strsplit(first_para, " "))[[1]] >= 20) {
    judged$status[is_summary] <- "filled"
    judged$evidence[is_summary] <- substr(first_para, 1, 100)
  }
  lines <- merge(lines, judged, by = "id", sort = FALSE)
  lines <- lines[order(match(lines$section, vapply(rules$readme$sections, function(s) s$id, character(1)))), ]
  sections <- do.call(rbind, lapply(split(lines, factor(lines$section, unique(lines$section))), function(s) {
    data.frame(section = s$section_hint[[1]], id = s$section[[1]],
               found = any(s$status != "missing"),
               missing = paste(s$label[s$required & s$status == "missing"], collapse = "; "),
               empty = paste(s$label[s$required & s$status == "empty"], collapse = "; "))
  }))
  data_folders <- folders[!grepl("/", folders) &
                            vapply(folders, function(f) any(startsWith(files$path[files$data_type %in% "data"], paste0(f, "/"))), logical(1))]
  s5 <- if (is.data.frame(ai)) .tdc_section5_ai(text, data_folders) else NULL
  no_s5 <- if (!is.null(s5)) s5$folder[!s5$has_section & s5$folder %in% data_folders] else character(0)
  n_missing <- sum(lines$required & lines$status == "missing")
  n_empty <- sum(lines$required & lines$status == "empty")
  t_problem <- n_missing > 0 || n_empty > 0 || length(no_s5) > 0
  items[[length(items) + 1]] <- .tdc_item(
    "readme_template", "1.2", "README follows the TU/e template",
    if (t_problem) .tdc_sev(rules, "readme-template") else "pass",
    found = c(sprintf("The README has %d of the template's %d sections; %d of its %d required lines are filled in, %d are missing and %d are left empty.",
                      sum(sections$found), nrow(sections), sum(lines$required & lines$status == "filled"),
                      sum(lines$required), n_missing, n_empty),
              if (length(no_s5)) sprintf("No data-specific information (section 5) for: %s.", paste0("`", no_s5, "`", collapse = ", "))),
    todo = c(if (n_missing + n_empty > 0) sprintf("Fill in the %s lines (see the table).",
                                                    if (n_missing && n_empty) "missing and empty" else if (n_missing) "missing" else "empty"),
             if (length(no_s5)) "Add a section 5 (data-specific information) for each data folder."),
    note = c(if (t_problem) "The template (10.5281/zenodo.19692008) is a recommendation; not all fields may apply.", method))

  # template help text left in
  # Markdown code (``` blocks and `inline` code) holds commands and paths
  # such as models/<hash>/, not template help text, so it is not searched
  prose <- gsub("(?s)```.*?```", " ", text, perl = TRUE)
  prose <- gsub("`[^`\n]*`", " ", prose, perl = TRUE)
  ph <- unique(unlist(lapply(rules$readme$placeholders, function(p) {
    regmatches(prose, gregexpr(p, prose, perl = TRUE, ignore.case = TRUE))[[1]]
  })))
  items[[length(items) + 1]] <- .tdc_item(
    "readme_template_text", "1.2", "No template help text left in the README",
    if (length(ph)) .tdc_sev(rules, "readme-template-text") else "pass",
    if (length(ph)) sprintf("The README still has template help text: %s.", .tdc_list(paste0("`", substr(ph, 1, 60), "`"), 5))
    else "No template help text (<…>, [NAME], [YYYY-MM-DD], TODO, lorem ipsum) found.",
    if (length(ph)) "Replace it with the real information, or remove the line.")

  # contacts: at least two e-mail addresses on or under the Contacts line
  at <- which(grepl("^\\s*(?:[-*+•#]+\\s*)?contacts?\\b", lines_txt, ignore.case = TRUE, perl = TRUE))
  block <- if (length(at)) {
    i <- at[[1]]
    nxt <- lines_txt[seq(i + 1, length.out = min(5, length(lines_txt) - i))]
    stop_at <- which(grepl("^\\s*(?:[-*+•#]+\\s*)?[A-Za-z][A-Za-z /()-]{2,40}:", nxt, perl = TRUE) & !grepl(.tdc_email_re, nxt))
    c(lines_txt[[i]], if (length(stop_at)) nxt[seq_len(stop_at[[1]] - 1)] else nxt)
  } else character(0)
  contacts <- unique(tolower(unlist(regmatches(block, gregexpr(.tdc_email_re, block, perl = TRUE)))))
  min_c <- rules$limits$min_contacts
  items[[length(items) + 1]] <- .tdc_item(
    "readme_contacts", "1.2", "At least two contacts",
    if (length(contacts) >= min_c) "pass" else .tdc_sev(rules, "readme-contacts"),
    if (!length(at)) "The README has no Contacts line."
    else if (length(contacts) >= min_c) sprintf("Contacts: %s.", paste(contacts, collapse = ", "))
    else sprintf("The Contacts line gives %d e-mail address%s.", length(contacts), if (length(contacts) == 1) "" else "es"),
    if (length(contacts) < min_c) "Add at least two contacts for the data package, with their e-mail addresses, as the TU/e template asks.")

  # file inventory: the README names the top folders and files
  top <- c(folders[!grepl("/", folders)], files$path[!grepl("/", files$path)])
  top <- top[!grepl("^(read[ _.-]?me|licen[cs]e|copying)(\\.[a-z]+)?$", top, ignore.case = TRUE)]
  low <- tolower(text)
  named <- vapply(top, function(x) {
    grepl(tolower(x), low, fixed = TRUE) ||
      grepl(.tdc_norm_name(tools::file_path_sans_ext(x)), .tdc_norm_name(low), fixed = TRUE)
  }, logical(1))
  share <- if (length(top)) mean(named) else 1
  items[[length(items) + 1]] <- .tdc_item(
    "readme_file_list", "1.2", "The README names the files and folders",
    if (share >= rules$limits$min_file_list_coverage) "pass" else .tdc_sev(rules, "readme-file-list"),
    sprintf("The README names %d of the %d top-level folders and files (%.0f%%)%s", sum(named), length(top), 100 * share,
            if (any(!named)) sprintf("; not named: %s.", .tdc_list(paste0("`", top[!named], "`"), 8)) else "."),
    if (share < rules$limits$min_file_list_coverage) "Describe the folders and files that are not named in the README.")

  # ORCIDs: form and check digit (metacheck's check_orcid(), no lookup online)
  orcids <- unique(regmatches(text, gregexpr(.tdc_orcid_re, text, perl = TRUE))[[1]])
  bad_orcid <- orcids[!vapply(orcids, function(o) !isFALSE(suppressWarnings(metacheck::check_orcid(o))), logical(1))]
  items[[length(items) + 1]] <- .tdc_item(
    "readme_orcid", "1.2", "ORCIDs are valid",
    if (length(bad_orcid)) .tdc_sev(rules, "readme-orcid") else if (length(orcids)) "pass" else "na",
    if (length(bad_orcid)) sprintf("Not a valid ORCID (wrong form or check digit): %s.", paste(bad_orcid, collapse = ", "))
    else if (length(orcids)) sprintf("%d ORCID%s, all valid.", length(orcids), if (length(orcids) == 1) "" else "s")
    else "The README gives no ORCID.",
    if (length(bad_orcid)) sprintf("Correct %s.", if (length(bad_orcid) == 1) "this ORCID" else "these ORCIDs"))

  # CRediT roles: only the 14 official roles
  role_lines <- lines_txt[grepl("credit\\s+roles?", lines_txt, ignore.case = TRUE)]
  given <- trimws(unlist(strsplit(sub("^[^:]*:", "", role_lines[grepl(":", role_lines)]), "[;,/]|\\band\\b")))
  given <- given[nzchar(given) & !grepl("^<.*>$|^\\[.*\\]$", given)]
  norm_role <- function(x) gsub("[^a-z]", "", gsub("&", "and", tolower(x)))
  official <- metacheck::credit_roles("names")
  unknown <- given[!norm_role(given) %in% norm_role(official)]
  items[[length(items) + 1]] <- .tdc_item(
    "readme_credit", "1.2", "CRediT roles are official roles",
    if (length(unknown)) .tdc_sev(rules, "readme-credit") else if (length(given)) "pass" else "na",
    if (length(unknown)) sprintf("Not a CRediT role: %s.", paste(unique(unknown), collapse = "; "))
    else if (length(given)) "All roles are CRediT roles."
    else "The README names no CRediT roles.",
    if (length(unknown)) "Use the 14 roles of the CRediT taxonomy (https://credit.niso.org), such as Data curation or Formal analysis.")

  revised <- .tdc_revise_readme(text, rules, pkg_info, revised_file)
  list(items = do.call(rbind, items), sections = sections, lines = lines,
       found_sections = sections$id[sections$found], revised = revised)
}

# -- the revised README ------------------------------------------------------

# The README rewritten into the TU/e template by the LLM, in English, keeping
# every fact and inventing nothing ([TO ADD: ...] where information is
# missing). Afterwards the template's help text is removed, a line left empty
# gets [TO ADD], and every e-mail address, ORCID, DOI and web address in it
# must also be in the README or the package description; anything else is
# listed as added by the AI. Written to `revised_file`.
.tdc_revise_readme <- function(text, rules, pkg_info, revised_file) {
  template <- paste(readLines(file.path(rules$dir, "data", "README_Template_TUe.txt"),
                              warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  prompt <- paste(
    "You rewrite the README of a research data package so it follows the TU/e README template exactly:",
    "its five numbered sections with all their lines, in English.",
    "Use only information in the README and in the package description you are given.",
    "Keep every fact of the original README. Copy its sentences as they are: do not complete, shorten or",
    "change them, even when a sentence looks unfinished. Do not invent anything: no names, e-mail addresses,",
    "ORCIDs, DOIs, web addresses, licences, dates, methods or file descriptions that are not given.",
    "The project title is the README's first heading. For the file list, describe the folders and what",
    "they hold (as in the package description), not every single file.",
    "Where the template asks for information that is not given, write [TO ADD: what is needed];",
    "never leave a line empty. Replace the template's help text in <...>; do not keep it.",
    "Answer with the revised README only, as plain text.")
  input <- paste0("THE TU/e README TEMPLATE:\n", template,
                  "\n\nTHE PACKAGE (folders, files and formats, from the package itself):\n", pkg_info,
                  "\n\nTHE RESEARCHER'S README:\n", substr(text, 1, 30000))
  ans <- .tdc_llm(input, prompt, params = list(temperature = 0))
  if (!length(ans) || is.null(ans$answer) || !nzchar(ans$answer[[1]] %||% "")) {
    return(list(text = NULL, added = character(0), file = NULL,
                note = sprintf("No revised README was written: %s.", attr(ans, "why") %||% "the LLM gave no answer")))
  }
  revised <- as.character(ans$answer[[1]])
  # The template's help text in <...> is to be "deleted before finalizing";
  # the model sometimes keeps it, so it is removed here (the template's own
  # pattern, which leaves HTML tags, web and e-mail addresses alone).
  revised <- gsub(rules$readme$placeholders[[1]], "", revised, perl = TRUE)
  revised <- gsub("[ \t]+(\r?\n)", "\\1", revised)
  revised <- gsub("(\r?\n){3,}", "\n\n", revised)
  # A template line with nothing after it (before the next line, heading or
  # the end) gets [TO ADD]. A label is text followed by a colon; a line that
  # describes a folder (a path ending in "/:", as in "code/: 3 files") is not.
  rl <- strsplit(revised, "\r?\n")[[1]]
  label_re <- "^\\s*(-\\s*)?(?![^:\\s]+/:)[A-Za-z][^:]{2,90}:"
  is_empty_label <- grepl(paste0(label_re, "\\s*$"), rl, perl = TRUE)
  next_txt <- vapply(seq_along(rl), function(i) {
    after <- rl[seq(i + 1, length.out = max(0, length(rl) - i))]
    after <- after[nzchar(trimws(after))]
    if (length(after)) after[[1]] else ""
  }, character(1))
  nothing_after <- !nzchar(next_txt) | grepl(label_re, next_txt, perl = TRUE) |
    grepl("^\\s*[0-9]\\.\\s+[A-Z]", next_txt)
  rl[is_empty_label & nothing_after] <- paste(sub("\\s+$", "", rl[is_empty_label & nothing_after]), "[TO ADD]")
  revised <- paste(rl, collapse = "\n")

  allowed <- tolower(paste(text, pkg_info))
  found <- unique(c(regmatches(revised, gregexpr(.tdc_email_re, revised, perl = TRUE))[[1]],
                    regmatches(revised, gregexpr(.tdc_orcid_re, revised, perl = TRUE))[[1]],
                    regmatches(revised, gregexpr(.tdc_doi_re, revised, perl = TRUE))[[1]],
                    regmatches(revised, gregexpr(.tdc_url_re, revised, perl = TRUE))[[1]]))
  found <- sub("[.,;:]+$", "", found)
  added <- found[!vapply(tolower(found), function(f) grepl(f, allowed, fixed = TRUE), logical(1))]
  written <- tryCatch({
    dir.create(dirname(revised_file), showWarnings = FALSE, recursive = TRUE)
    writeLines(revised, revised_file, useBytes = TRUE)
    revised_file
  }, error = function(e) NULL)
  list(text = revised, added = added, file = written, note = NULL)
}
