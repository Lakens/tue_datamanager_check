# Section 1.3 of the RDLC document: research with human participants or
# personal data (SPECIFICATION.md, section 1.3).
#
# - Whether people took part or personal data was used: yes, no or unclear.
#   The sources, in order: the argument `human_participants`; personal data or
#   demographic columns found by data_check (always yes); data collection from
#   people found by ethics_check in the paper, the README or the package's
#   scientific documents; the LLM (a local Ollama model only), reading those
#   texts and the file and folder names. Up to 3 scientific documents (the
#   manuscript, documents in a folder of scientific documents, the
#   methodology, PDFs at the top) are read, only when the paper given has no
#   text and nothing before has answered yes: a PDF converted with TU/e's
#   GROBID server, else with the SURF API.
# - The ERB approval letter and the signed informed consent forms: found by
#   name and folder among the documentation files (the components table), as
#   PDF; whether they must be there follows from the answer.

# Words in a text, file or folder name that suggest people took part.
.tdc_people_re <- paste0(
  "(?<![a-z])(?:participants?|subjects?|respondents?|volunteers?|patients?|",
  "students?|pupils?|employees?|people|persons?|humans?|interviews?|interviewees?|",
  "survey(?:s|ed)?|questionnaires?|focus[ _-]?groups?|consent|ethic(?:s|al)?|",
  "recruit(?:ed|ment)?|demographics?|personal[ _-]data|pseudonymi[sz]ed|anonymi[sz]ed)(?![a-z])")

# The paragraphs of a paper object, in order, as one text each.
.tdc_paragraphs <- function(paper) {
  txt <- paper$text
  if (is.null(txt) || !nrow(txt)) return(character(0))
  key <- paste(txt$section_id, txt$paragraph_id)
  paras <- vapply(split(txt$text, factor(key, unique(key))),
                  function(s) paste(s[!is.na(s)], collapse = " "), character(1))
  unname(paras[nzchar(trimws(paras))])
}

# A paper's text for the LLM: its abstract, then the paragraphs that mention
# people (participants, survey, consent, ...), in order, up to `max_chars`.
# The local model's context (16,384 tokens) holds no full paper.
.tdc_people_excerpt <- function(paper, max_chars = 20000) {
  paras <- .tdc_paragraphs(paper)
  abstract <- paper$info$abstract %||% NA_character_
  keep <- paras[grepl(.tdc_people_re, paras, ignore.case = TRUE, perl = TRUE)]
  out <- c(if (!is.na(abstract) && nzchar(abstract)) paste("Abstract:", abstract), keep)
  out <- out[cumsum(nchar(out) + 2) <= max_chars]
  paste(out, collapse = "\n\n")
}

# TRUE when a paper object holds a real text (not test_paper()'s letters).
.tdc_has_text <- function(paper) {
  !is.null(paper$text) && nrow(paper$text) > 0 &&
    !identical(paper$info$input_format, "test")
}

# ethics_check on a paper object: its statements of data collection from
# people (`live`) and of ethics approval (`ethics`).
.tdc_ethics_statements <- function(paper) {
  none <- list(live = character(0), ethics = character(0))
  ec <- tryCatch(metacheck::module_run(paper, "ethics_check"), error = function(e) NULL)
  st <- ec$summary_table
  if (is.null(st) || !nrow(st)) return(none)
  clean <- function(x) { x <- unlist(x); unique(x[!is.na(x) & nzchar(x)]) }
  list(live = clean(st$live_data_statements), ethics = clean(st$ethics_statements))
}

# A text (the README) as a paper object of sentences, for ethics_check.
.tdc_text_paper <- function(text) {
  s <- trimws(unlist(strsplit(text, "(?<=[.!?])\\s+|\\r?\\n", perl = TRUE)))
  s <- s[nzchar(s)]
  if (!length(s)) return(NULL)
  metacheck::test_paper(text = s)
}

# TU/e's GROBID server, which converts the scientific documents (on a paper
# new to them, much faster than the SURF API, which is used when GROBID
# fails); the option "tdc.grobid_url" names another.
.tdc_grobid_url <- "https://grobid.hti.ieis.tue.nl"

# Folders of scientific documents ("1 Scientific documents", "manuscripts",
# "papers", ...), compared as .tdc_norm_name() writes folder names.
.tdc_science_folder_re <- "^(?:scientific documents?|manuscripts?|papers?|publications?|articles?|preprints?)$"

# The scientific documents to read on human participants, in order: the
# manuscript (by name), documents in a folder of scientific documents, the
# methodology (protocols, questionnaires, ...), and documentation PDFs at the
# top of the package; the largest first within each. Documents of another
# component (ERB letter, consent forms, data management plan, codebook, lab
# journal, agreements) say nothing about the study design and are left out.
# `paths` are the component files (from the components table). Returns
# data.frame(path, location, ext) of every candidate, in that order.
.tdc_science_docs <- function(paths, files) {
  is_doc <- files$data_type %in% "documentation" & !files$doc_role %in% "readme"
  other <- unlist(paths[c("dmp", "ethics", "consent", "codebook", "lab_journal", "agreements")])
  segs <- strsplit(files$path, "/", fixed = TRUE)
  in_science_folder <- vapply(segs, function(s) {
    s <- s[-length(s)]
    length(s) > 0 && any(grepl(.tdc_science_folder_re, .tdc_norm_name(s), perl = TRUE))
  }, logical(1))
  rank <- ifelse(files$path %in% paths[["manuscript"]], 1,
          ifelse(in_science_folder, 2,
          ifelse(files$path %in% paths[["methodology"]], 3,
          ifelse(!grepl("/", files$path) & files$ext == "pdf", 4, NA))))
  keep <- is_doc & !is.na(rank) & !files$path %in% other
  docs <- files[keep, , drop = FALSE]
  docs <- docs[order(rank[keep], -docs$size), c("path", "location", "ext"), drop = FALSE]
  rownames(docs) <- NULL
  docs
}

# A scientific document as a paper object: a PDF converted with TU/e's GROBID
# server, else with the SURF API (convert_bibr()); a TXT file read as it is.
# Returns list(paper, how) or list(paper = NULL, why).
.tdc_read_doc <- function(doc, out_json) {
  if (is.na(doc$location) || !file.exists(doc$location)) {
    return(list(paper = NULL, why = "it is not a local file"))
  }
  if (doc$ext == "txt") {
    text <- .tdc_read_text(doc$location)
    if (is.null(text) || !nzchar(trimws(text))) return(list(paper = NULL, why = "it is empty"))
    return(list(paper = .tdc_text_paper(text), how = "read"))
  }
  if (doc$ext != "pdf") {
    return(list(paper = NULL, why = "only PDF and TXT documents can be turned into text"))
  }
  read_json <- function(json) {
    if (length(json) != 1 || is.na(json) || !file.exists(json)) stop("no text came back")
    metacheck::read(json)
  }
  paper <- tryCatch(suppressWarnings(suppressMessages(read_json(
    metacheck::convert(doc$location, out_json, method = "grobid", keep_xml = FALSE,
                       api_url = getOption("tdc.grobid_url", .tdc_grobid_url))))),
    error = function(e) e)
  if (!inherits(paper, "error")) return(list(paper = paper, how = "converted with TU/e's GROBID server"))
  grobid_why <- conditionMessage(paper)
  # convert_bibr() picks the bibr: on the SURF server itself (BIBR_API_URL is
  # set there) its own bibr, so a job never waits for another job of the same
  # server; elsewhere the SURF API.
  backend <- if (nzchar(Sys.getenv("SCIVRS_API_KEY")) || nzchar(Sys.getenv("BIBR_API_URL"))) "auto" else "surf"
  where <- if (backend == "surf") "the SURF API" else "bibr"
  paper <- tryCatch(suppressMessages(read_json(
    metacheck::convert_bibr(doc$location, save_path = dirname(out_json), backend = backend))),
    error = function(e) e)
  if (!inherits(paper, "error")) {
    return(list(paper = paper, how = sprintf("converted with %s, because TU/e's GROBID server could not convert it", where)))
  }
  list(paper = NULL, why = sprintf("neither TU/e's GROBID server (%s) nor %s (%s) could convert it",
                                   grobid_why, where, conditionMessage(paper)))
}

# The LLM's judgement on the texts: list(answer, quote, source), or an empty
# list (with attribute "why") without an answer. A quote that is not in the
# texts, word for word (ignoring capitals, spaces and punctuation), is dropped,
# and a "yes" without a quote found in the texts becomes "unclear".
.tdc_people_ai <- function(texts) {
  prompt <- paste(
    "You decide whether a research data package comes from research in which people took part,",
    "or in which personal data about people was used.",
    "'yes' = people took part as participants (for example in an experiment, survey, questionnaire,",
    "interview, focus group, observation, or measurements of their body or behaviour), or data about",
    "identifiable people was collected or used (for example patient records, names, e-mail addresses,",
    "recordings of people).",
    "'no' = the texts make clear that the research did not involve people: for example experiments on",
    "materials, devices or physical systems, computer simulations, or simulated data.",
    "'unclear' = the texts do not say enough to decide.",
    "The researchers themselves, the authors, the contacts and the people who are thanked are not participants.",
    "For 'yes', copy the sentence that shows it, word for word, as the quote, and name the text it comes from.",
    "For 'no', copy the sentence that shows what was studied instead, if there is one.",
    "For 'unclear', leave the quote empty.",
    "Judge only from the texts given.")
  input <- paste(vapply(seq_along(texts), function(i) {
    sprintf("TEXT %d: %s\n%s", i, toupper(names(texts)[[i]]), texts[[i]])
  }, character(1)), collapse = "\n\n")
  tp <- ellmer::type_object(
    answer = ellmer::type_enum(c("yes", "no", "unclear"), "whether people took part or personal data was used"),
    quote = ellmer::type_string("the sentence that shows it, copied word for word; empty for 'unclear'"),
    source = ellmer::type_string("the name of the text the quote comes from, as given after TEXT n:"))
  ans <- .tdc_llm(input, prompt, tp, params = list(temperature = 0))
  if (!length(ans) || !"answer" %in% names(ans)) {
    return(structure(list(), why = attr(ans, "why") %||% "the LLM gave no answer"))
  }
  answer <- as.character(ans$answer[[1]])
  # the model leaves the quote out (NA) for "unclear"
  quote <- trimws(as.character(ans$quote[[1]] %||% ""))
  if (!length(quote) || is.na(quote)) quote <- ""
  # the source is the text the quote is in (not the model's name for it)
  norm <- function(x) trimws(gsub("\\s+", " ", gsub("[^a-z0-9]+", " ", tolower(x))))
  in_text <- if (nzchar(norm(quote))) {
    names(texts)[vapply(texts, function(t) grepl(norm(quote), norm(t), fixed = TRUE), logical(1))]
  } else character(0)
  if (!length(in_text)) quote <- ""
  source <- if (length(in_text)) in_text[[1]] else ""
  if (identical(answer, "yes") && !nzchar(quote)) answer <- "unclear"
  if (!answer %in% c("yes", "no", "unclear")) answer <- "unclear"
  list(answer = answer, quote = quote, source = source)
}

# Shortened for a summary line.
.tdc_short_quote <- function(x, n = 160) {
  if (!length(x) || is.na(x[[1]])) return("")
  x <- gsub("\\s+", " ", trimws(x))
  if (nchar(x) > n) paste0(substr(x, 1, n - 1), "…") else x
}

# Whether people took part or personal data was used. `paper` is the paper
# given to the module; `readme_text` the README (or NULL); `component_paths`
# the files of each component (from the components table); `personal` the
# personal data found in the data (data.frame(path, column, kind));
# `pkg_info` the package description; `max_docs` how many scientific
# documents are read at most. Returns list(answer = "yes"/"no"/"unclear",
# source, quote, how, read, ethics_statements, item).
.tdc_people <- function(human_participants, paper, readme_text, component_paths,
                        files, personal, pkg_info, rules, max_docs = 3) {
  title <- "Human participants or personal data"
  read <- character(0)
  ethics_statements <- list()
  out <- function(answer, source, quote, how) {
    status <- if (answer == "unclear") .tdc_sev(rules, "people-unclear") else "pass"
    todo <- if (answer == "unclear") "Check whether people took part or personal data was used, and give the answer to the module as `human_participants = TRUE` or `FALSE`."
    list(answer = answer, source = source, quote = quote, how = how, read = read,
         ethics_statements = ethics_statements,
         item = .tdc_item("human_participants", "1.3", title, status, how, todo))
  }

  # The paper and the README are read first, whatever answers the question:
  # their ethics statements are reported with the ERB letter.
  texts <- list()
  live <- list()
  add_paper <- function(name, p, max_chars = 20000) {
    if (is.null(p)) return()
    es <- .tdc_ethics_statements(p)
    if (length(es$ethics)) ethics_statements[[name]] <<- es$ethics
    if (length(es$live)) live[[name]] <<- es$live
    excerpt <- if (identical(p$info$input_format, "test")) {
      substr(paste(p$text$text, collapse = " "), 1, min(12000, max_chars))
    } else .tdc_people_excerpt(p, max_chars)
    if (nzchar(excerpt)) texts[[name]] <<- excerpt
  }
  if (.tdc_has_text(paper)) {
    add_paper("the paper", paper)
    read <- c(read, "the paper")
  }
  if (!is.null(readme_text) && nzchar(trimws(readme_text))) {
    add_paper("the README", .tdc_text_paper(readme_text))
    read <- c(read, "the README")
  }

  # 1. the data steward's yes
  if (isTRUE(human_participants)) {
    read <- character(0)
    return(out("yes", "the data steward", "", "Given to the module (`human_participants`)."))
  }

  # 2. personal data in the data: always yes, also over the data steward's no
  if (!is.null(personal) && nrow(personal)) {
    read <- "the data"
    by_file <- vapply(split(personal, factor(personal$path, unique(personal$path))), function(d) {
      cols <- d$column[nzchar(d$column)]
      if (length(cols)) sprintf("%s in `%s`", .tdc_list(paste0("`", cols, "`"), 4), d$path[[1]]) else sprintf("`%s`", d$path[[1]])
    }, character(1))
    return(out("yes", sprintf("the data (%s)", .tdc_list(by_file, 2)), "",
               c(sprintf("The data holds personal data or demographic columns: %s.",
                         .tdc_list(sprintf("%s (`%s`%s)", personal$kind, personal$path,
                                           ifelse(nzchar(personal$column), sprintf(", column `%s`", personal$column), "")), 5)),
                 if (isFALSE(human_participants)) "This overrules the answer given to the module (`human_participants = FALSE`): personal data in the data always counts.")))
  }

  # 3. the data steward's no
  if (isFALSE(human_participants)) {
    read <- character(0)
    return(out("no", "the data steward", "", "Given to the module (`human_participants`)."))
  }

  # 4. data collection from people found by ethics_check, in the paper, the
  # README, or (when the paper has no text) up to `max_docs` scientific
  # documents of the package, which share the LLM's 20,000 characters
  docs_note <- NULL
  if (!length(live) && !.tdc_has_text(paper)) {
    docs <- .tdc_science_docs(component_paths, files)
    use <- utils::head(docs, max_docs)
    out_dir <- file.path(tempdir(), "tdc_docs")
    dir.create(out_dir, showWarnings = FALSE)
    not_read <- character(0)
    for (i in seq_len(nrow(use))) {
      rd <- .tdc_read_doc(use[i, ], file.path(out_dir, sprintf("doc%d.json", i)))
      if (is.null(rd$paper)) {
        not_read <- c(not_read, sprintf("`%s` (%s)", use$path[[i]], rd$why))
        next
      }
      add_paper(sprintf("`%s`", use$path[[i]]), rd$paper, max_chars = 20000 %/% nrow(use))
      read <- c(read, sprintf("`%s` (%s)", use$path[[i]], rd$how))
    }
    docs_note <- c(
      if (length(not_read)) sprintf("Not read: %s.", paste(not_read, collapse = "; ")),
      if (nrow(docs) > max_docs) sprintf("Not read, because at most %d documents are read: %s.", max_docs,
                                         .tdc_list(paste0("`", docs$path[-seq_len(max_docs)], "`"), 5)))
  }
  if (length(live)) {
    name <- names(live)[[1]]
    return(out("yes", name, live[[1]][[1]],
               c(sprintf("%s describes data collection from people (found by metacheck's ethics_check).",
                         sub("^the", "The", name)), docs_note)))
  }

  # 5. the LLM, on the texts and the file and folder names
  people_names <- unique(files$path[grepl(.tdc_people_re, .tdc_norm_name(files$path), ignore.case = TRUE, perl = TRUE)])
  texts[["file and folder names"]] <- paste0(
    pkg_info,
    if (length(people_names)) paste0("\nFiles whose names mention people: ",
                                     paste(utils::head(people_names, 30), collapse = ", ")) else "")
  read <- c(read, "the file and folder names")
  ai <- .tdc_people_ai(texts)
  if (!length(ai)) {
    return(out("unclear", "", "",
               c(sprintf("No AI judgement (%s), and no data collection from people was found in %s.",
                         attr(ai, "why"), .tdc_list(read, 6)), docs_note)))
  }
  how <- sprintf("Judged by the AI (%s) from %s.", metacheck::llm_model(), .tdc_list(read, 6))
  out(ai$answer, ai$source, ai$quote, c(how, docs_note))
}

# -- the ERB approval letter and the signed informed consent forms ------------

# Names that say a consent form is blank.
.tdc_blank_re <- "(?<![a-z])(?:templates?|blank|empty|examples?|samples?|leeg|voorbeeld|sjabloon)(?![a-z])"

# Two checklist rows: the ERB letter (component "ethics") and the signed
# consent forms (component "consent"), from the files the components table
# found, the answer on people, and ethics statements found in the texts (an
# approval only mentioned in a text is no letter).
.tdc_check_ethics_docs <- function(answer, paths, files, ethics_statements, rules) {
  mentioned <- unlist(ethics_statements)
  one <- function(id, title, what) {
    found <- paths[[id]] %||% character(0)
    ext <- files$ext[match(found, files$path)]
    not_pdf <- found[!ext %in% "pdf"]
    blank <- if (id == "consent") found[grepl(.tdc_blank_re, .tdc_norm_name(basename(found)), ignore.case = TRUE, perl = TRUE)] else character(0)
    if (length(found)) {
      status <- .tdc_worst(c("pass",
                             if (length(not_pdf)) .tdc_sev(rules, "ethics-wrong-format"),
                             if (length(blank) && answer != "no") .tdc_sev(rules, "consent-blank-template")))
      blank_problem <- length(blank) && answer != "no"
      found_txt <- c(sprintf("Found: %s.", .tdc_list(paste0("`", found, "`"), 5)),
                     if (length(not_pdf)) sprintf("Not PDF, as asked: %s.", .tdc_list(paste0("`", not_pdf, "`"), 5)),
                     if (blank_problem) sprintf("%s look%s like a blank form.",
                                                .tdc_list(paste0("`", blank, "`"), 5), if (length(blank) == 1) "s" else ""))
      todo <- c(if (length(not_pdf)) sprintf("Save %s as PDF.", if (length(not_pdf) == 1) "it" else "them"),
                if (blank_problem) "Add the signed forms.")
      return(.tdc_item(id, "1.3", title, status, found_txt, todo))
    }
    if (answer == "no") {
      return(.tdc_item(id, "1.3", title, "na", "Not needed: no people took part and no personal data was used."))
    }
    mention <- if (id == "ethics" && length(mentioned)) {
      sprintf("An approval is mentioned (\"%s\").", .tdc_short_quote(mentioned[[1]], 120))
    }
    why <- if (!is.null(mention)) "The letter itself must be in the package."
    if (answer == "yes") {
      return(.tdc_item(id, "1.3", title, .tdc_sev(rules, "ethics-missing"),
                       c("Not found.", mention), sprintf("Add %s (PDF).", what), why))
    }
    .tdc_item(id, "1.3", title, .tdc_sev(rules, "ethics-unknown"),
              c("Not found.", mention),
              sprintf("Check whether people took part or personal data was used; if so, add %s (PDF).", what), why)
  }
  rbind(one("ethics", "Ethical approval (ERB letter)", "the ERB approval letter"),
        one("consent", "Signed informed consent forms", "the signed informed consent forms"))
}

# The personal data found in the data by data_check: data.frame(path, column,
# kind), from its personal-information findings (values and column names)
# and its demographic columns.
.tdc_personal_from_dc <- function(dc) {
  none <- data.frame(path = character(0), column = character(0), kind = character(0))
  if (is.null(dc)) return(none)
  f <- dc$findings
  pii <- if (!is.null(f) && nrow(f) && all(c("check", "source_file", "column") %in% names(f))) {
    f <- f[f$check %in% c("Personal info (values)", "Personal info (column name)"), , drop = FALSE]
    data.frame(path = f$source_file, column = f$column,
               kind = ifelse(f$check == "Personal info (values)", "personal information", "a column name that suggests personal information"))
  } else none
  d <- dc$demographics
  demo <- if (!is.null(d) && nrow(d)) {
    data.frame(path = d$source_file, column = d$column, kind = paste("demographic column:", d$demographic))
  } else none
  out <- rbind(pii, demo)
  out[!duplicated(out[c("path", "column")]), , drop = FALSE]
}
