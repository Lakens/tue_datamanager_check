#' TU/e Data Manager Check
#'
#' @description
#' Checks a data package the way TU/e's RAPS data managers (RDLC) do: its
#' components and structure, its README, ethical approval and consent when
#' people took part, and its files (formats, names, junk, personal data,
#' codebook, missing values, code).
#'
#' @details
#' The checks and their results follow TU/e's archiving guidelines (Research
#' Cockpit) and the RDLC document "Using Metacheck for data package review and
#' paper analysis"; every decision is written down in SPECIFICATION.md in this
#' module's repository. The report is written to the researcher, as guidance
#' for their data package.
#'
#' The module runs metacheck's repo_check, data_check, codebook_check and
#' code_check itself, with the LLM off; it uses the LLM only for its own calls
#' (the README against the TU/e template, a revised README, and whether people
#' took part), and only a local Ollama model. To decide whether people took
#' part, up to 3 scientific documents in the package (the manuscript, the
#' methodology) are turned into text, a PDF with TU/e's GROBID server or, when
#' that fails, metacheck's API on SURF Research Cloud; but only when `paper`
#' has no text and the data and README have not already answered it. The
#' rules (preferred formats, folder layouts, package
#' components, the README template, how serious each finding is) are files in
#' this module's `data/` folder, so they can be changed without changing code.
#'
#' @keywords general
#'
#' @author Daniel Lakens
#'
#' @metacheck 0.4.0
#'
#' @param paper a paper object (for a data package alone, `test_paper()`, as
#'   `report_repository()` gives it)
#' @param local_path the data package: a folder, a zip of one, or a link to
#'   an online repository (GitHub, OSF, Zenodo, Dataverse, ...), whose public
#'   files are downloaded (at most 100 MB per file, 500 MB in all) and checked
#'   like a folder
#' @param human_participants whether people took part or personal data was
#'   used: `TRUE`, `FALSE`, or `NA` (default) to decide it from the paper and
#'   the package
#' @param readme_revised_dir the folder to write the revised README to
#'   (default: the working folder, where `report_repository()` writes its
#'   report)
#' @param run_code whether to run the package's R and Python code with
#'   metacheck's reproducibility_check (default `FALSE`): it runs the code on
#'   this computer, so only ask for it for a package you trust, or in a
#'   sandbox (`run_args`)
#' @param run_args further arguments to reproducibility_check when
#'   `run_code = TRUE`, such as `sandbox = "docker"` and `timeout` (seconds per
#'   script)
#' @param ... further arguments (not used)
#'
#' @returns a list with the checklist (`table`), the summary, the traffic
#'   light and the report
tue_datamanager_check <- function(paper, local_path = NULL,
                                  human_participants = NA,
                                  readme_revised_dir = ".", run_code = FALSE,
                                  run_args = list(), ...) {
  # -- this module's own folder: its rules (data/) and helper code (R/) ------
  # module_run() sources this file from inside its own frame, where
  # `module_path` is the path of this file.
  mod_path <- get0("module_path", envir = parent.env(environment()),
                   ifnotfound = getOption("tdc.module_path"))
  if (is.null(mod_path)) stop("tue_datamanager_check must be run with module_run()")
  mod_dir <- dirname(mod_path)
  for (f in list.files(file.path(mod_dir, "R"), "\\.R$", full.names = TRUE)) {
    sys.source(f, envir = environment())
  }
  rules <- .tdc_rules(mod_dir)

  # -- metacheck's modules, with the LLM off (also while a linked
  # repository is listed and fetched) -------------------------------------
  llm_before <- metacheck::llm_use()
  metacheck::llm_use(FALSE)
  on.exit(metacheck::llm_use(llm_before), add = TRUE)

  # -- a check that fails gives one row of its own --------------------------
  # Every check runs through safe(): an error becomes a "decide by hand" row
  # for that check (.tdc_check_failed()), so one file the readers cannot
  # handle never costs the whole report. `fallback` is what the later checks
  # and the report use instead of the check's result: a check that returns
  # its row alone gets the row, one that returns list(item, ...) gets
  # c(list(item = row), fallback).
  safe <- function(expr, id, section, title, fallback = NULL) {
    tryCatch(expr, error = function(e) {
      row <- .tdc_check_failed(id, section, title, e)
      if (is.null(fallback)) row else c(list(item = row), fallback)
    })
  }
  no_findings <- list(table = NULL, findings = NULL)
  # metacheck's modules: the result, or the error (class "error")
  run_quietly <- function(expr) tryCatch(expr, error = function(e) e)
  failed <- function(x) inherits(x, "error")

  # -- the package ---------------------------------------------------------
  pkg <- .tdc_package(local_path)
  rc <- tryCatch(if (!is.null(pkg$path)) {
    module_run(paper, "repo_check", local_path = pkg$path, local_only = TRUE)
  } else {
    module_run(paper, "repo_check")
  }, error = function(e) {
    stop("The package's files could not be listed (metacheck's repo_check): ", conditionMessage(e), call. = FALSE)
  })
  # data_check warns "Could not read <file>: <reason>" for each data file it
  # cannot open; those warnings are kept for "Data files can be opened"
  unreadable <- character(0)
  dc <- run_quietly(withCallingHandlers(module_run(rc, "data_check"), warning = function(w) {
    msg <- conditionMessage(w)
    if (startsWith(msg, "Could not read ")) {
      unreadable <<- c(unreadable, sub("^Could not read ", "", msg))
      invokeRestart("muffleWarning")
    }
  }))
  dc_error <- if (failed(dc)) dc
  if (failed(dc)) dc <- NULL

  files <- .tdc_files(rc$table)
  folders <- .tdc_folders(pkg$path, files)
  # only an archive extension is removed: file_path_sans_ext() took the end of
  # a folder name with dots for one ("doi.org_10.5061_dryad.fk7bh24")
  pkg_name <- if (!is.null(pkg$given))
    sub("\\.(zip|tar\\.gz|tgz|tar)$", "", basename(pkg$given), ignore.case = TRUE) else "package"
  pkg_info <- tryCatch(.tdc_package_info(files, folders), error = function(e) "")

  # -- 1.2 the README (first: its sections count for the components) -------
  readme <- .tdc_find_readme(files)
  rd <- tryCatch(
    .tdc_check_readme(readme, files, folders, rules, pkg_info = pkg_info,
                      revised_file = file.path(readme_revised_dir, paste0(pkg_name, "_README_revised.txt"))),
    error = function(e) {
      list(items = rbind(.tdc_check_readme_exists(readme, rules),
                         .tdc_check_failed("readme_template", "1.2", "README follows the TU/e template", e,
                                           what = "The README check")),
           sections = NULL, found_sections = character(0), revised = NULL)
    })

  # -- 1.1 components and structure ----------------------------------------
  no_components <- data.frame(component = character(0), group = character(0), title = character(0),
                              found = character(0), files = character(0), n_files = integer(0),
                              status = character(0), note = character(0))
  comp <- safe(.tdc_check_components(files, readme_sections = rd$found_sections, rules),
               "components", "1.1", "Package components",
               fallback = list(table = no_components, paths = list()))
  items <- rbind(
    comp$item,
    safe(.tdc_check_depth(files, folders, rules), "folder_depth", "1.1", "Folder depth (up to 3 levels)"),
    safe(.tdc_check_layout(files, folders, rules), "folder_layout", "1.1", "Component-level organisation"),
    safe(.tdc_check_classification(files, rules), "file_classification", "1.1", "File classification"),
    safe(.tdc_check_size(files, rules), "package_size", "1.1", "Package size"),
    rd$items
  )
  tree <- tryCatch(metacheck:::.file_tree(list(repo_check = rc, data_check = dc)),
                   error = function(e) sprintf("(The file tree could not be drawn: %s.)", conditionMessage(e)))

  # -- 1.3 human participants, ERB letter and consent forms ----------------
  # personal data in the data (section 2) also answers whether people took
  # part: data_check's identifiers and demographic columns, BSN and phone
  # numbers, and pseudonymisation key files
  no_personal <- data.frame(path = character(0), column = character(0), kind = character(0))
  pd <- run_quietly(.tdc_personal_data(dc, files, rules))
  pd_error <- if (failed(pd)) pd
  if (failed(pd)) pd <- cbind(no_personal, status = character(0))
  pd_people <- pd[!pd$kind %in% c("free text that may hold personal details", "geographic coordinates"), , drop = FALSE]
  personal <- rbind(tryCatch(.tdc_personal_from_dc(dc), error = function(e) no_personal),
                    data.frame(path = pd_people$path, column = pd_people$column, kind = pd_people$kind))
  personal <- personal[!duplicated(personal[c("path", "column")]), , drop = FALSE]
  readme_text <- if (!is.null(readme)) .tdc_read_text(readme$location)
  people <- tryCatch(
    .tdc_people(human_participants, paper, readme_text,
                component_paths = comp$paths, files = files,
                personal = personal, pkg_info = pkg_info, rules = rules),
    error = function(e) {
      list(answer = "unclear", source = "", quote = "", how = "", read = character(0),
           ethics_statements = list(),
           item = .tdc_check_failed("human_participants", "1.3", "Human participants or personal data", e))
    })
  items <- rbind(items, people$item,
                 safe(.tdc_check_ethics_docs(people$answer, comp$paths, files,
                                             people$ethics_statements, rules),
                      "ethics", "1.3", "Ethical approval (ERB letter) and signed informed consent forms"))

  # -- 2 file checks -------------------------------------------------------
  members <- tryCatch(.tdc_zip_members(files), error = function(e) .tdc_zip_members(files[0, , drop = FALSE]))
  fmt <- safe(.tdc_check_formats(files, members, rules), "formats", "2", "File formats", no_findings)
  readable <- safe(.tdc_check_data_readable(unique(unreadable), dc_error, rules, files, dc$table, dc$findings),
                   "data_readable", "2", "Data files can be opened", no_findings)
  chars <- safe(.tdc_check_name_characters(files, folders, rules),
                "name_characters", "2", "No spaces or special characters in names", no_findings)
  conv <- safe(.tdc_check_name_conventions(files, folders, rules),
               "name_conventions", "2", "Naming conventions (dates, numbering, one style per folder)", no_findings)
  junk <- safe(.tdc_check_junk(rc$useless_files, pkg$outer_junk, rules),
               "junk", "2", "No junk or temporary files", no_findings)
  pers <- if (!is.null(pd_error)) {
    list(item = .tdc_check_failed("personal_data", "2", "No personal data", pd_error), table = NULL, findings = pd)
  } else {
    safe(.tdc_check_personal_data(pd, length(dc$previews)), "personal_data", "2", "No personal data",
         list(table = NULL, findings = pd))
  }
  cb <- if (!is.null(dc)) run_quietly(module_run(dc, "codebook_check"))
  cbc <- if (failed(cb)) {
    list(item = .tdc_check_failed("codebook_columns", "2", "Columns are described in the codebook", cb,
                                  what = "metacheck's codebook_check"), table = NULL, findings = NULL)
  } else {
    safe(.tdc_check_codebook_columns(cb, rules), "codebook_columns", "2",
         "Columns are described in the codebook", no_findings)
  }
  if (failed(cb)) cb <- NULL
  codebook_files <- files$location[files$path %in% comp$paths[["codebook"]] &
                                     files$ext %in% c("txt", "md", "csv", "tsv")]
  doc_text <- paste(c(readme_text, unlist(lapply(codebook_files, .tdc_read_text))), collapse = "\n")
  miss <- safe(.tdc_check_missing_units(dc, files, doc_text, rules), "missing_values", "2",
               "One notation for missing values per table", c(no_findings, list(note = NULL, units = NULL)))
  cc <- run_quietly(module_run(if (!is.null(cb)) cb else if (!is.null(dc)) dc else rc, "code_check"))
  code <- if (failed(cc)) {
    list(item = .tdc_check_failed("code_check", "2", "Code check (metacheck's code_check)", cc,
                                  what = "metacheck's code_check"), table = NULL, note = NULL)
  } else {
    safe(.tdc_check_code(cc, rules), "code_check", "2", "Code check (metacheck's code_check)",
         list(table = NULL, note = NULL))
  }
  # running the code: metacheck's reproducibility_check on code_check's
  # results (and data_check's, before them)
  # The code runs in a copy of the package as it is, every file in its own
  # place (layout = "package"), not in a Psych-DS release of its data files
  # only. Whether reproducibility_check has `layout` is asked, not read from
  # metacheck's version: the numbers went back after the 0.4.0 release
  # (development versions 0.4.0.4 to 0.4.0.11, then 0.4.0.1).
  rp <- if (isTRUE(run_code) && failed(cc)) {
    simpleError(paste("it needs metacheck's code_check, which stopped:", conditionMessage(cc)))
  } else if (isTRUE(run_code) && !is.null(cc) && !identical(cc$traffic_light, "na")) {
    repro_args <- list(cc, "reproducibility_check", execute = TRUE)
    has_layout <- tryCatch("layout" %in% names(metacheck::module_info("reproducibility_check")$arg_defaults),
                           error = function(e) FALSE)
    if (has_layout) repro_args$layout <- "package"
    run_quietly(do.call(metacheck::module_run, utils::modifyList(repro_args, run_args)))
  }
  if (failed(cc)) cc <- NULL
  runs <- safe(.tdc_check_code_runs(rp, run_code, rules), "code_runs", "2",
               "The code runs (metacheck's reproducibility_check)", list(table = NULL, note = NULL))
  zipped <- safe(.tdc_check_zipped(pkg, files, rules), "zipped", "2", "Not zipped", no_findings)
  items <- rbind(items, fmt$item, readable$item, chars$item, conv$item, junk$item, pers$item, cbc$item,
                 miss$item, code$item, runs$item, zipped$item)
  file_checks <- list(formats = fmt, data_readable = readable, name_characters = chars,
                      name_conventions = conv, junk = junk, personal_data = pers,
                      codebook_columns = cbc, missing_values = miss,
                      code_check = code, code_runs = runs, zipped = zipped)

  # -- the result ----------------------------------------------------------
  report <- .tdc_report(items, comp$table, tree, pkg, rd, people, file_checks)
  checks <- .tdc_checks(items, comp$table, tree, rd, people, file_checks)
  counts <- table(factor(items$status, c("fail", "warn", "manual", "pass", "na")))
  summary_text <- sprintf("%d to fix, %d to improve, %d to decide by hand, %d fine.",
                          counts[["fail"]], counts[["warn"]], counts[["manual"]], counts[["pass"]])
  tl <- if (counts[["fail"]] > 0) "red" else if (counts[["warn"]] + counts[["manual"]] > 0) "yellow" else "green"

  list(
    table = items,
    summary_table = data.frame(paper_id = paper$paper_id,
                               tdc_fail = counts[["fail"]], tdc_warn = counts[["warn"]],
                               tdc_manual = counts[["manual"]], tdc_pass = counts[["pass"]]),
    components = comp$table,
    readme_sections = rd$sections,
    readme_lines = rd$lines,
    readme_revised = rd$revised$text,
    readme_revised_file = rd$revised$file,
    human_participants = people[c("answer", "source", "quote", "how", "read")],
    formats = fmt$findings,
    name_issues = rbind(chars$findings, conv$findings),
    junk = junk$findings,
    personal_data = pd,
    codebook_undescribed = cbc$findings,
    missing_mixed = miss$findings,
    traffic_light = tl,
    summary_text = summary_text,
    report = report,
    # for metacheck's checklist report (report_type("checklist"))
    checks = checks,
    intro = c("This report checks the data package against TU/e's archiving guidelines, the way the RAPS data managers do. Each check below says what is fine and what to fix or improve before archiving.",
              .tdc_package_note(pkg)),
    logo = file.path(mod_dir, "web", "www", "images", "tuelogo.png")
  )
}
