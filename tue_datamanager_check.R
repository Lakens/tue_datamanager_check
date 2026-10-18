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
#' @metacheck 0.4.0.4
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
#' @param ... further arguments (not used)
#'
#' @returns a list with the checklist (`table`), the summary, the traffic
#'   light and the report
tue_datamanager_check <- function(paper, local_path = NULL,
                                  human_participants = NA,
                                  readme_revised_dir = ".", ...) {
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

  # -- the package ---------------------------------------------------------
  pkg <- .tdc_package(local_path)
  rc <- if (!is.null(pkg$path)) {
    module_run(paper, "repo_check", local_path = pkg$path, local_only = TRUE)
  } else {
    module_run(paper, "repo_check")
  }
  dc <- tryCatch(module_run(rc, "data_check"), error = function(e) NULL)

  files <- .tdc_files(rc$table)
  folders <- .tdc_folders(pkg$path, files)
  pkg_name <- if (!is.null(pkg$given)) tools::file_path_sans_ext(basename(pkg$given)) else "package"

  # -- 1.2 the README (first: its sections count for the components) -------
  readme <- .tdc_find_readme(files)
  rd <- .tdc_check_readme(readme, files, folders, rules,
                          pkg_info = .tdc_package_info(files, folders),
                          revised_file = file.path(readme_revised_dir, paste0(pkg_name, "_README_revised.txt")))

  # -- 1.1 components and structure ----------------------------------------
  comp <- .tdc_check_components(files, readme_sections = rd$found_sections, rules)
  items <- rbind(
    comp$item,
    .tdc_check_depth(files, folders, rules),
    .tdc_check_layout(files, folders, rules),
    .tdc_check_classification(files, rules),
    .tdc_check_size(files, rules),
    rd$items
  )
  tree <- metacheck:::.file_tree(list(repo_check = rc, data_check = dc))

  # -- 1.3 human participants, ERB letter and consent forms ----------------
  # personal data in the data (section 2) also answers whether people took
  # part: data_check's identifiers and demographic columns, BSN and phone
  # numbers, and pseudonymisation key files
  pd <- .tdc_personal_data(dc, files, rules)
  pd_people <- pd[!pd$kind %in% c("free text that may hold personal details", "geographic coordinates"), , drop = FALSE]
  personal <- rbind(.tdc_personal_from_dc(dc),
                    data.frame(path = pd_people$path, column = pd_people$column, kind = pd_people$kind))
  personal <- personal[!duplicated(personal[c("path", "column")]), , drop = FALSE]
  readme_text <- if (!is.null(readme)) .tdc_read_text(readme$location)
  people <- .tdc_people(human_participants, paper, readme_text,
                        component_paths = comp$paths, files = files,
                        personal = personal,
                        pkg_info = .tdc_package_info(files, folders), rules = rules)
  items <- rbind(items, people$item,
                 .tdc_check_ethics_docs(people$answer, comp$paths, files,
                                        people$ethics_statements, rules))

  # -- 2 file checks -------------------------------------------------------
  members <- .tdc_zip_members(files)
  fmt <- .tdc_check_formats(files, members, rules)
  chars <- .tdc_check_name_characters(files, folders, rules)
  conv <- .tdc_check_name_conventions(files, folders, rules)
  junk <- .tdc_check_junk(rc$useless_files, pkg$outer_junk, rules)
  pers <- .tdc_check_personal_data(pd, length(dc$previews))
  cb <- if (!is.null(dc)) tryCatch(module_run(dc, "codebook_check"), error = function(e) NULL)
  cbc <- .tdc_check_codebook_columns(cb, rules)
  codebook_files <- files$location[files$path %in% comp$paths[["codebook"]] &
                                     files$ext %in% c("txt", "md", "csv", "tsv")]
  doc_text <- paste(c(readme_text, unlist(lapply(codebook_files, .tdc_read_text))), collapse = "\n")
  miss <- .tdc_check_missing_units(dc, files, doc_text, rules)
  cc <- tryCatch(module_run(if (!is.null(cb)) cb else if (!is.null(dc)) dc else rc, "code_check"),
                 error = function(e) NULL)
  code <- .tdc_check_code(cc, rules)
  zipped <- .tdc_check_zipped(pkg, files, rules)
  items <- rbind(items, fmt$item, chars$item, conv$item, junk$item, pers$item, cbc$item, miss$item,
                 code$item, zipped$item)
  file_checks <- list(formats = fmt, name_characters = chars, name_conventions = conv, junk = junk,
                      personal_data = pers, codebook_columns = cbc, missing_values = miss,
                      code_check = code, zipped = zipped)

  # -- the result ----------------------------------------------------------
  report <- .tdc_report(items, comp$table, tree, pkg, rd, people, file_checks)
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
    report = report
  )
}
