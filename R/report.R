# The report, written to the researcher, in the sections of the RDLC document.

# Results as the report words them.
.tdc_result_words <- c(fail = "to fix", warn = "to improve", manual = "decide by hand",
                       pass = "fine", na = "not applicable")

.tdc_report_items <- function(items) {
  result <- items$result %||% unname(.tdc_result_words[items$status])
  vapply(seq_len(nrow(items)), function(i) {
    sprintf("**%s:** %s. %s", items$title[[i]], result[[i]], items$detail[[i]])
  }, character(1))
}

# The answer on human participants, as its summary line shows it instead of
# pass or fail: "yes, from the README: '40 participants ...'".
.tdc_people_result <- function(people) {
  answer <- c(yes = "yes", no = "no", unclear = "unclear, decide by hand")[[people$answer]]
  if (nzchar(people$quote %||% "")) {
    sprintf("%s, from %s: \"%s\"", answer, people$source, .tdc_short_quote(people$quote))
  } else if (people$answer != "unclear" && nzchar(people$source %||% "")) {
    sprintf("%s, from %s", answer, people$source)
  } else answer
}

# Section 1.3: human participants, the ERB letter and the consent forms.
.tdc_report_people <- function(items, people) {
  s13 <- items[items$section == "1.3", , drop = FALSE]
  statements <- people$ethics_statements
  c("#### 1.3 Research with human participants or personal data",
    .tdc_report_items(s13),
    if (length(people$read)) sprintf("Read to decide this: %s.", .tdc_list(people$read, 6)),
    if (length(statements)) c(
      "**Ethics statements found** (metacheck's ethics_check; a statement is no approval letter):",
      unlist(lapply(names(statements), function(n) {
        sprintf("- In %s: \"%s\"", n, vapply(utils::head(statements[[n]], 3), .tdc_short_quote, character(1), n = 200))
      }))))
}

# Section 1.2: the README checks, the template per section, the revised README.
.tdc_report_readme <- function(items, rd) {
  s12 <- items[items$section == "1.2", , drop = FALSE]
  sec_tbl <- if (!is.null(rd$sections)) {
    data.frame(Section = rd$sections$section,
               `Found?` = ifelse(rd$sections$found, "yes", "no"),
               `Lines missing` = ifelse(rd$sections$found, rd$sections$missing, "(whole section)"),
               `Lines left empty` = rd$sections$empty, check.names = FALSE)
  }
  rev <- rd$revised
  revised <- if (!is.null(rev$text)) {
    c("**Revised README** (written by AI from the researcher's README and the package's files; check it before use):",
      if (length(rev$added)) sprintf("Added by the AI, not in the README or the package (check these): %s.",
                                     paste0("`", rev$added, "`", collapse = ", ")),
      if (!is.null(rev$file)) sprintf("Saved as `%s`.", basename(rev$file)),
      paste(c("::: {style=\"max-height: 40em; overflow: auto;\"}", "```", rev$text, "```", ":::"), collapse = "\n"))
  } else if (!is.null(rev$note)) {
    paste("**Revised README:**", rev$note)
  }
  c("#### 1.2 README",
    .tdc_report_items(s12),
    if (!is.null(sec_tbl)) c("**The README against the TU/e template, per section:**",
                             metacheck::scroll_table(sec_tbl, maxrows = nrow(sec_tbl))),
    revised)
}

# Section 2: the file checks, each with its table when it has one.
.tdc_report_files <- function(items, file_checks) {
  s2 <- items[items$section == "2", , drop = FALSE]
  lines <- .tdc_report_items(s2)
  out <- "#### 2 File checks"
  for (i in seq_len(nrow(s2))) {
    out <- c(out, lines[[i]])
    tbl <- file_checks[[s2$item[[i]]]]$table
    if (!is.null(tbl) && nrow(tbl)) out <- c(out, metacheck::scroll_table(tbl, maxrows = nrow(tbl)))
    out <- c(out, file_checks[[s2$item[[i]]]]$note)
  }
  out
}

# The checks as metacheck's checklist report (report_type("checklist")) reads
# them: one row per check, with what was found and what to do apart, and the
# tables of the module's own report as folded details.
.tdc_checklist_status <- c(fail = "fix", warn = "improve", manual = "decide", pass = "fine", na = "na")
.tdc_section_titles <- c("1.1" = "1.1 Components and structure", "1.2" = "1.2 README",
                         "1.3" = "1.3 Human participants", "2" = "2 File checks")

.tdc_checks <- function(items, components, tree, rd, people, file_checks) {
  checks <- data.frame(status = unname(.tdc_checklist_status[items$status]),
                       title = items$title,
                       section = unname(.tdc_section_titles[items$section]),
                       found = items$found, todo = items$todo, why = items$why, note = items$note,
                       details = "", details_title = "")
  at <- function(id) which(items$item == id)
  fold <- function(id, details, title) {
    i <- at(id)
    if (!length(i) || !length(details)) return(invisible())
    checks$details[[i]] <<- paste(details, collapse = "\n\n")
    checks$details_title[[i]] <<- title
  }

  # 1.1: the components table and the file tree
  comp_tbl <- data.frame(Group = components$group, Component = components$title,
                         `Found?` = components$found, Files = components$files,
                         Note = components$note, check.names = FALSE)
  comp_tbl$Group[duplicated(comp_tbl$Group)] <- ""
  fold("components", metacheck::scroll_table(comp_tbl, maxrows = nrow(comp_tbl)),
       # the same count as the summary ("2 of the 13 components"), which
       # leaves out the components marked "not expected"
       sprintf("Show the %d components TU/e lists",
               sum(components$found != "not expected")))
  fold("file_classification", tree, "File tree")

  # 1.2: the README per template section, and the revised README
  sec_tbl <- if (!is.null(rd$sections)) {
    data.frame(Section = rd$sections$section,
               `Found?` = ifelse(rd$sections$found, "yes", "no"),
               `Lines missing` = ifelse(rd$sections$found, rd$sections$missing, "(whole section)"),
               `Lines left empty` = rd$sections$empty, check.names = FALSE)
  }
  rev <- rd$revised
  revised <- if (!is.null(rev$text)) {
    c("Written by AI from the researcher's README and the package's files; check it before use.",
      if (length(rev$added)) sprintf("Added by the AI, not in the README or the package (check these): %s.",
                                     paste0("`", rev$added, "`", collapse = ", ")),
      if (!is.null(rev$file)) sprintf("Saved as `%s`.", basename(rev$file)),
      paste(c("```", rev$text, "```"), collapse = "\n"))
  } else if (!is.null(rev$note)) rev$note
  fold("readme_template",
       c(if (!is.null(sec_tbl)) metacheck::scroll_table(sec_tbl, maxrows = nrow(sec_tbl)),
         if (!is.null(revised)) c("**Revised README**", revised)),
       "Show the README per template section, and a revised README")

  # 1.3: the answer, how it was found, and the ethics statements
  hp <- at("human_participants")
  if (length(hp)) {
    answer <- .tdc_people_result(people)
    checks$found[[hp]] <- paste0(toupper(substr(answer, 1, 1)), substring(answer, 2), ". ", checks$found[[hp]])
    if (length(people$read)) {
      checks$note[[hp]] <- trimws(paste(checks$note[[hp]], sprintf("Read to decide this: %s.", .tdc_list(people$read, 6))))
    }
  }
  statements <- people$ethics_statements
  if (length(statements)) {
    fold("ethics", c("Ethics statements found by metacheck's ethics_check (a statement is no approval letter):",
                     unlist(lapply(names(statements), function(n) {
                       sprintf("- In %s: \"%s\"", n, vapply(utils::head(statements[[n]], 3), .tdc_short_quote, character(1), n = 200))
                     }))),
         "Show the ethics statements found")
  }

  # 2: each file check's table, and code_check's own report
  titles <- c(formats = "Show all formats", data_readable = "Show the files",
              name_characters = "Show the names", name_conventions = "Show the names and folders",
              junk = "Show the files", personal_data = "Show what was found", codebook_columns = "Show the columns",
              missing_values = "Show the tables")
  for (id in names(titles)) {
    tbl <- file_checks[[id]]$table
    if (!is.null(tbl) && nrow(tbl)) fold(id, metacheck::scroll_table(tbl, maxrows = nrow(tbl)), titles[[id]])
  }
  code_report <- file_checks$code_check$note
  if (length(code_report) > 2) fold("code_check", code_report[-c(1, length(code_report))], "Show code_check's report")
  run_report <- file_checks$code_runs$note
  if (length(run_report) > 2) fold("code_runs", run_report[-c(1, length(run_report))], "Show the output of each script")

  # units of numeric columns, for information
  units <- file_checks$missing_values$units
  if (!is.null(units)) {
    checks <- rbind(checks, data.frame(status = "info", title = "Units of numeric columns",
                                       section = .tdc_section_titles[["2"]], found = units$found,
                                       todo = units$todo, why = "", note = "", details = "", details_title = ""))
  }
  rownames(checks) <- NULL
  checks
}

# Where the package came from: fetched from a link, handed in as a zip, or
# a folder inside the folder given.
.tdc_package_note <- function(pkg) {
  fetched <- pkg$fetched
  c(
    if (!is.null(fetched)) sprintf("The package was fetched from %s: %d file%s.%s", fetched$url, fetched$n_files,
                                   if (fetched$n_files == 1) "" else "s",
                                   if (length(fetched$not_fetched)) sprintf(" Not fetched (too large, or not available): %s.", .tdc_list(paste0("`", fetched$not_fetched, "`"), 5)) else ""),
    if (isTRUE(pkg$zipped)) "The package was handed in as a zip; it was unpacked to check its contents.",
    if (length(pkg$wrapper)) sprintf("The package is the folder `%s/`.", paste(pkg$wrapper, collapse = "/"))
  )
}

.tdc_report <- function(items, components, tree, pkg, rd, people, file_checks) {
  items$result <- unname(.tdc_result_words[items$status])
  items$result[items$item == "human_participants"] <- .tdc_people_result(people)
  summary_tbl <- data.frame(Check = items$title, Result = items$result)
  package_note <- .tdc_package_note(pkg)
  comp_tbl <- data.frame(Group = components$group, Component = components$title,
                         `Found?` = components$found, Files = components$files,
                         Note = components$note, check.names = FALSE)
  comp_tbl$Group[duplicated(comp_tbl$Group)] <- ""

  s11 <- items[items$item %in% c("components", "folder_depth", "folder_layout",
                                 "file_classification", "package_size"), , drop = FALSE]
  c(
    "This report checks the data package against TU/e's archiving guidelines, the way the RAPS data managers do. Each check below says what is fine and what to fix or improve before archiving.",
    package_note,
    metacheck::scroll_table(summary_tbl, maxrows = nrow(summary_tbl)),
    "#### 1.1 Components and structure",
    "**Package components** (TU/e's archiving guidelines, Step 1; not every component applies to every study):",
    metacheck::scroll_table(comp_tbl, maxrows = nrow(comp_tbl)),
    .tdc_report_items(s11),
    tree,
    .tdc_report_readme(items, rd),
    .tdc_report_people(items, people),
    .tdc_report_files(items, file_checks)
  )
}
