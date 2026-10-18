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

.tdc_report <- function(items, components, tree, pkg, rd, people, file_checks) {
  items$result <- unname(.tdc_result_words[items$status])
  items$result[items$item == "human_participants"] <- .tdc_people_result(people)
  summary_tbl <- data.frame(Check = items$title, Result = items$result)
  fetched <- pkg$fetched
  package_note <- c(
    if (!is.null(fetched)) sprintf("The package was fetched from %s: %d file%s.%s", fetched$url, fetched$n_files,
                                   if (fetched$n_files == 1) "" else "s",
                                   if (length(fetched$not_fetched)) sprintf(" Not fetched (too large, or not available): %s.", .tdc_list(paste0("`", fetched$not_fetched, "`"), 5)) else ""),
    if (isTRUE(pkg$zipped)) "The package was handed in as a zip; it was unpacked to check its contents.",
    if (length(pkg$wrapper)) sprintf("The package is the folder `%s/`.", paste(pkg$wrapper, collapse = "/"))
  )
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
