# Section 1.3: human participants, the ERB letter and the consent forms.
# No test reads a scientific document, so GROBID and the SURF API are never
# called; the LLM is set to a model the module refuses.

personal_none <- data.frame(path = character(0), column = character(0), kind = character(0))
files0 <- data.frame(path = "README.md", name = "README.md", ext = "md", data_type = "documentation",
                     doc_role = "readme", size = 1, location = NA_character_, repo_url = NA_character_)

people <- function(...) {
  args <- list(human_participants = NA, paper = metacheck::test_paper(),
               readme_text = NULL, component_paths = list(), files = files0,
               personal = personal_none, pkg_info = "", rules = rules)
  given <- list(...)
  args[names(given)] <- given
  do.call(tdc$.tdc_people, args)
}

test_that(".tdc_people: the data steward's answer, and personal data over a no", {
  no_llm()
  expect_equal(people(human_participants = TRUE)$answer, "yes")
  expect_equal(people(human_participants = FALSE)$answer, "no")
  pd <- data.frame(path = "raw.csv", column = "email", kind = "personal information")
  out <- people(human_participants = FALSE, personal = pd)
  expect_equal(out$answer, "yes")
  expect_match(out$item$detail, "overrules")
  expect_equal(out$item$status, "pass")
})

test_that(".tdc_people: data collection described in the README", {
  no_llm()
  out <- people(readme_text = "In this study, 40 students completed an online questionnaire. Participants were recruited via the participant pool and gave informed consent.")
  expect_equal(out$answer, "yes")
  expect_equal(out$source, "the README")
  expect_match(out$quote, "Participants were recruited")
})

test_that(".tdc_people: without a local model and without evidence, unclear", {
  no_llm()
  out <- people(readme_text = "The data of the measurements.")
  expect_equal(out$answer, "unclear")
  expect_equal(out$item$status, "manual")
  expect_match(out$item$detail, "not a local Ollama model")
})

test_that(".tdc_people_ai drops a quote that is not in the texts", {
  no_llm()
  expect_length(tdc$.tdc_people_ai(list("the README" = "x")), 0)   # refused: not a local model
})

test_that(".tdc_science_docs: order, exclusions", {
  f <- data.frame(path = c("manuscript_v2.pdf", "1 Scientific documents/Smith_2024.pdf",
                           "1 Scientific documents/Protocols & methodologies.txt", "poster.pdf",
                           "2 Administrative documents/ERB approval letter.pdf", "data/values.csv"),
                  data_type = c("documentation", "documentation", "documentation", "documentation",
                                "documentation", "data"),
                  doc_role = NA_character_, size = c(10, 50, 1, 30, 5, 1), location = NA_character_,
                  ext = c("pdf", "pdf", "txt", "pdf", "pdf", "csv"))
  f$name <- basename(f$path)
  paths <- list(manuscript = "manuscript_v2.pdf",
                methodology = "1 Scientific documents/Protocols & methodologies.txt",
                ethics = "2 Administrative documents/ERB approval letter.pdf")
  docs <- tdc$.tdc_science_docs(paths, f)
  expect_equal(docs$path, c("manuscript_v2.pdf", "1 Scientific documents/Smith_2024.pdf",
                            "1 Scientific documents/Protocols & methodologies.txt", "poster.pdf"))
})

test_that(".tdc_check_ethics_docs", {
  f <- data.frame(path = c("admin/ERB letter.pdf", "admin/consent_form_template.pdf", "admin/consent.docx"),
                  ext = c("pdf", "pdf", "docx"))
  st <- function(answer, paths, statements = list()) {
    out <- tdc$.tdc_check_ethics_docs(answer, paths, f, statements, rules)
    stats::setNames(out$status, out$item)
  }
  none <- list(ethics = character(0), consent = character(0))
  expect_equal(st("yes", none), c(ethics = "fail", consent = "fail"))
  expect_equal(st("no", none), c(ethics = "na", consent = "na"))
  expect_equal(st("unclear", none), c(ethics = "manual", consent = "manual"))
  found <- list(ethics = "admin/ERB letter.pdf", consent = "admin/consent.docx")
  expect_equal(st("yes", found), c(ethics = "pass", consent = "warn"))   # consent not PDF
  blank <- list(ethics = "admin/ERB letter.pdf", consent = "admin/consent_form_template.pdf")
  expect_equal(st("yes", blank)[["consent"]], "manual")                   # a blank form
  expect_equal(st("no", blank)[["consent"]], "pass")
  # an approval only mentioned in a text is no letter
  out <- tdc$.tdc_check_ethics_docs("yes", none, f, list("the README" = "Approved by the ERB (ERB2024.1)."), rules)
  expect_equal(out$status[out$item == "ethics"], "fail")
  expect_match(out$detail[out$item == "ethics"], "An approval is mentioned")
})
