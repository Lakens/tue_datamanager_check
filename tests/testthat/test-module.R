# The whole module on a small package, without an LLM, GROBID or the SURF
# API (the data steward answers the participants question).

test_that("tue_datamanager_check runs on a package", {
  no_llm()
  dir <- make_package(list(
    "README.md" = c("# Reading study", "", "Data and code of the study.",
                    "", "Contacts: a@tue.nl"),
    "raw_data/raw data.csv" = c("pid,score", "1,3", "2,", "3,NA"),
    "code/analysis.R" = "x <- read.csv('raw_data/raw data.csv')",
    ".DS_Store" = ""))
  revised <- withr::local_tempdir()
  out <- metacheck::module_run(metacheck::test_paper(), mod_file, local_path = dir,
                               human_participants = FALSE, readme_revised_dir = revised)

  expect_true(all(c("components", "folder_depth", "readme_exists", "readme_template", "human_participants",
                    "ethics", "consent", "formats", "name_characters", "name_conventions", "junk",
                    "personal_data", "codebook_columns", "missing_values", "code_check", "code_runs", "zipped")
                  %in% out$table$item))
  expect_true(all(out$table$status %in% tdc$.tdc_status_levels))
  st <- stats::setNames(out$table$status, out$table$item)
  expect_equal(st[["name_characters"]], "fail")   # "raw data.csv"
  expect_equal(st[["junk"]], "fail")              # .DS_Store
  expect_equal(st[["missing_values"]], "warn")    # empty cells and NA
  expect_equal(st[["ethics"]], "na")              # no people (the data steward)
  expect_equal(st[["zipped"]], "pass")
  expect_equal(st[["code_runs"]], "na")           # run_code = FALSE
  expect_equal(out$traffic_light, "red")
  expect_equal(out$human_participants$answer, "no")
  # without a local model, no revised README is written, and the report says why
  expect_null(out$readme_revised)
  expect_length(list.files(revised), 0)

  r <- out$report
  for (h in c("#### 1.1 Components and structure", "#### 1.2 README",
              "#### 1.3 Research with human participants or personal data", "#### 2 File checks")) {
    expect_true(h %in% r, info = h)
  }
  expect_true(any(grepl("not a local Ollama model", r)))
  expect_equal(out$summary_table$tdc_fail, sum(out$table$status == "fail"))

  # no codebook and no methodology: recommended components
  expect_equal(st[["components"]], "warn")

  # the checks for metacheck's checklist report: every check, what was found
  # and what to do apart, and the tables folded under their check
  ch <- out$checks
  expect_true(all(out$table$title %in% ch$title))
  expect_true(all(ch$status %in% c("fix", "improve", "decide", "info", "fine", "na")))
  expect_equal(ch$status[ch$title == "No junk or temporary files"], "fix")
  expect_true(all(nzchar(ch$found)))
  expect_true(all(nzchar(ch$todo[ch$status %in% c("fix", "improve", "decide")])))
  expect_match(ch$details[ch$title == "File classification"], "raw data.csv", fixed = TRUE)
  expect_match(ch$found[ch$title == "Human participants or personal data"], "^No, from the data steward")
  expect_true(file.exists(out$logo))
  expect_match(out$intro[[1]], "TU/e's archiving guidelines")
})

test_that("a data file with columns that need an R package gives a warning (#4, item 8a)", {
  no_llm()
  dir <- make_package(list("README.md" = c("# Study", "", "Data.")))
  # a made-up package stands in for lubridate, whose periods need it to be read
  span <- asS4(structure(as.numeric(1:5), class = structure("Span", package = "notinstalledpkg")))
  saveRDS(structure(list(id = 1:5, span = span), class = "data.frame", row.names = 1:5),
          file.path(dir, "trials.rds"))
  out <- metacheck::module_run(metacheck::test_paper(), mod_file, local_path = dir,
                               human_participants = FALSE, readme_revised_dir = withr::local_tempdir())
  row <- out$table[out$table$item == "data_readable", ]
  expect_equal(row$status, "warn")
  expect_match(row$found, "column `span`: Span, from the R package notinstalledpkg", fixed = TRUE)
})
