# Section 2: file checks.

# A minimal file table for checks that only read paths and extensions.
files_of <- function(paths, data_type = "data", location = NA_character_) {
  data.frame(path = paths, name = basename(paths), ext = tolower(tools::file_ext(paths)),
             data_type = data_type, doc_role = NA_character_, size = 1,
             location = location, repo_url = NA_character_)
}

# -- 2.1 formats -----------------------------------------------------------------

test_that(".tdc_requirement_met reads inside files", {
  d <- withr::local_tempdir()
  f <- function(name, lines) { p <- file.path(d, name); writeLines(lines, p); p }
  expect_true(tdc$.tdc_requirement_met("pdfa", f("a.pdf", c("%PDF-1.4", "<rdf:Description pdfaid:part=\"2\"/>"))))
  expect_false(tdc$.tdc_requirement_met("pdfa", f("b.pdf", c("%PDF-1.4", "%%EOF"))))
  expect_true(tdc$.tdc_requirement_met("mat-v7.3", f("a.mat", "MATLAB 7.3 MAT-file, Platform: GLNXA64")))
  expect_false(tdc$.tdc_requirement_met("mat-v7.3", f("b.mat", "MATLAB 5.0 MAT-file, Platform: PCWIN64")))
  expect_true(tdc$.tdc_requirement_met("svg-no-script", f("a.svg", "<svg><circle r='4'/></svg>")))
  expect_false(tdc$.tdc_requirement_met("svg-no-script", f("b.svg", "<svg onload=\"x()\"></svg>")))
  expect_false(tdc$.tdc_requirement_met("svg-no-script", f("c.svg", "<svg><script>x()</script></svg>")))
  expect_true(tdc$.tdc_requirement_met("html-self-contained", f("a.html", "<img src=\"data:image/png;base64,AA\">")))
  expect_false(tdc$.tdc_requirement_met("html-self-contained", f("b.html", "<script src='https://cdn.x.org/a.js'></script>")))
  expect_true(is.na(tdc$.tdc_requirement_met("pdfa", NULL)))
})

test_that(".tdc_check_formats", {
  d <- withr::local_tempdir()
  writeLines(c("%PDF-1.4", "%%EOF"), file.path(d, "plain.pdf"))
  files <- files_of(c("a.csv", "b.sav", "c.vcd", "d.yml", "e.R", "plain.pdf", "f.zip", "LICENSE"),
                    location = c(NA, NA, NA, NA, NA, file.path(d, "plain.pdf"), NA, NA))
  files$data_type[files$ext == "r"] <- "code"
  out <- tdc$.tdc_check_formats(files, tdc$.tdc_zip_members(files), rules)
  st <- stats::setNames(out$findings$status, out$findings$path)
  expect_equal(st[["a.csv"]], "pass")
  expect_equal(st[["b.sav"]], "warn")       # non-preferred
  expect_equal(st[["c.vcd"]], "manual")     # not on TU/e's list
  expect_equal(st[["d.yml"]], "pass")       # open plain-text format
  expect_equal(st[["e.R"]], "pass")         # code
  expect_equal(st[["plain.pdf"]], "warn")   # not PDF/A
  expect_false(any(c("f.zip", "LICENSE") %in% names(st)))  # archives and no extension: skipped
  expect_equal(out$item$status, "warn")
  expect_true(all(c("Extension", "Format", "Files", "Result", "What to do") %in% names(out$table)))
})

test_that(".tdc_zip_members lists and checks files inside a zip", {
  d <- withr::local_tempdir()
  src <- withr::local_tempdir()
  writeLines("old", file.path(src, "notes.doc"))
  writeLines("<svg><script>x()</script></svg>", file.path(src, "fig.svg"))
  writeLines("", file.path(src, ".DS_Store"))
  withr::with_dir(src, utils::zip(file.path(d, "bundle.zip"), c("notes.doc", "fig.svg", ".DS_Store"), flags = "-q"))
  files <- files_of("bundle.zip", data_type = "unknown", location = file.path(d, "bundle.zip"))
  m <- tdc$.tdc_zip_members(files)
  expect_setequal(m$path, c("bundle.zip/notes.doc", "bundle.zip/fig.svg"))
  out <- tdc$.tdc_check_formats(files, m, rules)
  st <- stats::setNames(out$findings$status, out$findings$path)
  expect_equal(st[["bundle.zip/notes.doc"]], "warn")
  expect_equal(st[["bundle.zip/fig.svg"]], "warn")   # unpacked and read: it has a script
})

# -- 2.2 and 2.3 names -------------------------------------------------------------

test_that(".tdc_check_name_characters", {
  files <- files_of(c("Raw data/a b.csv", "Raw data/c&d.csv", "Raw data/résumé.txt",
                      "Raw data/results (final).csv", "Raw data/fine_name-1.csv"))
  out <- tdc$.tdc_check_name_characters(files, "Raw data", rules)
  st <- stats::setNames(out$findings$status, out$findings$path)
  expect_equal(st[["Raw data/"]], "fail")                  # a folder, reported once
  expect_equal(st[["Raw data/a b.csv"]], "fail")           # a space
  expect_equal(st[["Raw data/c&d.csv"]], "fail")           # a forbidden character
  expect_equal(st[["Raw data/résumé.txt"]], "warn")  # an accented letter
  expect_match(out$findings$problem[out$findings$path == "Raw data/results (final).csv"], "a space; other special characters")
  expect_false("Raw data/fine_name-1.csv" %in% names(st))
  expect_equal(out$item$status, "fail")
  ok <- tdc$.tdc_check_name_characters(files_of("data/a.csv"), "data", rules)
  expect_equal(ok$item$status, "pass")
})

test_that(".tdc_name_styles", {
  expect_equal(tdc$.tdc_name_styles("raw_data.csv"), "snake_case")
  expect_equal(tdc$.tdc_name_styles("raw-data.csv"), "kebab-case")
  expect_equal(tdc$.tdc_name_styles("rawData.csv"), "camelCase/PascalCase")
  expect_equal(tdc$.tdc_name_styles("Raw_data-YYYYMMDD-v0.2-Title.csv"), c("snake_case", "kebab-case"))
  # dates, a leading number and leading or trailing underscores do not count
  expect_length(tdc$.tdc_name_styles("session_01-05-2024.csv"), 0)
  expect_length(tdc$.tdc_name_styles("session_2024-05-03.csv"), 0)
  expect_length(tdc$.tdc_name_styles("__init__.py"), 0)
  expect_equal(tdc$.tdc_name_styles("01_raw-data.csv"), "kebab-case")
})

test_that(".tdc_check_name_conventions", {
  files <- files_of(c("data/s_20240501.csv", "data/s_01-05-2024.csv", "data/s_2024-05-03.csv",
                      "data/Raw_data-YYYYMMDD-v1.csv", "data/pp1.csv", "data/pp2.csv", "data/pp10.csv",
                      "mixed/raw_data.csv", "mixed/rawData.csv"))
  out <- tdc$.tdc_check_name_conventions(files, c("data", "mixed"), rules)
  p <- out$findings$problem
  names(p) <- out$findings$path
  expect_match(p[["data/s_20240501.csv"]], "written as YYYYMMDD")
  expect_match(p[["data/s_01-05-2024.csv"]], "written as DD-MM-YYYY")
  expect_false("data/s_2024-05-03.csv" %in% names(p))
  expect_match(p[["data/Raw_data-YYYYMMDD-v1.csv"]], "date placeholder `YYYYMMDD`")
  expect_true(all(c("data/pp1.csv", "data/pp2.csv") %in% names(p)))   # no leading zeros
  expect_false("data/pp10.csv" %in% names(p))
  expect_match(p[["mixed/"]], "names mix snake_case, camelCase/PascalCase")
  expect_true(all(out$findings$status == "warn"))
})

# -- 2.4 junk ------------------------------------------------------------------------

test_that(".tdc_check_junk", {
  useless <- data.frame(file_path = c("a/.DS_Store", ".git/config"), kind = c("operating system", "version control"))
  out <- tdc$.tdc_check_junk(useless, "__MACOSX/", rules)
  st <- stats::setNames(out$findings$status, out$findings$path)
  expect_equal(st[["a/.DS_Store"]], "fail")
  expect_equal(st[[".git/config"]], "warn")
  expect_equal(st[["__MACOSX/"]], "fail")   # beside the package folder
  expect_equal(out$item$status, "fail")
  expect_equal(tdc$.tdc_check_junk(NULL, character(0), rules)$item$status, "pass")
})

# -- 2.5 personal data ---------------------------------------------------------------

test_that(".tdc_elfproef", {
  expect_true(tdc$.tdc_elfproef("111222333"))
  expect_false(tdc$.tdc_elfproef("123456789"))
  expect_true(tdc$.tdc_elfproef("11222333") == tdc$.tdc_elfproef("011222333"))
  expect_false(tdc$.tdc_elfproef("000000000"))
  expect_false(tdc$.tdc_elfproef("12345"))
})

test_that(".tdc_personal_data", {
  dc <- list(
    findings = data.frame(source_file = "raw.csv", column = "email", check = "Personal info (values)"),
    previews = list("raw.csv" = data.frame(bsn = rep(c("111222333", "123456782"), 5),
                                           phone = rep(c("06-12345678", "+31 40 247 9111"), 5),
                                           score = 1:10)))
  files <- files_of(c("raw.csv", "data/pseudonymisation_key.csv", "data/keyboard.csv"))
  pd <- tdc$.tdc_personal_data(dc, files, rules)
  kinds <- paste(pd$column, pd$kind)
  expect_true(any(grepl("^email ", kinds)))
  expect_true(any(grepl("^bsn .*BSN", kinds)))
  expect_true(any(grepl("^phone .*Phone", kinds)))
  expect_false(any(pd$column == "score"))
  expect_equal(pd$path[pd$kind == "a pseudonymisation key file"], "data/pseudonymisation_key.csv")
  expect_true(all(pd$status == "fail"))
  out <- tdc$.tdc_check_personal_data(pd, 1)
  expect_equal(out$item$status, "fail")
  expect_equal(nrow(out$table), 4)   # one row per column, plus the key file
  expect_equal(tdc$.tdc_check_personal_data(pd[0, ], 1)$item$status, "pass")
})

# -- 2.6 codebook ------------------------------------------------------------------

test_that(".tdc_check_codebook_columns", {
  cb <- list(summary_table = data.frame(codebook_var_n = 2),
             table = data.frame(source_file = "raw.csv", column_name = c("a", "b", "c"),
                                label_status = c("labelled", "unlabelled", "conflicted"),
                                label_source = "codebook.csv"))
  out <- tdc$.tdc_check_codebook_columns(cb, rules)
  expect_equal(out$item$status, "warn")
  expect_equal(out$findings$column_name, "b")
  cb$summary_table$codebook_var_n <- 0
  expect_equal(tdc$.tdc_check_codebook_columns(cb, rules)$item$status, "na")
  expect_equal(tdc$.tdc_check_codebook_columns(NULL, rules)$item$status, "na")
})

# -- 2.7 missing values ----------------------------------------------------------------

test_that(".tdc_missing_notations", {
  expect_equal(tdc$.tdc_missing_notations(c("1", "", "2")), "empty cells")
  expect_setequal(tdc$.tdc_missing_notations(c("1", "NA", "n/a", ".")), c("NA", "N/A", "."))
  expect_setequal(tdc$.tdc_missing_notations(c(1, 2, -999, NA)), c("empty cells", "-999"))
  # a code is no code in a column with real negative values
  expect_equal(tdc$.tdc_missing_notations(c(-1.5, 2, -999)), character(0))
  expect_equal(tdc$.tdc_missing_notations(c("a", "b")), character(0))
})

test_that(".tdc_check_missing_units reads CSV files as written", {
  d <- withr::local_tempdir()
  writeLines(c("id,score,weight_kg,height", "1,3,70,", "2,-999,NA,180", "3,4,80,175"), file.path(d, "raw.csv"))
  files <- files_of("raw.csv", location = file.path(d, "raw.csv"))
  dc <- list(previews = list("raw.csv" = utils::read.csv(file.path(d, "raw.csv"))),
             table = data.frame(source_file = "raw.csv", column_name = c("id", "score", "weight_kg", "height"),
                                representation = "numeric", role = c("identifier", "measure", "measure", "measure"),
                                unit = NA_character_))
  out <- tdc$.tdc_check_missing_units(dc, files, "- height: body height in cm", rules)
  expect_equal(out$item$status, "warn")
  expect_match(out$findings$notations, "empty cells")
  expect_match(out$findings$notations, "-999")
  expect_match(out$findings$notations, "NA")
  # units: weight_kg by its name, height by the README line; score has none
  expect_match(out$note, "`score`")
  expect_false(grepl("`weight_kg`|`height`|`id`", out$note))
})

# -- 2.8 code and zipped ----------------------------------------------------------------

test_that(".tdc_check_code", {
  expect_equal(tdc$.tdc_check_code(NULL, rules)$item$status, "na")
  expect_equal(tdc$.tdc_check_code(list(traffic_light = "na"), rules)$item$status, "na")
  cc <- list(traffic_light = "yellow", report = "#### Code Comments",
             table = data.frame(file_name = c("a.py", "b.py"), file_path = c("code/a.py", "code/b.py"),
                                language = "Python", percentage_comment = c(0, 20),
                                code_abs_path = c(0, 1), absolute_paths = c("", "C:/Users/me/data")),
             version_pin = list(pinned = FALSE))
  out <- tdc$.tdc_check_code(cc, rules)
  expect_equal(out$item$status, "warn")
  expect_match(out$item$found, "2 code files: 2 Python.", fixed = TRUE)
  expect_match(out$item$found, "- 1 file has no comments: `code/a.py`.", fixed = TRUE)
  expect_match(out$item$found, "`C:/Users/me/data` in `code/b.py`", fixed = TRUE)
  expect_match(out$item$todo, "- Replace the absolute paths", fixed = TRUE)
  expect_match(out$item$todo, "requirements.txt", fixed = TRUE)
  expect_false(grepl("renv", out$item$todo))
  expect_true("###### Code Comments" %in% out$note)
  green <- tdc$.tdc_check_code(list(traffic_light = "green"), rules)$item
  expect_equal(green$status, "pass")
  expect_equal(green$found, "No problems were found.")
  expect_equal(green$todo, "")
})

test_that(".tdc_check_code_runs", {
  # not asked to run the code
  expect_equal(tdc$.tdc_check_code_runs(NULL, FALSE, rules)$item$status, "na")
  # asked, but no code
  expect_equal(tdc$.tdc_check_code_runs(NULL, TRUE, rules)$item$status, "na")
  # reproducibility_check failed
  out <- tdc$.tdc_check_code_runs(simpleError("no docker"), TRUE, rules)
  expect_equal(out$item$status, "manual")
  expect_match(out$item$found, "no docker", fixed = TRUE)
  # every script ran
  rp <- list(traffic_light = "green", report = "#### Run",
             run_results = data.frame(file_name = c("a.R", "b.py"), outcome = "ran_ok", error = ""))
  out <- tdc$.tdc_check_code_runs(rp, TRUE, rules)
  expect_equal(out$item$status, "pass")
  expect_match(out$item$found, "2 of 2 scripts were run: 2 without an error", fixed = TRUE)
  expect_equal(out$item$todo, "")
  expect_true("###### Run" %in% out$note)
  # an error and a missing input
  rp$run_results <- data.frame(file_name = c("a.R", "b.py", "c.R"),
                               outcome = c("ran_ok", "errored", "skipped_missing_inputs"),
                               error = c("", "Traceback ...\nFileNotFoundError: data.csv", ""))
  out <- tdc$.tdc_check_code_runs(rp, TRUE, rules)
  expect_equal(out$item$status, "warn")
  expect_match(out$item$found, "- `b.py`: \"FileNotFoundError: data.csv\"", fixed = TRUE)
  expect_match(out$item$found, "files they read are not in the package: `c.R`", fixed = TRUE)
  expect_match(out$item$todo, "- Make each script run", fixed = TRUE)
  expect_match(out$item$todo, "- Add the files the scripts read", fixed = TRUE)
})

test_that(".tdc_check_zipped", {
  expect_equal(tdc$.tdc_check_zipped(list(zipped = FALSE), files_of("a.csv"), rules)$item$status, "pass")
  expect_equal(tdc$.tdc_check_zipped(list(zipped = TRUE), files_of("a.csv"), rules)$item$status, "fail")
  out <- tdc$.tdc_check_zipped(list(zipped = FALSE), files_of(c("a.csv", "data/b.7z")), rules)
  expect_equal(out$item$status, "fail")
  expect_match(out$item$detail, "data/b.7z")
})
