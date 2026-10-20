test_that("the rule files load", {
  expect_named(rules, c("formats", "open_formats", "layouts", "components", "readme",
                        "personal", "severity", "limits", "dir"))
  expect_true(all(rules$severity %in% tdc$.tdc_status_levels))
  expect_true(all(vapply(rules$formats, function(f) f$level %in% c("preferred", "non-preferred"), logical(1))))
  ids <- vapply(rules$components, function(c) c$id, character(1))
  expect_false(any(duplicated(ids)))
  expect_true(all(c("manuscript", "methodology", "ethics", "consent", "codebook", "raw_data") %in% ids))
  expect_true(all(c("max_depth", "min_layout_coverage", "flat_max_files", "min_contacts") %in% names(rules$limits)))
})

test_that("every severity the code asks for is in severity.json", {
  code <- unlist(lapply(c(list.files(file.path(mod_dir, "R"), "\\.R$", full.names = TRUE), mod_file),
                        readLines, warn = FALSE))
  used <- unique(unlist(regmatches(code, gregexpr('(?<=\\.tdc_sev\\(rules, ")[a-z0-9-]+', code, perl = TRUE))))
  expect_gt(length(used), 30)
  expect_setequal(used, names(rules$severity))
  expect_error(tdc$.tdc_sev(rules, "no-such-finding"), "No severity")
})

test_that(".tdc_item keeps what was found and what to do apart", {
  it <- tdc$.tdc_item("x", "2", "Title", "warn", c("Found one.", "Found two."), "Do this.", NULL, NA)
  expect_equal(it$found, "Found one. Found two.")
  expect_equal(it$todo, "Do this.")
  expect_equal(it$why, "")
  expect_equal(it$note, "")
  expect_equal(it$detail, "Found one. Found two. Do this.")
  # pieces that start with "- " become a list after the sentences
  it <- tdc$.tdc_item("x", "2", "Title", "warn", c("Three files.", "- a", "- b"), c("- Do a.", "- Do b."))
  expect_equal(it$found, "Three files.\n\n- a\n- b")
  expect_equal(it$todo, "- Do a.\n- Do b.")
  expect_equal(it$detail, "Three files.\n\n- a\n- b\n\n- Do a.\n- Do b.")
})

test_that(".tdc_check_failed", {
  row <- tdc$.tdc_check_failed("formats", "2", "File formats", simpleError("bad\n input."))
  expect_equal(row$status, "manual")
  expect_equal(row$found, "This check could not be completed, because of an error: bad input.")
  expect_match(row$todo, "Check this by hand")
  row2 <- tdc$.tdc_check_failed("code_check", "2", "Code check", simpleError("x"), what = "metacheck's code_check")
  expect_match(row2$found, "^metacheck's code_check could not be completed")
})

test_that(".tdc_worst", {
  expect_equal(tdc$.tdc_worst(c("pass", "warn", "manual")), "warn")
  expect_equal(tdc$.tdc_worst(c("pass", "fail", NA)), "fail")
  expect_equal(tdc$.tdc_worst(character(0)), "pass")
  expect_equal(tdc$.tdc_worst("na"), "na")
})

test_that(".tdc_norm_name", {
  expect_equal(tdc$.tdc_norm_name("3 Code & Script"), "code and script")
  expect_equal(tdc$.tdc_norm_name("01_raw-data"), "raw data")
  expect_equal(tdc$.tdc_norm_name("Fig.1"), "fig 1")
})

test_that(".tdc_list", {
  expect_equal(tdc$.tdc_list(c("a", "b")), "a, b")
  expect_equal(tdc$.tdc_list(letters[1:7], 3), "a, b, c and 4 more")
  expect_equal(tdc$.tdc_list(c("a", NA, "a")), "a")
})
