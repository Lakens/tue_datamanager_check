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
