test_that(".tdc_package: a folder", {
  dir <- make_package(list("README.md" = "# x", "data/a.csv" = "a\n1"))
  pkg <- tdc$.tdc_package(dir)
  expect_equal(pkg$path, normalizePath(dir, winslash = "/"))
  expect_false(pkg$zipped)
  expect_null(pkg$wrapper)
  expect_null(tdc$.tdc_package(NULL)$path)
  expect_error(tdc$.tdc_package(file.path(dir, "nothing_here")), "No folder or file")
  expect_error(tdc$.tdc_package(file.path(dir, "README.md")), "folder, or a zip")
})

test_that(".tdc_package: a wrapper folder, its junk, and a zip", {
  dir <- make_package(list("my_package/README.md" = "# x", "my_package/data/a.csv" = "a\n1",
                           ".DS_Store" = "", "__MACOSX/._README.md" = ""))
  pkg <- tdc$.tdc_package(dir)
  expect_equal(pkg$wrapper, "my_package")
  expect_equal(basename(pkg$path), "my_package")
  expect_setequal(pkg$outer_junk, c(".DS_Store", "__MACOSX/"))

  zip_file <- file.path(withr::local_tempdir(), "my_package.zip")
  withr::with_dir(dir, utils::zip(zip_file, c("my_package", ".DS_Store", "__MACOSX"), flags = "-qr"))
  z <- tdc$.tdc_package(zip_file)
  expect_true(z$zipped)
  expect_equal(z$wrapper, "my_package")
  expect_true(file.exists(file.path(z$path, "data", "a.csv")))
  expect_setequal(z$outer_junk, c(".DS_Store", "__MACOSX/"))
})

test_that(".tdc_files and .tdc_folders", {
  dir <- make_package(list("README.md" = "# x", "data/raw/a.csv" = "a\n1", "code/run.R" = "x <- 1"))
  dir.create(file.path(dir, "empty"))
  files <- package_files(dir)
  expect_setequal(files$path, c("README.md", "data/raw/a.csv", "code/run.R"))
  expect_equal(files$ext[files$path == "code/run.R"], "r")
  expect_setequal(tdc$.tdc_folders(dir, files), c("data", "data/raw", "code", "empty"))
  expect_setequal(tdc$.tdc_folders(NULL, files), c("data", "data/raw", "code"))
})
