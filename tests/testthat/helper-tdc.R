# Helpers for the tests of tue_datamanager_check.
#
# Run the tests from the repository's top folder, with metacheck 0.4.0.4 or
# later loaded:
#   testthat::test_dir("tests/testthat")
# The tests never call a language model, GROBID or the SURF API: the model is
# set to a name that is not a local Ollama model, which the module refuses to
# call, and no test asks the module to read a scientific document.

mod_dir <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
mod_file <- file.path(mod_dir, "tue_datamanager_check.R")

# The module's helper functions, as the module loads them.
tdc <- new.env()
for (f in list.files(file.path(mod_dir, "R"), "\\.R$", full.names = TRUE)) {
  sys.source(f, envir = tdc)
}
rules <- tdc$.tdc_rules(mod_dir)

# A model name the module never calls (not a local Ollama model).
no_llm <- function(env = parent.frame()) {
  withr::local_options(metacheck.llm.model = "groq/not-a-local-model", .local_envir = env)
}

# A package folder in a temporary folder, with files written from a named
# list (path = lines of text); returns the folder.
make_package <- function(contents, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  for (p in names(contents)) {
    dir.create(dirname(file.path(dir, p)), recursive = TRUE, showWarnings = FALSE)
    writeLines(contents[[p]], file.path(dir, p))
  }
  dir
}

# The module's file table for a folder, as the module makes it from
# repo_check.
package_files <- function(dir) {
  rc <- metacheck::module_run(metacheck::test_paper(), "repo_check",
                              local_path = dir, local_only = TRUE)
  tdc$.tdc_files(rc$table)
}
