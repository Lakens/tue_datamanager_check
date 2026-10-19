# tue_datamanager_check

A [metacheck](https://github.com/scienceverse/metacheck) module that checks a research data package the way TU/e's data managers do before it is archived through RAPS. It follows TU/e's archiving guidelines in Research Cockpit and the RDLC document "Using Metacheck for data package review and paper analysis", and writes a report for the researcher. The report shows what is fine and what to fix or improve, in the sections of that document:

- **1.1 Components and structure:** which of the package components TU/e lists are there, folder depth, how the folders are organised, file classification, package size, and the full file tree.
- **1.2 README:** whether there is a README at the top, whether it follows the [TU/e README template](https://doi.org/10.5281/zenodo.19692008) line by line, template text left in, contacts, the file list, ORCIDs and CRediT roles. It also writes a revised README in the template's structure, as a suggestion.
- **1.3 Research with human participants or personal data:** whether people took part (yes, no or unclear, with the sentence that shows it), and then whether the ERB approval letter and the signed informed consent forms are there.
- **2 File checks:** file formats against TU/e's preferred formats, spaces and special characters in names, naming conventions, junk files, personal data, columns against the codebook, notations for missing values, metacheck's code check, and zipped packages.

Each check gives one of five results: *to fix*, *to improve*, *decide by hand*, *fine* or *not applicable*. The report's light is red when anything is to fix, yellow when anything is to improve or to decide by hand, and green otherwise. Every decision behind the checks is written down in [SPECIFICATION.md](SPECIFICATION.md).

## What you need

- **R with metacheck 0.4.0.4 or later.** Until that version is released, install metacheck from its development branch:

  ```r
  pak::pkg_install("scienceverse/metacheck@external-modules")
  ```

- **[Ollama](https://ollama.com) with a local model (optional, recommended).** The module uses a language model for three things: checking the README against the template, writing the revised README, and deciding whether people took part. It only ever uses a model running on your own computer through Ollama, and refuses any other model, so the README and the package's documents never leave your computer for this. The module was built and tested with `qwen3.5:4b`, run with a context of 16,384 tokens:

  ```sh
  ollama pull qwen3.5:4b
  OLLAMA_CONTEXT_LENGTH=16384 ollama serve
  ```

  Without a local model, the README is checked by its headings and labels instead, no revised README is written, and whether people took part is decided from the data and the README alone (else it is "decide by hand").

## Install the module

```r
metacheck::module_install("Lakens/tue_datamanager_check")
```

`metacheck::module_update("Lakens/tue_datamanager_check")` gets a newer version later.

## Check a data package

```r
library(metacheck)
llm_model("ollama/qwen3.5:4b")

# a folder: writes my_package_report.html in the working folder
report_repository("path/to/my_package", modules = "tue_datamanager_check")

# the same, as a checklist: what to fix and to improve first, with what was
# found and what to do on separate lines (metacheck 0.4.0.6 or later)
report_repository("path/to/my_package", modules = "tue_datamanager_check",
                  report_type = "checklist")

# the results without a report, for a folder, a zip of one, or a link to an online repository
out <- module_run(test_paper(), "tue_datamanager_check", local_path = "path/to/my_package.zip")
out <- module_run(test_paper(), "tue_datamanager_check", local_path = "https://github.com/user/repo")
out$table          # one row per check: section, title, result, explanation
out$traffic_light
```

Arguments, passed with `args = list(tue_datamanager_check = list(...))` in `report_repository()`:

- `human_participants`: `TRUE` or `FALSE` when you already know whether people took part or personal data was used; the default `NA` lets the module decide. Personal data found in the data always counts as people having taken part, even with `FALSE`.
- `readme_revised_dir`: the folder the revised README is written to (default: the working folder). It is never written into the package.
- `run_code`: `TRUE` to run the package's R and Python code with metacheck's `reproducibility_check` and report which scripts ran without an error (default `FALSE`). This runs the code on your computer, so only use it for a package you trust, or in a sandbox: `run_args` passes further arguments to `reproducibility_check`, such as `list(sandbox = "docker", timeout = 300)`. The web page runs the code in the SURF server's Docker sandbox when its tick box is ticked (the default).

## Use it online, without installing anything

The folder [`web/`](web) holds a web page that sends a package to metacheck's server on SURF Research Cloud and shows the report. The server must have this module installed (its `metacheck_modules` setting). A data manager drops a package folder or a .zip on the page, or pastes a link to an online repository (GitHub, OSF, Zenodo, Dataverse, ...), says whether people took part (or lets the check decide), and gets the report, to open or save, after a few minutes. The browser packs a folder into a .zip itself, hidden files included, because the check looks for those too; for a link, the module fetches the repository's public files on the server (at most 100 MB per file, 500 MB in all). The page looks like the metacheck website on SURF, whose style sheet and font it uses.

The same page runs in three ways:

- **On any website:** copy `web/index.html` and `web/www/` to any web host, such as GitHub Pages.
- **As a Shiny app:** `shiny::runApp("web")`, or copy `web/` into a Shiny Server's app folder. The app only serves the page.
- **Locally:** open `web/index.html` in a browser.

In all three, the visitor's browser talks to the SURF server directly; the server allows pages from any website. The server's address is in `web/www/config.js`, and a page address ending in `?api=<address>` overrides it, for example to use a test server. When the page opens, it asks the server whether the check is installed and says so if it is not.

## What leaves your computer

When you check a local folder or zip:

- Nothing about the package goes online, with one exception. To decide whether people took part, the module may turn up to 3 scientific documents of the package into text: the manuscript, documents in a folder of scientific documents, the methodology, and PDFs at the top of the package. It does so only when the data and the README have not already answered the question. A PDF is sent to TU/e's GROBID server (`https://grobid.hti.ieis.tue.nl`), and to metacheck's API on SURF Research Cloud only when GROBID cannot convert it (on the SURF server itself, to its own bibr). Pass `human_participants = TRUE` or `FALSE` to skip this.
- The language model runs on your own computer (see above).
- metacheck's modules read only that folder: `repo_check` runs with `local_only = TRUE`, so no links are followed, and the other modules read the local copies it lists, so nothing is downloaded.

## The rules

The rules are files in [`data/`](data), so they can be changed without changing the code:

- `formats.json`: TU/e's preferred and non-preferred file formats, with what to convert to;
- `components_tue.json`: the package components of TU/e's guidelines, and how files and folders are recognised as each;
- `layouts_tue.json`: the folder layouts the guidelines describe;
- `readme_tue.json` and `README_Template_TUe.txt`: the TU/e README template and how its lines are recognised;
- `personal_data.json`: patterns for BSN and phone numbers, and names of pseudonymisation key files;
- `severity.json`: how serious each finding is (fail, warn or manual), and the limits (folder depth, number of contacts, ...).

## Tests

From the repository's top folder, with metacheck loaded:

```r
testthat::test_dir("tests/testthat")
```

The tests use no language model, GROBID server or SURF API.

## Licence and credits

Daniel Lakens wrote this module. It is licensed under the [GNU Affero General Public License v3.0](LICENSE).

- The files `formats.json`, `components_tue.json`, `layouts_tue.json` and `readme_tue.json` in `data/` are adapted from the TU/e rule pack for pytacheck (`scienceverse/metacheck-tue`) by Jakub Werner, licensed AGPL-3.0. The changes are listed in [SPECIFICATION.md](SPECIFICATION.md).
- `data/README_Template_TUe.txt` is the TU/e README template, unchanged, from [Zenodo](https://doi.org/10.5281/zenodo.19692008), licensed [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/); see `data/README_Template_TUe.source.txt`.
- `web/www/jszip.min.js` is [JSZip](https://stuk.github.io/jszip/) 3.10.1 by Stuart Knightley, licensed MIT or GPLv3 (its notice is at the top of the file).
- `web/www/site.css` is the style sheet of the metacheck website on SURF (`scienceverse/surf-research-cloud`, `site/`), with the font paths made relative; `web/www/fonts/` holds the Cabin font, licensed under the SIL Open Font License 1.1 (`OFL-Cabin.txt`).
- `web/www/images/logo.png` is metacheck's logo; `web/www/images/tuelogo.png` is the logo of Eindhoven University of Technology (TU/e), used for TU/e's own data managers' check.
