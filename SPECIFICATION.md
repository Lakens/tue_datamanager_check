# tue_datamanager_check: specification

All decisions for the metacheck module that checks a data package the way TU/e's
RAPS data managers (RDLC) do. Decided on 2026-10-18.

Sources:
- **RDLC**: "Using Metacheck for data package review and paper analysis" (TU/e Research Data, RDLC, 16 Oct 2026). Its structure (sections 1.1, 1.2, 1.3 and 2) is the structure of this module and its report.
- **Guidelines**: TU/e archiving guidelines in Research Cockpit: "Step 1: Check what you should archive", "How to write README file", "Step 2: Decide what data should be archived", "Step 3: Make your data package for archiving", and "TU/e De-identification Guidelines for Research data".
- **Template**: TU/e README template, `README_Template_TUe.txt`, Zenodo 10.5281/zenodo.19692008 (CC BY 4.0).
- **Rule files** from the pytacheck pack `scienceverse/metacheck-tue` (formats, layouts, components, README template), with the changes listed below.

Where the guidelines answer a question, the module follows them.

---

## 0. General

**What the module checks:** a local folder (`local_path`), or the repositories a paper links to. If the folder holds only one folder and nothing else (apart from junk files), that inner folder is the package; the report says so ("The package is the folder `my_package_v2/`").

**What it runs:** `repo_check`, `data_check`, `codebook_check` and `code_check`, in that order, with the LLM **off**. The LLM is used only for the module's own three calls: the README template check (1.2), the revised README (1.2), and the judgement on human participants (1.3). A manuscript PDF in the package is turned into text with bibr.

**Results:** every check gives one of *fail*, *warn*, *manual* (a person must decide), *pass* or *not applicable*. The traffic light is red when any check fails, yellow when any check gives a warn or manual, and green otherwise.

**The report:**
- is written to the researcher, because the RDLC wants "to share the review results with researchers as guidance";
- follows the RDLC's sections 1.1, 1.2, 1.3 and 2;
- starts with a summary table, one line per check with its result. The data tree and the revised README have no line. The human-participants line shows its answer instead of pass or fail.

**The checklist report** (2026-10-18): every check keeps what was found (`found`), what to do (`todo`), why (`why`) and a note (`note`) apart; `detail` is all four in one text, as the module's own report shows it. The module also returns them as `checks`, which metacheck's checklist report (`report(..., report_type = "checklist")`, metacheck 0.4.0.6 or later) reads: the checks sorted into to fix, to improve, decide by hand, for information, fine and not applicable, with what was found and what to do on separate lines, and the module's tables folded under their check (the components table under "Package components", the data tree under "File classification", the README per template section and the revised README under "README follows the TU/e template", code_check's report under "Code check"). The units of numeric columns are one check "for information". The report has TU/e's logo next to metacheck's (`logo`) and starts with the module's introduction (`intro`). The web page's server asks for this report.

**Rule files in `data/`:**
- `formats.json`, `layouts_tue.json`, `components_tue.json`, `readme_tue.json` (from the pack, with the changes below)
- `README_Template_TUe.txt` (from Zenodo)
- `severity.json`
- `personal_data.json`

**Changes in metacheck itself:**
- The shared file tree (section 1.1) changes for every module and app that shows it.
- The file classifier gets these fixes:
  - `LICENSE`, `LICENCE` and `COPYING` without an extension → documentation (licence)
  - `.slx`, `.mph` and `.nlogo` → code
  - `.vcd` → data
  - `.pgf` → output
  - `requirements.txt`, `environment.yml`, `renv.lock` and similar package lists → code
- A README is only recognised by English names (not LEESMIJ).

---

## 1.1 Components and structure

### Data tree (snapshot)

The data tree is information only: it has no result and no line in the summary table. It is printed open in the report.

**Code:** one shared function in metacheck (`R/file-tree.R`), used by `data_check`'s report, both repository apps (where it is now shown open) and this module. All trees behave the same.

**Entries:**
- *Local folder:* the folder is listed itself: every file and folder, including hidden files, junk files and empty folders.
- *Repository link:* `repo_check`'s file list plus its `useless_files`. Empty folders cannot be shown; hidden files appear if the repository lists them.
- *Files inside zip archives:* as `data_check` gives them, under the zip's name.
- *Several repositories:* one tree each, headed "Repository: <name>".
- *Junk folders* (`.git/`, `__pycache__/`, …) are one line with the number of files inside, for example `.git/  junk: keep only on purpose (1,274 files)`.

**Order:** within each folder, folders first (empty ones included) and then files, both alphabetical without regard to capitals.

**Each line:** the name, then:
- the classification, with icon;
- the study (when `data_check` grouped the files into studies);
- the first naming problem;
- or a mark: "junk: remove", "junk: keep only on purpose", "empty folder" or "hidden file".

No sizes, and no results of other checks.

**Display:** a legend sentence, then the whole tree as monospaced text in a box about 30 lines high with its own scroll bar.

### Package components

The table follows the guidelines' table (Step 1): six groups, each component shown as found or not found. Every component is optional, a missing one shown as "not found" without a warning, except two that are recommended (decided 2026-10-18): the methodology and the codebook. Without them a package is hard to reuse, so a missing one → **warn**, with the component's advice ("Add a data dictionary or codebook ..."). The summary table has one line, "Package components", with the worst result of all components.

**Recognition:**
- Scientific and administrative documents are taken from the files metacheck classifies as *documentation*, and recognised by name (and folder) among those.
- A scanned document counts only as a PDF.
- Notebooks (`.ipynb`, `.Rmd`, `.qmd`) count only as code.

| Group | Component | Recognised by | Asked format |
|---|---|---|---|
| Scientific documents | Manuscript | name: manuscript (also "maniscript", "manscript"), paper, article, preprint, postprint, publication | PDF / TXT |
| | Methodology, protocols, instruments and measurements (recommended) | name: method(s), methodology, protocol(s), instrument(s), **measurement(s)**, questionnaire(s), interview guide(s), procedure(s), SOP(s); folder: methods, methodology, protocols, instruments, materials; or a README section "Methodological information" | PDF / TXT |
| | Lab journals or computational notebooks | name: lab journal / notebook / book / notes, logbook, notebook | PDF / TXT |
| | Data dictionary or codebook (recommended) | metacheck's codebook role; name: codebook, data dictionary, data legend, variable list; or a README section "Data-specific information" | PDF / TXT |
| Administrative documents | Data management plan | name: data management plan, DMP, gegevensbeheerplan | PDF |
| | Ethical approvals | name or folder: ethic(s), ethiek, ethisch, approval, goedkeuring, ERB, IRB, METC | PDF |
| | Informed consents | name or folder: consent, toestemming, informed | PDF |
| | Agreements and correspondence (e.g. NDA) | name: agreement(s), NDA, non-disclosure, overeenkomst, **correspondence** | PDF |
| Code, script, computational model | Code | metacheck's code classification; extensions `.R .Rmd .qmd .py .ipynb .m .mlx .jl .sps .sas .do .ado .sh .cpp .c .java` and **`.slx .mph .nlogo`** | (format check: section 2) |
| Data (code and README excluded) | Raw data | folder: raw, unprocessed, original, collected (+ data/files/measurements/recordings, optionally numbered); name: raw, unprocessed | (section 2) |
| | Processed data | folder or name: processed, cleaned, digitised, anonymised, pseudonymised, preprocessed, derived | (section 2) |
| | Analysed data | folder: analysed, final data, results, figures, figs, plots, graphs, tables, **output(s)**, Fig.N / Table N; name: analysed, results, fig/table + number | (section 2) |
| Metadata form | Metadata form | (not expected: filled in in the RAPS portal) | XML / JSON |
| README file | README | see 1.2 | TXT / MD |

**Results:**
- A component found in another format than asked → **warn**, naming the asked format.
- *Methodology and codebook:* not found → **warn**.
- *Raw data:* if none is marked but other data exists → **manual** ("is the raw data included under another name?"). If the package holds no data at all → **fail**.

**The table in the report:** Group | Component | Found? | Files (the first 3, and how many) | Note (wrong format, raw data unclear).

### Folder depth (up to 3 levels)

- *Measurement:* the number of folders between the top of the package and a file. A file at a 4th level is too deep.
- *Not counted:* junk folders and files inside zip archives. Empty folders do count.
- *Result:* too deep → **warn** ("We generally recommend").
- *Report text:* "1,062 files lie 4 folder levels deep; TU/e recommends at most 3. The folders with these files: `…/` (90 files), … and 10 more folders." At most 10 folders are listed, each with its number of files.

### Component-level organisation

**What counts:** only the top folders. Files, junk folders and hidden folders do not count. Names are normalised: lower case, without a leading number, "&" read as "and", and "_", "-" and "." read as spaces.

**Accepted layouts:**
- **A. Numbered component folders** (TU/e example 1): scientific documents, administrative documents, code (code / script(s) / software / model(s)), and data (with raw / processed / analysed). The numbers are optional. A data folder is required.
- **B. One folder per figure** (TU/e example 2): Fig.N, Figure N, Fig SN, extended data fig N; Table N is optional. At least one figure folder is required.
- **C. Folders by component:** every top folder is named after a component. Each name may be plural, or have "data", "files" or "code" added:
  - *data:* data, datasets, raw, processed, analysed, original, cleaned, derived, final
  - *code:* code, scripts, software, src, models, notebooks, analysis
  - *documentation:* docs, documentation, documents, methods, protocols, materials, manuscript, paper, supplementary
  - *administrative:* admin, administrative, ethics, consent, agreements
  - *results:* output, results, figures, figs, plots, tables

**When a layout fits:** at least 60% of the top folders belong to it, and its required folder is there. A package without folders and with at most 10 files needs no layout.

**Results:**
- No layout fits → **warn**.
- Data or code files lying loose at the top of a package that has folders → **warn** ("move them into the component folders"). A README, LICENSE and manuscript may lie at the top.

**Report text:**
- When a layout fits, the layout and the top folders.
- When none fits, the top folders that are not components, and TU/e's examples.

### File classification

- metacheck's categories: data (one category), code, documentation (readme, codebook, supplemental, licence), materials (images stay materials), output and unknown.
- Unclassified files → **warn**.
- Report: counts per category, for example "data: 1,840; code: 21; documentation: 9 (readme 2, licence 1, supplemental 6); materials: 3; output: 6; unknown: 21 (.vcd)". Each file's own category is in the tree.

### Package size

A package over 2 TB → **manual**: "consult a RAPS data manager (rdi-lab@tue.nl)".

---

## 1.2 README

### README existence

- A README is a file metacheck gives the role "readme".
- It must lie at the top of the package, as `.txt`, `.md` or `.markdown`, or without an extension. `.rst` does not count.
- Several READMEs at the top are fine if one of them is TXT or MD.

**Results:**
- No README → **warn**. (The README is not optional, but its absence gives a warn.)
- A README only in a subfolder, or only in another format → **warn**.

**Report texts:**
- "`README.md` is at the top of the package."
- "The only README (`docs/README.pdf`) is not at the top of the package and not plain text or Markdown. Put a README.txt or README.md at the top."
- "The package has no README. Add a README.txt or README.md at the top: it is the first file others open to understand the package."

### README content against the TU/e template

The README must follow the template's five sections and their lines. *Optional* means the template itself says "optional", "if applicable" or "if important". The three "Links…" lines are **required**, as in the template.

| Section | Lines |
|---|---|
| 1. General information | project or paper title; summary/short description; authors information: name, ORCID, institution, e-mail; contributor roles (CRediT): name, CRediT role(s); contacts (at least two); *date of data collection; geographic location; funding sources* |
| 2. Sharing/access information | licences/restrictions on reuse; links to publications that cite or use the data; links to other locations of the data; links to related data sets |
| 3. Data & file overview | file naming convention; file list or folder tree (with a description of each data file); file formats; *relationship between files*; additional related data not included |
| 4. Methodological information | methods of collection or generation; methods for processing; instrument or software information; *calibration information* |
| 5. Data-specific information for: [FILENAME] | variable list; missing data codes (repeated for each dataset, folder or file) |

**The AI check (LLM).**
- *It gets:* the README, the template, and the package's top folders and data files.
- *It answers, per line:* "filled", "missing" or "only empty or help text", with a short quote as evidence for "filled", in a fixed answer form (a structured LLM call).
- *It also judges* whether each dataset or data folder has its own section 5.
- *Without an LLM* (none set up), the module falls back to matching headings and labels. The report says which way was used.
- *Results:* every finding (a missing section, a missing line, an empty line) → **warn**, because the template "is a recommendation, and not all fields may apply".
- *Summary line:* "README follows the TU/e template".
- *The report shows* a table per section: Section | Found? | Lines missing | Lines left empty.

**Rule-based checks, without AI, each with its own summary line:**
- *Template help text left in* (`<…>`, `[NAME]`, `[YYYY-MM-DD]`, `[FILENAME]`, TODO, lorem ipsum) → **warn**.
- *Contacts:* fewer than two e-mail addresses on or under the Contacts line → **warn**.
- *File inventory:* the README names fewer than 80% of the top folders and files → **warn**.
- *ORCID:* an ORCID that fails metacheck's `check_orcid()` (form and check digit, no online lookup) → **warn**.
- *CRediT roles:* a role that is not in metacheck's `credit_roles()` → **warn**.
- *Variables:* the README's variable list against the data columns is reported **once**, in section 2, item 6 (columns against the codebook, where the README's variable list counts as a codebook), not here.

### Revised README

- It is made on every run (the LLM is always on for this), in English.
- *The AI gets:* the README, the template text, and everything the module knows about the package (file list, formats, data columns, findings).
- *Its instructions:* rewrite into the template's structure, keep every fact, invent nothing, and write `[TO ADD: …]` where information is missing.
- *A check afterwards, without AI:* every e-mail address, ORCID, DOI and web address in the revised README must also appear in the original README or the package listing. Anything else is listed as "added by the AI: check this".
- *Display:* in section 1.2, in a scroll box, marked "written by AI from the researcher's README; check it before use". Information only, with no summary line.
- *As a file:* written as `<package name>_README_revised.txt` next to the report (the working folder by default, or the folder given in an argument), and returned in the module's output. Never written into the package.

---

## 1.3 Research with human participants / personal data

### People or personal data: yes / no / unclear

**Sources, in this order:**
1. The argument `human_participants = TRUE/FALSE` (the data steward).
2. A paper, when one is given. The AI reads its **full text**, and `ethics_check` reports data collection from people and ethics statements.
3. The package. The AI and `ethics_check` (given the text as `test_paper(text = …)`) read:
   - the README;
   - a manuscript PDF in the package, converted with bibr;
   - file and folder names (questionnaire, survey, interview, consent, participants);
   - what `data_check` found: personal identifiers and demographic columns.

**The answer:**
- *yes*, with a quote as evidence;
- *no*;
- *unclear* → **manual**.

Personal data found in the data always counts as **yes**. The summary line shows the answer itself, for example "Human participants or personal data: yes, from the README: '40 participants completed a questionnaire'". There is no animal research at TU/e, so none is checked.

**How it is built** (decided while building, 2026-10-18):
- The order: the argument `TRUE`; then personal data in the data (`data_check`'s "Personal info (values)" and "Personal info (column name)" findings, and its demographic columns), always *yes*, also when the argument is `FALSE` (the report says it overrules that answer); then the argument `FALSE`; then data collection from people that `ethics_check` finds in the paper, the README or the manuscript, *yes* with its sentence as the quote; then the AI. The first source that answers decides.
- The package's scientific documents are read only when the paper given has no text and nothing before has answered *yes*. At most 3 are read, in this order, the largest first within each: the manuscript (by name, as in the components table), documents in a folder of scientific documents ("Scientific documents", "manuscripts", "papers", "publications", "articles", "preprints"), the methodology component, and documentation PDFs at the top of the package. Documents of another component (ERB letter, consent forms, data management plan, codebook, lab journal, agreements) are left out. Any further documents are named in the report as not read. The components table itself is unchanged.
- A PDF is converted with TU/e's GROBID server (`https://grobid.hti.ieis.tue.nl`, `convert(method = "grobid")`), and with the SURF API when GROBID fails; on a paper new to them, GROBID took 5.8 s and SURF 50 s (2026-10-18). On a server with its own bibr (`BIBR_API_URL` set, as on the SURF server itself), `convert_bibr()` picks that bibr instead, so a job on the SURF server never waits for another job on the same server. A TXT document is read as it is; a DOCX document is not read, and the report says so.
- The AI gets the README, and of the paper or each document its abstract and the paragraphs that mention people (participants, survey, consent, ...), at most 20,000 characters together, since the local model holds 16,384 tokens; and the package description with the file names that mention people.
- The AI's quote must be in the texts, word for word (ignoring capitals, spaces and punctuation); the source shown is the text it is in. A *yes* without such a quote becomes *unclear*. Without a local model the answer is *unclear*, unless an earlier source answered.
- The ethics statements `ethics_check` finds in the paper and README are always listed in section 1.3, and an approval mentioned while the letter is missing is named in the ERB line.

### ERB approval letter and signed informed consent forms

**Recognition:** documentation files (as in the components table) whose name or folder matches ethics or consent. The asked format is PDF; another format → **warn**.

| People or personal data? | Found | Not found |
|---|---|---|
| yes | pass (files listed) | **fail**: "Add the ERB approval letter (PDF)" / "Add the signed informed consent forms (PDF)" |
| no | pass | not applicable |
| unclear | pass | **manual**: "Check whether people took part or personal data was used; if so, add …" |

**Special cases:**
- An approval that is only mentioned in the README (for example an approval number) is still a **fail**: the letter must be in the package.
- A consent file whose name says template, blank, empty or example → **manual**: "this looks like a blank form; add the signed forms".
- Files are not read; recognition is by name and folder only.

The summary table has two lines: "Ethical approval (ERB letter)" and "Signed informed consent forms".

---

## 2. File checks

### 1. Non-preferred and proprietary formats

Each file's extension is looked up in `formats.json`, ignoring capitals. Files inside zips are checked too.

**Extensions:**
- TU/e's table → as in the table.
- Open formats the table does not mention (code, `.md`, `.tsv`, `.htm`, `.geojson`, `.docx`, `.pptx`, `.xlsx`, `.xlsm`) → pass.
- Binary R, statistics-package and Python data files (`.rds`, `.rda`, `.sav`, `.por`, `.dta`, `.sas7bdat`, `.pkl`, `.pickle`, `.npy`, `.npz`), Blender and Atlas.ti files → **warn** ("other binary files").
- On neither list (for example `.vcd`, `.db`) → **manual**.
- Files without an extension are skipped.

**Read inside the file** (local or downloaded files only; otherwise "not checked: file not downloaded"):
- a PDF that is not PDF/A (no PDF/A identification in its metadata) → **warn**;
- a `.mat` file older than version 7.3 (from its first bytes) → **warn**;
- an SVG with scripts (`<script`, `javascript:`, event attributes) → **warn**;
- an HTML file with external links (`href` or `src` pointing to `http(s)://`) → **warn**.

"3D PDF" cannot be recognised and is not checked.

**Report:** a table per extension (Extension | Files | Result | What to do), with the first 3 file names per row.

**How it is built** (2026-10-18):
- The files inside a local zip are listed by the module itself (`utils::unzip(list = TRUE)`; `repo_check` lists a local zip as one file). A member is unpacked into a temporary folder only when its content must be checked (PDF, MAT, SVG, HTML).
- Archives themselves (`.zip`, `.tar`, `.gz`, `.tgz`, `.bz2`, `.xz`, `.7z`, `.rar`) are not judged as a format: their files are, and the zipped-package check covers the archive.
- Added to `formats.json`: HTML and HTM require "no links to files online" (`html-self-contained`), and a list `open_formats` of plain-text formats TU/e's table does not mention (`.yml`, `.yaml`, `.toml`, `.ini`, `.cfg`, `.conf`, `.log`, `.bib`, `.tex`, `.sty`, `.cls`, `.css`, `.cff`, `.csl`, `.lock`, `.Rproj`), which pass like code.
- PDF/A is recognised by `pdfaid:part` in the file (the whole file up to 50 MB, else its first and last 25 MB); MATLAB 7.3 by the header "MATLAB 7.3 MAT-file".

### 2. Forbidden and special characters, spaces

- A space, or one of `~ ! @ # $ % ^ & * : ?`, in a file or folder name → **fail** (the guidelines say "Do not use").
- Other characters metacheck's naming check flags (for example accented letters) → **warn**.

### 3. Naming conventions

- Dates not written as YYYY-MM-DD → **warn**.
- Numbered files without leading zeros → **warn**, from metacheck's naming check.
- Mixing snake_case, kebab-case and PascalCase/camelCase within one folder → **warn**.

**How 2 and 3 are built** (2026-10-18):
- Every file name and every folder name is checked once (a folder with a space is one finding, not one per file in it). Junk and hidden folders are left out; they belong to the junk check.
- Dates recognised: `20240501` (YYYYMMDD), `2024_05_01` and `2024.05.01`, `01-05-2024` (DD-MM-YYYY, also with `_` or `.`), and the placeholders `YYYYMMDD`, `DDMMYYYY`, `MMDDYYYY`, `YYYY_MM_DD`, `DD-MM-YYYY` and `MM-DD-YYYY` that templates use.
- Naming styles: a name uses snake_case when it has `_`, kebab-case when it has `-`, camelCase/PascalCase when a capital follows a small letter. A leading number (`01_`), leading and trailing underscores (`__init__.py`) and dates do not count. A folder whose files and subfolders together use two or more styles gets a warn; so does a single name that uses two (`Raw_data-YYYYMMDD-v0.2-Title.csv`).

### 4. Junk and temporary files

- From `repo_check`'s `useless_files`.
- Problem kinds → **fail**; version control (`.git`, `.svn`, `.hg`) and editor settings (`.idea`, `.vscode`) → **warn**.
- Junk beside the package folder (when the package is one folder inside what was handed in, such as `__MACOSX/` and `.DS_Store` at the top of a zip) lies outside what `repo_check` lists; the module records it while finding the package folder and reports it too.

### 5. Personal data in tabular data

- `data_check`'s findings: values, column names, free text and coordinates.
- Added by the module:
  - BSN numbers (8–9 digits passing the 11-test, in at least half of a column's values);
  - full phone numbers (Dutch and international, in at least half of a column's values);
  - TU/e student and employee numbers, once their format is known (asked in the email).
- Any personal data found → **fail**.
- **Pseudonymisation key files** (names such as pseudonymisation key, linking table, koppeltabel, sleutel) → **fail**: the de-identification guideline says to keep them encrypted and delete them.

### 6. Columns against the codebook

- *With a codebook or a README variable list:* for every table, each column whose name is not described in it → **warn**. The report lists them per file, for example "`raw data 2024.csv`: `score`, `height_cm` are not described". This uses `codebook_check`'s matching.
- *Without a codebook:* not applicable, since the RDLC says "if any". Whether a codebook exists is the "Data dictionary or codebook" row of the components table.

### 7. Missing values and units

- *Missing values:* a table that uses more than one notation for missing values (empty cells, NA, NaN, NULL, N/A, ".", "-", and the codes -9, -99, -999 and -9999) → **warn**, naming the notations and the columns.
- *Units:* information only, never a warning. Numeric columns whose unit is not recognised from the column name, the codebook or the README's variable list are listed. No LLM is used.

**How 5 to 7 are built** (2026-10-18):
- Personal data: `data_check`'s findings "Personal info (values)", "Personal info (column name)", "Free-text (may hold PII)" and "Geographic coordinates"; BSN, phone and (later) TU/e number patterns on every table `data_check` read; key files by name. The report shows one row per column with everything found in it. BSN and phone columns and key files also count as personal data in section 1.3; free text and coordinates do not (they say little about whether people took part).
- Columns against the codebook: `codebook_check` runs (LLM off) after `data_check`; a column counts as described when its `label_status` is "labelled" or "conflicted". Not applicable when `codebook_check` found no codebook variables.
- Missing values: CSV and TSV files are read again as text, because `data_check`'s tables turn empty cells and "NA" into the same missing value; other tables come from `data_check`. The codes -9, -99, -999 and -9999 count only in a numeric column whose other values are all zero or more, so a real negative measurement is not taken for a code.
- Units (information): a numeric column, other than an identifier, has a unit when `data_check` recognised one, when its name ends in a unit (`_cm`, `_ms`, `_hz`, ...), or when a line of the README or a text codebook names the column together with a unit or the word "unit".

### 8. Code check

`code_check` runs as it is. Anything it flags → **warn**, never a fail (TU/e only wants to experiment with it). Its report is included in this section.

*Built as* (2026-10-18): `code_check` runs after `codebook_check` with the LLM off. Its light yellow or red → **warn**, green → pass, no code files → not applicable. What was found and what to do are taken from its table of code files, one finding per kind, each with its own advice (2026-10-18): absolute paths (named, with their file), files the code loads that are not in the package, `setwd()`, `install.packages()`, files without comments, libraries loaded in more than one place, R code that cannot be read, and no list of package versions (for R: an `renv.lock` or `sessionInfo()`; for Python: a `requirements.txt`). Its full report follows in a folded box, with its headings one level lower.

### 9. The code runs

Not asked by the RDLC, added on 2026-10-18 because a data manager cannot easily try this by hand. With `run_code = TRUE`, the module runs metacheck's `reproducibility_check` (`execute = TRUE`) on `code_check`'s results, with any further arguments in `run_args`; on the web page this is a tick box, ticked by default, and the SURF server runs the code in its Docker sandbox, without installing packages, at most 5 minutes per script.

- *Result:* a script that errored, was still running at the time limit, could not be run because files it reads are missing, needs a package that could not be installed, or could not be read → **warn**, as for the code check. Every script ran without an error → pass. Not asked, or no R or Python code → not applicable. `reproducibility_check` itself failed → **manual**.
- *What was found:* how many scripts were run and how each ended, the scripts that were not run and why, and for each script with an error the last line of its error. *What to do:* one line per kind of problem. Its full report (the output of each script) follows in a folded box.

### Zipped packages

A package handed in as a zip, or zip/tar/7z/rar archives inside the package → **fail** ("Data packages should not be zipped before submission for RAPS").

---

## Open points

1. The licence of the public repository `Lakens/tue_datamanager_check`: AGPL-3.0 (decided 2026-10-18), because four rule files are adapted from the AGPL-3.0 pack by Jakub Werner (`scienceverse/metacheck-tue`, a private repository); the README template keeps its CC BY 4.0 licence. Credits are in README.md.
2. The format of TU/e student and employee numbers (to ask the data managers).
3. The email to the data managers:
   - the mistakes in their own templates: spaces and "&" in names, YYYYMMDD dates, `.DS_Store` and `__MACOSX` in the zips, a README that does not follow their template, and a manuscript as `.docx`;
   - why the RDLC says "based on paper abstract" rather than the full text;
   - the format of TU/e student and employee numbers.
