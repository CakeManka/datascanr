# datascanr

**datascanr** is a lightweight R package for *early-stage data sanity checks*.

It helps you quickly detect common data issues **before** you start statistical analysis or modeling — especially useful for beginners, interdisciplinary researchers, and anyone working with messy real-world data.

Designed to answer one question fast:

> **“Is my data basically OK to analyze?”**

---

## ✨ Features

- 🔍 Automatic object type detection (S3 dispatch)
- 📦 Supports common objects:
  - data.frame / tibble
  - matrix
  - list
  - Seurat (if installed)
  - SummarizedExperiment (if installed)
- 📊 Data checks (examples):
  - Missing values (overall and by column)
  - Duplicate / empty names
  - Columns that look numeric but are stored as text
  - Constant (non-informative) columns
  - Inf/NaN in numeric data
- 🧾 Concise, human-readable console reports (with emoji badges)
- 🔎 Helpers to inspect results:
  - `report_details()` for summaries/tables
  - `report_issues()` for the full issues table
  - `report_meta()` for diagnostics

---

## 📦 Installation

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("CakeManka/datascanr")
```

---

## 🚀 Quick Start

```r
library(datascanr)

df <- data.frame(
  id  = c("S1", "S2", "S3"),
  age = c("20", "21", "22"),   # numeric stored as text
  bmi = c(22.1, NA, 30.5),     # missing value
  stringsAsFactors = FALSE
)

r <- scan(df)
r

# inspect details / issues
report_details(r)
report_issues(r)
```


---

## 🧾 Example output

datascanr
- type : data.frame
- class: data.frame
- dim  : 3 x 3
- missing(overall): 11.11%
- issues: 🟥0 🟨2 ℹ0

Top issues:
 🟨 Columns with missing rate >= 10% detected.  (bmi)
 🟨 Columns look numeric but are stored as text or factor.  (age)

Tip: `report_details(x)` for details; `report_issues(x)` for all issues.
Next: fix the fields shown in `(where)`; then rerun `scan()`.


🧠 Output structure

scan() always returns a datascan_report object with a unified structure:

- type — detected object type
- class — original R class(es)
- details — descriptive summaries and tables
- issues — structured issues with severity and suggestions
- meta — optional diagnostic information

---

## ✅ What does datascanr check?

`scan()` always returns a unified `datascan_report`:

- `details`: descriptive summaries (dimensions, profiles, tables)
- `issues`: structured issues with `severity` (`error`/`warn`/`info`), `message`, `where`, and `suggestion`

### data.frame / tibble
Checks:
- empty data (0 rows/cols)
- empty/duplicate column names
- high missingness (>=80% error, >=10% warn)
- numeric-looking text columns
- constant columns
- Inf/NaN in numeric columns
- list-columns (common in tibbles; informational, may warn if many)

Details:
- `nrow`, `ncol`, `missing_overall`
- `details$tables$missing_by_col`
- `details$tables$type_profile`

### matrix
Checks:
- empty matrix
- high missingness (>=80% error, >=10% warn)
- duplicate row/col names
- Inf/NaN in numeric matrix

Details:
- `nrow`, `ncol`, `typeof`, `missing_rate`, `min/max` (numeric), optional `zero_rate`

### list
Checks:
- empty list
- missing/empty/duplicate names
- NULL elements (info)
- very large list (info)

Details:
- length, name stats, element class distribution, element length summary, n_null

### SummarizedExperiment (if installed)
Checks:
- empty dims, no assays
- empty rowData/colData (info)
- assay dimension mismatch
- non-finite values in primary assay (optional warn)

Details:
- dims, assay count/names, rowData/colData columns, primary assay class/dim

### Seurat (if installed)
Checks:
- no assays
- 0 cells or 0 features
- default assay missing
- empty metadata (info)

Details:
- assays, default assay, cells/features, metadata columns, reductions

---

## 🔎 When should I use datascanr?

- Right after loading or merging data
    
- Before modeling or statistical testing
    
- When debugging strange downstream errors
    
- When reviewing data from collaborators or external sources
    

Think of **datascanr** as a _data pre-flight check_ ✈️.

---
