# datascanr

**datascanr** is a lightweight R package for *early-stage data sanity checks*.

It helps you quickly detect common data issues **before** you start statistical analysis or modeling — especially useful for beginners, interdisciplinary researchers, and anyone working with messy real-world data.

---

## ✨ Features

- 🔍 Automatic object type detection
- 📊 Data frame checks:
  - Missing values (overall and by column)
  - Columns that look numeric but are stored as text
  - Constant (non-informative) columns
- 🧾 Concise, human-readable console reports (with emoji badges)
- 🔎 `details()` helper for full inspection tables

Designed to answer one question fast:

> **“Is my data basically OK to analyze?”**

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

datascanr::scan(df)
```


##  Example output:

```yaml
datascanr
- type: data_frame
- dim : 3 x 3
- missing(overall): 11.11%
- issues: 🟨 2

Top issues:
 🟨 Columns with missing rate >= 10% detected (bmi)
 🟨 Columns look numeric but are stored as text (age)

Tip: `details(x)` for tables; `x$issues` for all issues.
```

