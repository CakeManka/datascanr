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
