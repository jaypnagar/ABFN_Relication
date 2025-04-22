
# Replication Package  
**Revisiting Stock Market Signals as a Lens for Patent Valuation**  
*Ashish Arora, Sharon Belenzon, Elia Ferracuti, Jay Prakash Nagar*

## Overview

This repository contains all code and instructions necessary to replicate the empirical results from the paper. The analysis builds on the KPSS methodology to estimate the private value of patents using stock market reactions, accounting for differences in signal-to-noise ratios.

---

## Folder Structure

```
ABF_Replication/
├── code/        # Stata and Python code
├── data/        # Input datasets (not all included due to licensing)
├── output/      # Output files and regression results
├── README.md    # This file
```

---

## Getting Started

Set your working directory in Stata to the main project folder:

```stata
cd "PATH\ABF_Replication"
gl OUT "PATH\ABF_Replication\Output"
gl IN "PATH\ABF_Replication\Data"
clear all
```

---

## Code Workflow

### Step 1: Merge Citation Data
```stata
do "code/A0_add_citations_to_patents.do"
```

### Step 2: Create Patent-Level Indicators
```stata
do "code/A1_patent_indicator_construction_variable.do"
```

> Also run the Python notebook `A0_PatentLevelIndicator_TeamForeignInvt.ipynb` to generate team size and foreign inventor indicators.

### Step 3: Sample Construction
```stata
do "code/A2_Do_SampleCreation.do"
```

> Note: Requires SAS code to convert `patentawards.xlsx` to `ret_patents.dta`.

---

## KPSS Estimation

- **Baseline (single SNR)**  
  `do "code/A3_Kpss_baseline.do"`

- **Foreign vs. Domestic**  
  `do "code/A3_Kpss_Estimation_ForeignInvt.do"`

- **Team Size**  
  `do "code/A3_Kpss_Estimation_TeamSize.do"`

- **Vertical Integration**  
  `do "code/A3_Kpss_Estimation_VerticalInteg.do"`

- **IID Assumption (Appendix)**  
  `do "code/Appendix_A3_Do_Analyses_KPSS_NewAssumption.do"`

---

## Regression and Output

- **Create Final Patent-Level Sample**  
  `do "code/A4_Do_Regression_Sample_Construction.do"`

- **Summary Stats and Main Regressions**  
  `do "code/A5_Regression_Baseline.do"`

---

## Data Inputs

Some raw datasets are proprietary or too large for GitHub. Below are required inputs by stage:

### A0
- `patents_1980_2021_UO.dta` (DISCERN)
- `_pcs_mag_doi_pmid.tsv` (Reliance on Science)

### A1
- `g_ipc_at_issue.tsv` (USPTO IPC)
- `202401_OECD_PATENT_QUALITY_USPTO_INDIC.txt` (OECD indicators)
- `CPIAUCSL.csv` (CPI data)
- `patent_invt_indicator.csv` (generated via Python: To run Python code, EPO Patstat data is required)

### A2
- `ret_patents.dta` (from SAS + `patentawards.xlsx`)
- `VertInteg.txt` (Hoberg-Phillips vertical relatedness data library)

---

## Notes

- Please ensure all files are placed in their correct directories (`data/`, `code/`, etc.).
- Outputs (e.g., tables, regression logs) will be saved to the `/output` folder.
- This package is compatible with Stata 16+ and Python 3.7+ (for preprocessing tasks).

---
