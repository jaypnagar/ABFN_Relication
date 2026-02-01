# Replication Package
## Revisiting Stock Market Signals as a Lens for Patent Valuation

**Ashish Arora, Sharon Belenzon, Elia Ferracuti, Jay Prakash Nagar**

---

## Overview

This repository contains the complete replication package for the paper
**“Revisiting Stock Market Signals as a Lens for Patent Valuation.”**

The analysis applies the KPSS methodology to estimate the private value of patents
using stock market reactions. The empirical framework allows for heterogeneity in
signal-to-noise ratios across patents, capturing differences related to inventor
teams, foreign inventorship, science linkage, and vertical integration.

Some raw datasets are proprietary and therefore not redistributed.

---

## Repository Structure

```
ABF_Replication/
├── code/        Stata .do files and Python notebooks
├── data/        Input datasets (restricted datasets not included)
├── output/      Regression outputs, tables, and logs
├── README.md    Replication instructions (this file)
```

---

## Software Requirements

- Stata 16 or higher
- Python 3.7 or higher (for preprocessing steps)
- SAS (required only to generate stock market return data)

---

## Getting Started

Set the working directory and global paths in Stata:

```stata
/* Set working directory */

/* Elia */
/* cd "C:\Users\ef122\Duke University\Jay Prakash Nagar - ABF_PatValue_Research" */

/* Jay */
cd "C:\Users\jayho\OneDrive - Duke University\ABF_Replication"

gl OUT "C:\Users\jayho\OneDrive - Duke University\ABF_Replication\Output"
gl IN  "C:\Users\jayho\OneDrive - Duke University\ABF_Replication\Data"

clear all
```

---

## Code Workflow

### Step 1: Merge Patent and Citation Data

```stata
do "code/A0_add_citations_to_patents.do"
```

Output:
- patents_1980_2021_UO_with_citations_to_science.dta

---

### Step 2: Construct Patent-Level Indicators

```stata
do "code/A1_patent_indicator_construction_variable.do"
```

Additionally, run the Python notebook:
- code/A0_PatentLevelIndicator_TeamForeignInvt.ipynb

---

### Step 3: Sample Construction

```stata
do "code/A2_Do_SampleCreation.do"
```

Note: Requires SAS code to convert patentawards.xlsx into ret_patents.dta.

---

## KPSS Estimation

```stata
do "code/A3_Kpss_baseline.do"
do "code/A3_Kpss_Estimation_ForeignInvt.do"
do "code/A3_Kpss_Estimation_TeamSize.do"
do "code/A3_Kpss_Estimation_VerticalInteg.do"
do "code/Appendix_A3_Do_Analyses_KPSS_NewAssumption.do"
```

---

## Regression Analysis and Tables

```stata
do "code/A4_Do_Regression_Sample_Construction.do"
do "code/A5_Table_SignalToNoiseRatio.do"
do "code/A6_SummaryStatsTables_PatentLevel.do"
do "code/A7_Table_Regression_baseline.do"
do "code/A8_AbnormalReturnAsDepVar_Regression.do"
do "code/A9_KPSSasIndepVar_standarized_bootstrap.do"
```

---

## Data Inputs

Some datasets are proprietary or too large to be hosted on GitHub.

### Citation Merge (A0)
- patents_1980_2021_UO.dta (DISCERN)
- _pcs_mag_doi_pmid.tsv (Reliance on Science)

### Patent-Level Indicators (A1)
- g_ipc_at_issue.tsv (USPTO / PatentsView IPC data)
- 202401_OECD_PATENT_QUALITY_USPTO_INDIC.txt (OECD)
- CPIAUCSL.csv (Consumer Price Index)
- patent_invt_indicator.csv (generated via Python; requires EPO PATSTAT)

### Sample Construction (A2)
- ret_patents.dta (generated via SAS from patentawards.xlsx)
- VertInteg.txt (Hoberg–Phillips vertical relatedness data)

---

## Notes

- All output files are written to the output/ directory.
- File paths may need to be adjusted depending on the local system.
- Results are reproducible conditional on access to the proprietary datasets.

---

## Citation

If you use this code or data structure, please cite:

Arora, A., Belenzon, S., Ferracuti, E., and Nagar, J. P.
Revisiting Stock Market Signals as a Lens for Patent Valuation.
