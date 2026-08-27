# Week 1 Task — Data Cleaning and Preliminary Analysis with R

**Software Engineering Internship Program**
**Author:** Nishanth Sridhar

## Overview

This project performs a complete, reproducible data-cleaning and exploratory-analysis workflow in **R** on a 500-record HR Employee Attrition dataset. The dataset combines numerical variables (age, monthly income, tenure, distance from home) with categorical variables (gender, department, job role, overtime status, attrition) and includes realistic data-quality issues: missing values, inconsistent category labels, duplicate records, and outliers.

Full write-up, R code, console outputs, and visualizations are documented in **`Week1_Data_Cleaning_Report.docx`** (attached separately in the internship portal submission).

## Files in this repository

| File | Description |
|---|---|
| `week1_analysis.R` | Full R script — data cleaning, missing value imputation, outlier detection, encoding, normalization, and EDA (runs end-to-end) |
| `employee_attrition_raw.csv` | Raw input dataset (500 employees, 17 variables) before cleaning |
| `employee_attrition_cleaned.csv` | Cleaned, encoded, and normalized dataset (500 rows × 30 columns) produced by the script |

## Dataset

500 employee records across three departments (Sales, Research & Development, Human Resources), modeled on the structure of the public [IBM HR Analytics Employee Attrition dataset](https://www.kaggle.com/datasets/pavansubhasht/ibm-hr-analytics-attrition-dataset). Variables include:

- **Numeric:** Age, MonthlyIncome, YearsAtCompany, TotalWorkingYears, DistanceFromHome, NumCompaniesWorked
- **Ordinal:** Education, JobSatisfaction, WorkLifeBalance, PerformanceRating
- **Categorical:** Gender, Department, EducationField, JobRole, OverTime, Attrition (target)

## Data quality issues addressed

- Missing values in 6 columns (up to 6% per column)
- Inconsistent categorical labels (e.g. `"Male"` / `"M"` / `"male"`)
- Leading/trailing whitespace in text fields
- 4 duplicate records
- Outliers: biologically implausible ages, abnormally high income entries

## Pipeline steps (R)

1. **Load & inspect** — `str()`, `dim()`, `colSums(is.na(df))`
2. **Standardize** categorical labels and whitespace
3. **Remove duplicates** — `janitor::get_dupes()`
4. **Detect outliers** — IQR method (Q1 − 1.5×IQR, Q3 + 1.5×IQR)
5. **Impute missing values** — median (numeric), mode (categorical)
6. **Encode categoricals** — label encoding (binary) + one-hot encoding (`fastDummies`)
7. **Normalize** — min-max scaling on numeric columns
8. **Exploratory analysis** — `summary()`, `skim()`, correlation matrix, `ggplot2` visualizations

## Requirements

```r
install.packages(c("tidyverse", "janitor", "skimr", "corrplot", "psych", "fastDummies", "gridExtra"))
```

## Usage

```r
setwd("path/to/repo")
source("week1_analysis.R")
```

This regenerates `employee_attrition_cleaned.csv` and all charts (missing-value plot, boxplots, histograms, correlation heatmap, attrition breakdowns) referenced in the report.

## Key findings

- Overall attrition rate: **22.8%** (114 of 500 employees)
- Employees working overtime leave at more than double the rate of those who don't (**40.3% vs 17.0%**)
- Sales has the highest departmental attrition (**27.4%**); R&D the lowest (**17.9%**)
- `YearsAtCompany` and `TotalWorkingYears` are strongly correlated (**r = 0.81**)
- No strong multicollinearity among remaining numeric predictors

## License

Educational project prepared for the Yuva Intern internship program.
