## =============================================================
## Internship Week 1: Data Cleaning & Preliminary Analysis
## Dataset : Employee Attrition Dataset (HR analytics, 500+ employees)
## Author  : Nishanth Sridhar
## =============================================================

# ---- 0. Packages -------------------------------------------------
required_pkgs <- c("tidyverse", "janitor", "skimr", "corrplot", "psych")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[,"Package"])]
if (length(new_pkgs)) install.packages(new_pkgs)

library(tidyverse)   # dplyr, ggplot2, readr, tidyr
library(janitor)      # clean_names(), get_dupes()
library(skimr)        # skim() -> richer summary()
library(corrplot)     # correlation heatmap
library(psych)        # describe()

# ---- 1. Load the data ---------------------------------------------
df <- read_csv("employee_attrition_raw.csv", show_col_types = FALSE)

# Quick structural look (str() equivalent)
str(df)
dim(df)
head(df)

# ---- 2. Initial data quality audit --------------------------------
sum(is.na(df))
colSums(is.na(df))                       # missing values per column
sapply(df, function(x) round(mean(is.na(x))*100, 2))  # % missing per column

# ---- 3. Clean column-level inconsistencies -------------------------
df <- df %>%
  mutate(
    Department = str_trim(Department),                 # remove stray whitespace
    Gender     = str_to_title(str_trim(toupper(Gender))) # normalise "M"/"male"/"Male" -> "Male"
  ) %>%
  mutate(Gender = recode(Gender, "M" = "Male", "F" = "Female"))

# ---- 4. Remove duplicate records -----------------------------------
dupe_check <- df %>% get_dupes(-EmployeeID)
nrow(dupe_check)                                        # duplicate rows found
df <- df %>% distinct(across(-EmployeeID), .keep_all = TRUE)
nrow(df)

# ---- 5. Outlier detection (IQR method) ------------------------------
detect_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  sum(x < lower | x > upper, na.rm = TRUE)
}

numeric_cols <- c("Age","MonthlyIncome","YearsAtCompany",
                   "TotalWorkingYears","DistanceFromHome","NumCompaniesWorked")
sapply(df[numeric_cols], detect_outliers)

# Treat biologically impossible ages (e.g. 85, 90, 120) as missing
df <- df %>% mutate(Age = ifelse(Age > 65, NA, Age))

# Cap extreme MonthlyIncome outliers (beyond 1.4x the 99th percentile) -> NA, to be imputed
income_cap <- quantile(df$MonthlyIncome, 0.99, na.rm = TRUE) * 1.4
df <- df %>% mutate(MonthlyIncome = ifelse(MonthlyIncome > income_cap, NA, MonthlyIncome))

# ---- 6. Handle missing values ---------------------------------------
# Numeric columns -> median imputation (robust to skew/outliers)
num_impute <- c("Age","MonthlyIncome","TotalWorkingYears","DistanceFromHome","WorkLifeBalance")
for (col in num_impute) {
  med <- median(df[[col]], na.rm = TRUE)
  df[[col]][is.na(df[[col]])] <- med
}

# Categorical columns -> mode imputation
get_mode <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
df$EducationField[is.na(df$EducationField)] <- get_mode(df$EducationField)

# Confirm no missing values remain
colSums(is.na(df))

# ---- 7. Encode categorical variables ---------------------------------
df_encoded <- df %>%
  mutate(
    Gender_enc    = ifelse(Gender == "Female", 1, 0),
    OverTime_enc  = ifelse(OverTime == "Yes", 1, 0),
    Attrition_enc = ifelse(Attrition == "Yes", 1, 0)
  )

# One-hot encode multi-level categorical variables
df_encoded <- df_encoded %>%
  fastDummies::dummy_cols(select_columns = c("Department","JobRole"),
                           remove_selected_columns = FALSE)

# ---- 8. Normalise numeric variables (min-max scaling) -----------------
min_max_norm <- function(x) (x - min(x)) / (max(x) - min(x))
norm_cols <- c("Age","MonthlyIncome","YearsAtCompany",
               "TotalWorkingYears","DistanceFromHome","NumCompaniesWorked")
df_encoded[paste0(norm_cols, "_norm")] <- lapply(df_encoded[norm_cols], min_max_norm)

write_csv(df_encoded, "employee_attrition_cleaned.csv")

# ---- 9. Exploratory Data Analysis --------------------------------------

## 9a. Overall summary statistics
summary(df[c("Age","MonthlyIncome","YearsAtCompany","TotalWorkingYears",
             "DistanceFromHome","Education","JobSatisfaction",
             "WorkLifeBalance","NumCompaniesWorked")])

skim(df)                       # richer tidyverse-style summary
describe(df[numeric_cols])     # psych::describe() -> mean, sd, skew, kurtosis

## 9b. Categorical frequency tables
table(df$Gender)
table(df$Department)
table(df$Attrition)
prop.table(table(df$Attrition)) * 100   # attrition rate %

## 9c. Missing-value visual (generated BEFORE cleaning, using raw copy)
raw <- read_csv("employee_attrition_raw.csv", show_col_types = FALSE)
na_counts <- colSums(is.na(raw))
na_df <- data.frame(column = names(na_counts), missing = na_counts) %>%
  filter(missing > 0) %>% arrange(desc(missing))

ggplot(na_df, aes(x = reorder(column, missing), y = missing)) +
  geom_col(fill = "#4C72B0") +
  coord_flip() +
  labs(title = "Missing Values by Column (Before Cleaning)",
       x = "", y = "Number of Missing Values") +
  theme_minimal()
ggsave("missing_values.png", width = 7, height = 4.5, dpi = 150)

## 9d. Boxplot of MonthlyIncome before vs after cleaning
p1 <- ggplot(raw, aes(y = MonthlyIncome)) +
  geom_boxplot(fill = "#DD8452") + labs(title = "Before", y = "Monthly Income")
p2 <- ggplot(df, aes(y = MonthlyIncome)) +
  geom_boxplot(fill = "#55A868") + labs(title = "After Cleaning", y = "")
gridExtra::grid.arrange(p1, p2, ncol = 2)

## 9e. Age distribution
ggplot(df, aes(x = Age)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "#4C72B0", alpha = 0.8) +
  geom_density(color = "black", linewidth = 0.8) +
  labs(title = "Age Distribution (After Cleaning)", x = "Age", y = "Density") +
  theme_minimal()
ggsave("age_hist.png", width = 7, height = 4.5, dpi = 150)

## 9f. Correlation matrix + heatmap
corr_matrix <- cor(df[numeric_cols], use = "complete.obs")
round(corr_matrix, 2)
corrplot(corr_matrix, method = "color", addCoef.col = "black",
         type = "upper", tl.col = "black", number.cex = 0.8)

## 9g. Attrition rate by Department
attr_dept <- df %>%
  group_by(Department) %>%
  summarise(attrition_rate = mean(Attrition == "Yes") * 100) %>%
  arrange(attrition_rate)

ggplot(attr_dept, aes(x = reorder(Department, attrition_rate), y = attrition_rate)) +
  geom_col(fill = "#C44E52") + coord_flip() +
  labs(title = "Attrition Rate by Department", x = "", y = "Attrition Rate (%)") +
  theme_minimal()
ggsave("attrition_dept.png", width = 7, height = 4.5, dpi = 150)

## 9h. Attrition rate by OverTime status
attr_ot <- df %>%
  group_by(OverTime) %>%
  summarise(attrition_rate = mean(Attrition == "Yes") * 100)

ggplot(attr_ot, aes(x = OverTime, y = attrition_rate)) +
  geom_col(fill = "#8172B2") +
  labs(title = "Attrition Rate by OverTime Status", y = "Attrition Rate (%)") +
  theme_minimal()
ggsave("attrition_ot.png", width = 5.5, height = 4.5, dpi = 150)

## 9i. Monthly income by department
ggplot(df, aes(x = Department, y = MonthlyIncome, fill = Department)) +
  geom_boxplot() +
  labs(title = "Monthly Income by Department") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 15, hjust = 1))
ggsave("income_by_dept.png", width = 7, height = 4.5, dpi = 150)

## ---- 10. Save cleaning log / session info for reproducibility --------
sink("session_info.txt")
sessionInfo()
sink()

## =============================================================
## End of Week 1 script
## =============================================================
