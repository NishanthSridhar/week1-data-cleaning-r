## =============================================================
## Internship Week 2: Data Visualization & Insight Communication
## Dataset : Employee Attrition Dataset (continued from Week 1)
## Author  : Nishanth Sridhar
## =============================================================

library(tidyverse)   # ggplot2, dplyr, readr
library(scales)      # percent/label formatting on axes

# ---- 1. Load the cleaned Week 1 dataset -----------------------------
df <- read_csv("employee_attrition_cleaned.csv", show_col_types = FALSE)

# Derive HireYear (used for the time-trend chart in Section 5)
CURRENT_YEAR <- 2026
df <- df %>% mutate(HireYear = CURRENT_YEAR - YearsAtCompany)

# =====================================================================
## 2. BAR CHART — Attrition rate by Department
## Chart type chosen: horizontal bar chart, for comparing a rate (%)
## across a small number of unordered categories.
# =====================================================================
attr_dept <- df %>%
  group_by(Department) %>%
  summarise(attrition_rate = mean(Attrition == "Yes") * 100) %>%
  arrange(attrition_rate)

ggplot(attr_dept, aes(x = reorder(Department, attrition_rate), y = attrition_rate)) +
  geom_col(fill = "#C44E52") +
  geom_text(aes(label = paste0(round(attrition_rate,1), "%")), hjust = -0.1) +
  coord_flip() +
  labs(title = "Attrition Rate by Department",
       x = "", y = "Attrition Rate (%)") +
  theme_minimal(base_size = 13)
ggsave("bar_attrition_dept.png", width = 7, height = 4.5, dpi = 150)

# =====================================================================
## 3. SCATTER PLOT — Monthly Income vs. Years at Company, by Attrition
## Chart type chosen: scatter plot, to reveal the relationship between
## two continuous variables while a third categorical variable (colour)
## shows how attrition is distributed across that relationship.
# =====================================================================
ggplot(df, aes(x = YearsAtCompany, y = MonthlyIncome, color = Attrition)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_manual(values = c("No" = "#4C72B0", "Yes" = "#C44E52")) +
  labs(title = "Monthly Income vs. Years at Company (by Attrition)",
       x = "Years at Company", y = "Monthly Income") +
  theme_minimal(base_size = 13)
ggsave("scatter_income_years.png", width = 7.2, height = 5, dpi = 150)

cor(df$MonthlyIncome, df$YearsAtCompany)   # correlation coefficient referenced in the narrative

# =====================================================================
## 4. HISTOGRAM — Age distribution split by Attrition
## Chart type chosen: stacked histogram, to compare the shape of a
## continuous distribution (Age) across two groups (Attrition Yes/No).
# =====================================================================
ggplot(df, aes(x = Age, fill = Attrition)) +
  geom_histogram(bins = 18, position = "stack", color = "white") +
  scale_fill_manual(values = c("No" = "#4C72B0", "Yes" = "#C44E52")) +
  labs(title = "Age Distribution by Attrition Status", x = "Age", y = "Count") +
  theme_minimal(base_size = 13)
ggsave("hist_age_attrition.png", width = 7.2, height = 4.5, dpi = 150)

# =====================================================================
## 5. LINE CHART — Hiring trend over time (current employees, by hire year)
## Chart type chosen: dual-axis line chart, the standard choice for
## showing a trend over a continuous time axis (year) together with a
## cumulative running total.
# =====================================================================
hires_per_year <- df %>% count(HireYear, name = "new_hires") %>% arrange(HireYear)
hires_per_year <- hires_per_year %>% mutate(cumulative = cumsum(new_hires))

# Base R dual-axis version (kept close to how it is often taught):
par(mar = c(5, 4, 4, 4) + 0.3)
plot(hires_per_year$HireYear, hires_per_year$new_hires, type = "o", pch = 16,
     col = "#4C72B0", xlab = "Year", ylab = "New Hires",
     main = "Hiring Trend Over Time (Current Employees, by Hire Year)")
par(new = TRUE)
plot(hires_per_year$HireYear, hires_per_year$cumulative, type = "o", pch = 15,
     col = "#55A868", axes = FALSE, xlab = "", ylab = "")
axis(side = 4)
mtext("Cumulative Headcount", side = 4, line = 3)
legend("topleft", legend = c("New hires (that year)", "Cumulative headcount"),
       col = c("#4C72B0", "#55A868"), pch = c(16, 15), bty = "n")
dev.copy(png, "line_hiring_trend.png", width = 800, height = 480); dev.off()

# =====================================================================
## 6. BOXPLOT — Monthly Income distribution by Job Role
## Chart type chosen: boxplot, ideal for comparing the spread, median,
## and outliers of a continuous variable across several categories.
# =====================================================================
role_order <- df %>% group_by(JobRole) %>%
  summarise(med = median(MonthlyIncome)) %>% arrange(med) %>% pull(JobRole)
df$JobRole <- factor(df$JobRole, levels = role_order)

ggplot(df, aes(x = MonthlyIncome, y = JobRole, fill = JobRole)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Monthly Income Distribution by Job Role",
       x = "Monthly Income", y = "") +
  theme_minimal(base_size = 13)
ggsave("box_income_role.png", width = 8.5, height = 5, dpi = 150)

# =====================================================================
## 7. STACKED BAR — Attrition rate within each Job Satisfaction level
## Chart type chosen: 100%-stacked bar chart, best for showing how a
## binary outcome (Attrition) is proportionally distributed within
## each level of an ordinal variable (JobSatisfaction).
# =====================================================================
jobsat_attr <- df %>%
  count(JobSatisfaction, Attrition) %>%
  group_by(JobSatisfaction) %>%
  mutate(pct = n / sum(n) * 100)

ggplot(jobsat_attr, aes(x = factor(JobSatisfaction), y = pct, fill = Attrition)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("No" = "#4C72B0", "Yes" = "#C44E52")) +
  labs(title = "Attrition Rate Within Each Job Satisfaction Level",
       x = "Job Satisfaction (1 = Low, 4 = High)", y = "% of Employees") +
  theme_minimal(base_size = 13)
ggsave("stackedbar_jobsat_attrition.png", width = 7, height = 4.5, dpi = 150)

# =====================================================================
## 8. DONUT CHART — Education Field distribution
## Chart type chosen: donut chart, a simple, non-technical-audience-
## friendly way to show the composition of a single categorical variable.
# =====================================================================
edu <- df %>% count(EducationField) %>% mutate(pct = n / sum(n) * 100,
                                                 label = paste0(EducationField, " (", round(pct,1), "%)"))

ggplot(edu, aes(x = 2, y = pct, fill = EducationField)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  labs(title = "Education Field Distribution", fill = "Education Field") +
  theme_void(base_size = 13)
ggsave("donut_education_field.png", width = 6, height = 6, dpi = 150)

# =====================================================================
## 9. GROUPED BAR CHART — Average Monthly Income by Department and Gender
## Chart type chosen: grouped (dodged) bar chart, for comparing a
## numeric summary (mean income) across two categorical dimensions
## side-by-side (Department x Gender).
# =====================================================================
inc_g <- df %>% group_by(Department, Gender) %>%
  summarise(avg_income = mean(MonthlyIncome), .groups = "drop")

ggplot(inc_g, aes(x = Department, y = avg_income, fill = Gender)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("Male" = "#4C72B0", "Female" = "#DD8452")) +
  labs(title = "Average Monthly Income by Department and Gender",
       x = "", y = "Average Monthly Income") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 10, hjust = 1))
ggsave("bar_income_dept_gender.png", width = 7.5, height = 4.8, dpi = 150)

## =============================================================
## End of Week 2 script
## =============================================================
