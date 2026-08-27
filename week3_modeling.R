## =============================================================
## Internship Week 3: Statistical Analysis & Predictive Modeling
## Dataset : Employee Attrition Dataset (continued from Weeks 1-2)
## Author  : Nishanth Sridhar
## =============================================================

library(tidyverse)
library(caret)        # train/test split, confusion matrix, cross-validation
library(pROC)         # ROC curve / AUC
library(car)          # vif()
library(broom)         # tidy() model summaries

set.seed(42)

df <- read_csv("employee_attrition_cleaned.csv", show_col_types = FALSE)
df$Attrition <- factor(df$Attrition, levels = c("No","Yes"))

# =====================================================================
## 1. Dataset Rationale
## The same cleaned Employee Attrition dataset used in Weeks 1-2 is
## reused here: it has a clearly defined binary outcome (Attrition),
## a mix of continuous and categorical predictors, and — as
## established in Week 2's EDA — visible, non-trivial relationships
## between predictors and the outcome (income, tenure, overtime,
## satisfaction), making it well-suited for both hypothesis testing
## and predictive classification modeling.
# =====================================================================

# =====================================================================
## 2. EXPLORATORY STATISTICAL ANALYSIS / HYPOTHESIS TESTING
# =====================================================================

## 2.1 Normality check — Shapiro-Wilk test on MonthlyIncome
shapiro.test(df$MonthlyIncome)
# H0: MonthlyIncome is normally distributed
# Result: W = 0.972, p < .001  -> reject H0 (income is not normally
# distributed; right-skewed, as seen in the Week 1 histogram). This
# justifies using non-parametric/robust summaries (median) alongside
# the mean, and supports feature standardization before modeling.

## 2.2 Welch two-sample t-test — MonthlyIncome by Attrition
t.test(MonthlyIncome ~ Attrition, data = df)
# H0: mean MonthlyIncome is equal for Attrition = Yes vs No

## 2.3 Chi-square test of independence — OverTime vs Attrition
tbl <- table(df$OverTime, df$Attrition)
chisq.test(tbl)
# H0: OverTime and Attrition are independent

## 2.4 Pearson correlation test — MonthlyIncome vs YearsAtCompany
cor.test(df$MonthlyIncome, df$YearsAtCompany)
# H0: true correlation is 0

## 2.5 One-way ANOVA — MonthlyIncome across Department
anova_model <- aov(MonthlyIncome ~ Department, data = df)
summary(anova_model)
# H0: mean MonthlyIncome is equal across all three departments

# =====================================================================
## 3. MODEL BUILDING — Logistic Regression Classifier
## Attrition (Yes/No) is a binary outcome, so logistic regression
## (via glm(family = "binomial")) is the appropriate baseline model.
# =====================================================================

model_df <- df %>%
  mutate(
    OverTime_enc = ifelse(OverTime == "Yes", 1, 0),
    Gender_enc   = ifelse(Gender == "Female", 1, 0)
  ) %>%
  select(Attrition, Age, MonthlyIncome, YearsAtCompany, TotalWorkingYears,
         DistanceFromHome, JobSatisfaction, WorkLifeBalance, NumCompaniesWorked,
         OverTime_enc, Gender_enc, `Department_Human Resources`, Department_Sales)

## 3.1 Train / test split (80/20, stratified on the outcome)
train_idx <- createDataPartition(model_df$Attrition, p = 0.8, list = FALSE)
train_df <- model_df[train_idx, ]
test_df  <- model_df[-train_idx, ]

## 3.2 Standardize continuous predictors (fit scaling on train only)
preproc <- preProcess(train_df %>% select(-Attrition), method = c("center","scale"))
train_scaled <- predict(preproc, train_df)
test_scaled  <- predict(preproc, test_df)

## 3.3 Fit logistic regression
log_model <- glm(Attrition ~ ., data = train_scaled, family = binomial(link = "logit"))
summary(log_model)

## 3.4 Model fit statistics
null_dev <- log_model$null.deviance
res_dev  <- log_model$deviance
mcfadden_r2 <- 1 - (res_dev / null_dev)
cat("McFadden's pseudo R-squared:", round(mcfadden_r2, 4), "\n")
AIC(log_model)

## 3.5 Multicollinearity check
vif(log_model)

## 3.6 5-fold cross-validation
train_control <- trainControl(method = "cv", number = 5)
cv_model <- train(Attrition ~ ., data = train_scaled, method = "glm",
                   family = "binomial", trControl = train_control)
print(cv_model)

# =====================================================================
## 4. MODEL EVALUATION ON HELD-OUT TEST SET
# =====================================================================
test_probs <- predict(log_model, newdata = test_scaled, type = "response")
test_pred  <- factor(ifelse(test_probs > 0.5, "Yes", "No"), levels = c("No","Yes"))

## 4.1 Confusion matrix and classification metrics
conf_mat <- confusionMatrix(test_pred, test_scaled$Attrition, positive = "Yes")
print(conf_mat)

## 4.2 ROC curve and AUC
roc_obj <- roc(response = test_scaled$Attrition, predictor = test_probs, levels = c("No","Yes"))
auc(roc_obj)
plot(roc_obj, main = "ROC Curve - Logistic Regression", col = "#4C72B0", lwd = 2)

# =====================================================================
## 5. DIAGNOSTIC PLOTS
# =====================================================================

## 5.1 Confusion matrix heatmap
cm_table <- as.data.frame(conf_mat$table)
ggplot(cm_table, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() + geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "white", high = "#4C72B0") +
  labs(title = "Confusion Matrix (Test Set, threshold = 0.5)") +
  theme_minimal(base_size = 13)
ggsave("confusion_matrix.png", width = 5, height = 4.5, dpi = 150)

## 5.2 Deviance residual diagnostics (base R plot, standard glm diagnostics)
plot(log_model, which = 1)   # Residuals vs Fitted
dev.copy(png, "residuals_vs_fitted.png", width = 650, height = 450); dev.off()

## 5.3 Coefficient plot
coefs <- tidy(log_model) %>% filter(term != "(Intercept)") %>%
  mutate(sig = p.value < 0.05) %>% arrange(estimate)

ggplot(coefs, aes(x = reorder(term, estimate), y = estimate, fill = sig)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#C44E52", "FALSE" = "grey70")) +
  labs(title = "Logistic Regression Coefficients (red = significant at p<0.05)",
       x = "", y = "Coefficient (log-odds, standardized predictors)") +
  theme_minimal(base_size = 13)
ggsave("coefficient_plot.png", width = 7.5, height = 5.5, dpi = 150)

## 5.4 Normality diagnostics for MonthlyIncome (histogram + Q-Q plot)
par(mfrow = c(1,2))
hist(df$MonthlyIncome, breaks = 25, col = "#4C72B0", main = "Monthly Income Distribution",
     xlab = "Monthly Income")
qqnorm(df$MonthlyIncome, main = "Q-Q Plot vs. Normal Distribution"); qqline(df$MonthlyIncome, col = "red")
dev.copy(png, "normality_check.png", width = 900, height = 420); dev.off()
par(mfrow = c(1,1))

## =============================================================
## End of Week 3 script
## =============================================================
