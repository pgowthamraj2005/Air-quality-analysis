# ==============================================================
# Air Quality Analysis using R
# Course : 23CAP317 - R Programming (Mini Project)
# Author : GOWTHAMRAJ P
# Data   : New York daily air quality, May-Sep 1973 (153 rows)
#          Ozone (ppb), Solar.R (Ly), Wind (mph), Temp (F), Month, Day
# Goal   : Predict ground-level Ozone from weather conditions
#          (regression) and flag high-ozone days (classification)
# ==============================================================

suppressPackageStartupMessages({
  library(ggplot2); library(corrplot); library(ranger)
  library(caret);   library(dplyr)
})
set.seed(2026)
dir.create("plots", showWarnings = FALSE)

# ---------------------------------------------------------------
# PART 1 : DATA STRUCTURES  (Q1)
# ---------------------------------------------------------------
cat("\n===== PART 1: DATA STRUCTURES =====\n")
temp_vec   <- c(67, 72, 74, 62, 56)                   # vector
month_fct  <- factor(c("May","Jun","Jul","Aug","Sep"),
                     levels = c("May","Jun","Jul","Aug","Sep"))  # factor
sample_mat <- matrix(c(41,190,7.4, 36,118,8.0), nrow = 2, byrow = TRUE,
                     dimnames = list(c("Day1","Day2"),
                                     c("Ozone","Solar","Wind")))  # matrix
sample_lst <- list(station = "New York", year = 1973,
                   pollutant = "Ozone", units = "ppb")            # list
print(temp_vec); print(month_fct); print(sample_mat); str(sample_lst)

# Load the dataset (data frame)
air <- read.csv("aq.csv")
air$rownames <- NULL
cat("\nDimensions:\n"); print(dim(air))
cat("\nColumn names:\n"); print(names(air))
cat("\nHead:\n"); print(head(air))
cat("\nStructure:\n"); str(air)
cat("\nSummary:\n"); print(summary(air))

# ---------------------------------------------------------------
# PART 2 : CONTROL STRUCTURES, FUNCTIONS, LIST, DATA FRAME  (Q2)
# ---------------------------------------------------------------
cat("\n===== PART 2: CONTROL STRUCTURES & FUNCTIONS =====\n")

# User-defined function with if / else if / else
# Ozone categories (illustrative bands in ppb)
ozone_category <- function(x) {
  if (is.na(x))      "Unknown"
  else if (x <= 30)  "Good"
  else if (x <= 60)  "Moderate"
  else if (x <= 90)  "Unhealthy"
  else               "Very Unhealthy"
}
air$OzoneLevel <- factor(sapply(air$Ozone, ozone_category),
                         levels = c("Good","Moderate","Unhealthy",
                                    "Very Unhealthy","Unknown"))
print(table(air$OzoneLevel))

# for loop : mean of every numeric column ignoring NA
col_means <- numeric(0)
for (nm in c("Ozone","Solar.R","Wind","Temp")) {
  col_means[nm] <- mean(air[[nm]], na.rm = TRUE)
}
cat("\nColumn means (for loop):\n"); print(round(col_means, 2))

# while loop : first day in the data where Ozone exceeds 100 ppb
i <- 1
while (i <= nrow(air) && (is.na(air$Ozone[i]) || air$Ozone[i] <= 100)) i <- i + 1
cat("\nFirst row with Ozone > 100 ppb (while loop): row", i,
    "| Month", air$Month[i], "Day", air$Day[i], "Ozone", air$Ozone[i], "\n")

# Function returning a list
describe_var <- function(v) {
  v <- v[!is.na(v)]
  list(n = length(v), mean = mean(v), sd = sd(v),
       min = min(v), max = max(v))
}
stats_list <- lapply(air[, c("Ozone","Solar.R","Wind","Temp")], describe_var)
cat("\nList of statistics (lapply):\n"); str(stats_list)

# apply family on a data frame
cat("\nMonthly average Ozone (tapply):\n")
print(round(tapply(air$Ozone, air$Month, mean, na.rm = TRUE), 2))

# dplyr summarise on the data frame
monthly <- air %>% group_by(Month) %>%
  summarise(days = n(), mean_ozone = mean(Ozone, na.rm = TRUE),
            mean_temp = mean(Temp), mean_wind = mean(Wind),
            .groups = "drop")
cat("\nMonthly summary (data frame):\n"); print(as.data.frame(monthly))

# ---------------------------------------------------------------
# PART 3 : STATISTICAL PROBLEMS  (Q3)
# ---------------------------------------------------------------
cat("\n===== PART 3: STATISTICAL PROBLEMS =====\n")

# 3.1 Missing values
cat("\nMissing values per column:\n"); print(colSums(is.na(air[, 1:6])))
cat("Duplicate rows:", sum(duplicated(air[, 1:6])), "\n")
cat("Complete rows:", sum(complete.cases(air[, 1:6])), "of", nrow(air), "\n")

# 3.2 Outliers (IQR rule)
iqr_outliers <- function(v) {
  q <- quantile(v, c(.25, .75), na.rm = TRUE); r <- diff(q)
  sum(v < q[1] - 1.5 * r | v > q[2] + 1.5 * r, na.rm = TRUE)
}
cat("\nOutliers by IQR rule:\n")
print(sapply(air[, c("Ozone","Solar.R","Wind","Temp")], iqr_outliers))

# 3.3 Normality of Ozone (Shapiro-Wilk) and skewness
sk <- function(v) { v <- v[!is.na(v)]; mean((v - mean(v))^3) / sd(v)^3 }
cat("\nSkewness of Ozone:", round(sk(air$Ozone), 3),
    "| log(Ozone):", round(sk(log(air$Ozone)), 3), "\n")
print(shapiro.test(air$Ozone))
print(shapiro.test(log(air$Ozone[!is.na(air$Ozone)])))

# 3.4 Correlation + significance
cc <- air[complete.cases(air[, 1:6]), ]
cat("\nCorrelation with Ozone (complete cases):\n")
print(round(cor(cc[, c("Ozone","Solar.R","Wind","Temp")]), 3))
cat("\nPearson test Ozone ~ Temp:\n");  print(cor.test(cc$Ozone, cc$Temp))
cat("\nPearson test Ozone ~ Wind:\n");  print(cor.test(cc$Ozone, cc$Wind))

# 3.5 Hypothesis tests / ANOVA
cat("\nANOVA: Ozone by Month\n")
print(summary(aov(Ozone ~ factor(Month), data = cc)))
cat("\nWelch t-test: Ozone in Jul-Aug vs May-Jun-Sep\n")
print(t.test(cc$Ozone[cc$Month %in% 7:8], cc$Ozone[!cc$Month %in% 7:8]))

# 3.6 Multicollinearity (VIF computed by hand)
vif_manual <- function(df, preds) {
  sapply(preds, function(p) {
    r2 <- summary(lm(reformulate(setdiff(preds, p), p), data = df))$r.squared
    1 / (1 - r2)
  })
}
cat("\nVIF:\n"); print(round(vif_manual(cc, c("Solar.R","Wind","Temp")), 3))

# ---------------------------------------------------------------
# PART 4 : DATA CLEANING & VISUALISATION
# ---------------------------------------------------------------
cat("\n===== PART 4: CLEANING & VISUALISATION =====\n")

# Imputation: Solar.R by median; Ozone by month median
# (rows with Ozone missing are kept for imputation of features only;
#  the target is NOT imputed - rows with missing Ozone are dropped
#  for modelling to avoid fabricating the thing we predict)
air$Solar.R[is.na(air$Solar.R)] <- median(air$Solar.R, na.rm = TRUE)
model_df <- air %>% filter(!is.na(Ozone)) %>%
  select(Ozone, Solar.R, Wind, Temp, Month)
cat("Rows used for modelling:", nrow(model_df), "\n")
cat("NA left:", sum(is.na(model_df)), "\n")

p1 <- ggplot(model_df, aes(Ozone)) + geom_histogram(bins = 25, fill = "#2a6f97", colour = "white") +
  ggtitle("Distribution of Ozone (ppb)")
ggsave("plots/01_ozone_hist.png", p1, width = 7, height = 4.5, dpi = 150)

p2 <- ggplot(model_df, aes(factor(Month, labels = c("May","Jun","Jul","Aug","Sep")), Ozone)) +
  geom_boxplot(fill = "#a9d6e5") + xlab("Month") + ggtitle("Ozone by Month")
ggsave("plots/02_ozone_by_month.png", p2, width = 7, height = 4.5, dpi = 150)

png("plots/03_correlation.png", width = 900, height = 800, res = 140)
corrplot(cor(model_df), method = "color", addCoef.col = "black",
         tl.col = "black", number.cex = 0.8)
dev.off()

long_df <- stack(model_df[, c("Ozone","Solar.R","Wind","Temp")])
p3b <- ggplot(long_df, aes(ind, values)) + geom_boxplot(fill = "#a9d6e5") +
  facet_wrap(~ind, scales = "free") + xlab("") + ylab("value") +
  ggtitle("Boxplots of features (outlier check)")
ggsave("plots/03b_boxplots.png", p3b, width = 8, height = 5, dpi = 150)

p4 <- ggplot(model_df, aes(Temp, Ozone)) + geom_point(alpha = .6, colour = "#01497c") +
  geom_smooth(method = "loess", formula = y ~ x, colour = "firebrick") +
  ggtitle("Ozone vs Temperature")
ggsave("plots/04_ozone_vs_temp.png", p4, width = 7, height = 4.5, dpi = 150)

p5 <- ggplot(model_df, aes(Wind, Ozone)) + geom_point(alpha = .6, colour = "#01497c") +
  geom_smooth(method = "loess", formula = y ~ x, colour = "firebrick") +
  ggtitle("Ozone vs Wind Speed")
ggsave("plots/05_ozone_vs_wind.png", p5, width = 7, height = 4.5, dpi = 150)

# ---------------------------------------------------------------
# PART 5 : DATA SPLITTING
# ---------------------------------------------------------------
cat("\n===== PART 5: DATA SPLITTING (80/20) =====\n")
idx   <- createDataPartition(model_df$Ozone, p = 0.8, list = FALSE)
train <- model_df[idx, ]
test  <- model_df[-idx, ]
cat("Train rows:", nrow(train), "| Test rows:", nrow(test), "\n")

metrics <- function(actual, pred)
  c(RMSE = sqrt(mean((actual - pred)^2)),
    MAE  = mean(abs(actual - pred)),
    R2   = cor(actual, pred)^2)

# ---------------------------------------------------------------
# PART 6 : REGRESSION MODELS (Q4)
# ---------------------------------------------------------------
cat("\n===== PART 6: REGRESSION MODELS =====\n")

# Model A: multiple linear regression
lm_a <- lm(Ozone ~ Solar.R + Wind + Temp + factor(Month), data = train)
cat("\n--- Model A: Multiple Linear Regression ---\n"); print(summary(lm_a))
lm_a_pred <- predict(lm_a, newdata = test)

# Model B: linear regression on log(Ozone) - fixes skew / heteroscedasticity
lm_b <- lm(log(Ozone) ~ Solar.R + Wind + Temp, data = train)
cat("\n--- Model B: Linear Regression on log(Ozone) ---\n"); print(summary(lm_b))
lm_b_pred <- exp(predict(lm_b, newdata = test))
cat("\nResidual diagnostics (Model A) - Shapiro on residuals:\n")
print(shapiro.test(residuals(lm_a)))

png("plots/06_lm_diagnostics.png", width = 1100, height = 900, res = 140)
par(mfrow = c(2, 2)); plot(lm_a); par(mfrow = c(1, 1)); dev.off()

# Model C: polynomial / interaction regression
lm_c <- lm(Ozone ~ Solar.R + Wind + Temp + I(Temp^2) + I(Wind^2) + Temp:Wind,
           data = train)
cat("\n--- Model C: Polynomial + Interaction Regression ---\n")
print(summary(lm_c))
lm_c_pred <- predict(lm_c, newdata = test)

# Model D: Random Forest (ranger)
rf <- ranger(Ozone ~ ., data = train, num.trees = 500,
             importance = "permutation", seed = 2026)
rf_pred <- predict(rf, data = test)$predictions
cat("\n--- Model D: Random Forest ---\n"); print(rf)
cat("\nVariable importance (permutation):\n"); print(round(sort(rf$variable.importance, TRUE), 2))

# Model comparison on the held-out test set
cmp <- rbind(
  "Linear (A)"        = metrics(test$Ozone, lm_a_pred),
  "Log-Linear (B)"    = metrics(test$Ozone, lm_b_pred),
  "Poly+Interact (C)" = metrics(test$Ozone, lm_c_pred),
  "Random Forest (D)" = metrics(test$Ozone, rf_pred))
cat("\n===== TEST-SET COMPARISON =====\n"); print(round(cmp, 3))

# 10-fold CV x 5 repeats on ALL modelling rows (more reliable: n is small)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
cv_lm <- train(Ozone ~ Solar.R + Wind + Temp + factor(Month), data = model_df,
               method = "lm", trControl = ctrl)
cv_c  <- train(Ozone ~ Solar.R + Wind + Temp + I(Temp^2) + I(Wind^2) + Temp:Wind,
               data = model_df, method = "lm", trControl = ctrl)
cv_rf <- train(Ozone ~ ., data = model_df, method = "ranger", trControl = ctrl,
               tuneGrid = expand.grid(mtry = 2:4, splitrule = "variance",
                                      min.node.size = 5), num.trees = 300)
cv_tab <- rbind("Linear (A)"        = cv_lm$results[, c("RMSE","Rsquared","MAE")],
                "Poly+Interact (C)" = cv_c$results[,  c("RMSE","Rsquared","MAE")],
                "Random Forest (D)" = cv_rf$results[which.min(cv_rf$results$RMSE),
                                                    c("RMSE","Rsquared","MAE")])
cat("\n===== 10-FOLD x5 CROSS-VALIDATION =====\n"); print(round(cv_tab, 3))

pa <- function(pred, title) {
  d <- data.frame(Observed = test$Ozone, Predicted = pred)
  ggplot(d, aes(Observed, Predicted)) + geom_point(colour = "#01497c", size = 2) +
    geom_abline(colour = "firebrick") + ggtitle(title)
}
ggsave("plots/07_lm_actual_pred.png", pa(lm_a_pred, "Linear Regression: Predicted vs Actual"), width = 6, height = 5, dpi = 150)
ggsave("plots/08_poly_actual_pred.png", pa(lm_c_pred, "Polynomial Regression: Predicted vs Actual"), width = 6, height = 5, dpi = 150)
ggsave("plots/09_rf_actual_pred.png", pa(rf_pred, "Random Forest: Predicted vs Actual"), width = 6, height = 5, dpi = 150)

imp <- data.frame(Variable = names(rf$variable.importance),
                  Importance = as.numeric(rf$variable.importance))
p10 <- ggplot(imp, aes(reorder(Variable, Importance), Importance)) +
  geom_col(fill = "#2a6f97") + coord_flip() + xlab("") +
  ggtitle("Random Forest Variable Importance")
ggsave("plots/10_rf_importance.png", p10, width = 6.5, height = 4, dpi = 150)

# ---------------------------------------------------------------
# PART 7 : LOGISTIC REGRESSION (classification)
# ---------------------------------------------------------------
cat("\n===== PART 7: LOGISTIC REGRESSION (High Ozone day) =====\n")
thr <- 50                                   # ppb threshold for a 'high ozone' day
model_df$HighOzone <- factor(ifelse(model_df$Ozone > thr, 1, 0), levels = c(0, 1))
cat("Class balance (HighOzone, threshold", thr, "ppb):\n"); print(table(model_df$HighOzone))

train_c <- model_df[idx, ]; test_c <- model_df[-idx, ]
# NOTE: Ozone itself is excluded from the predictors (it defines the label)
glm_fit <- glm(HighOzone ~ Solar.R + Wind + Temp, data = train_c, family = binomial)
cat("\n"); print(summary(glm_fit))
cat("\nOdds ratios:\n"); print(round(exp(coef(glm_fit)), 3))
prob <- predict(glm_fit, newdata = test_c, type = "response")
cls  <- factor(ifelse(prob > 0.5, 1, 0), levels = c(0, 1))
cm   <- confusionMatrix(cls, test_c$HighOzone, positive = "1")
print(cm)

roc_df <- do.call(rbind, lapply(seq(0, 1, by = 0.01), function(t) {
  p <- factor(ifelse(prob > t, 1, 0), levels = c(0, 1))
  tp <- sum(p == 1 & test_c$HighOzone == 1); fn <- sum(p == 0 & test_c$HighOzone == 1)
  fp <- sum(p == 1 & test_c$HighOzone == 0); tn <- sum(p == 0 & test_c$HighOzone == 0)
  data.frame(TPR = tp / (tp + fn), FPR = fp / (fp + tn))
}))
auc <- abs(sum(diff(roc_df$FPR) * (head(roc_df$TPR, -1) + tail(roc_df$TPR, -1)) / 2))
cat("\nApprox. AUC on test set:", round(auc, 3), "\n")
ggsave("plots/11_roc.png",
       ggplot(roc_df, aes(FPR, TPR)) + geom_line(colour = "#01497c", linewidth = 1) +
         geom_abline(linetype = "dashed") + ggtitle(sprintf("ROC Curve (AUC = %.3f)", auc)),
       width = 5.5, height = 5, dpi = 150)

# ---------------------------------------------------------------
# PART 8 : PREDICTION ON NEW DATA (Q5)
# ---------------------------------------------------------------
cat("\n===== PART 8: PREDICTING NEW DAYS =====\n")
new_days <- data.frame(Solar.R = c(250, 150, 80),
                       Wind    = c(5,   9,   14),
                       Temp    = c(92,  80,  65),
                       Month   = c(7,   6,   5))
new_days$Pred_Ozone_RF   <- round(predict(rf, data = new_days)$predictions, 1)
new_days$Pred_Ozone_Poly <- round(predict(lm_c, newdata = new_days), 1)
new_days$HighOzone_Prob  <- round(predict(glm_fit, newdata = new_days, type = "response"), 3)
new_days$Category_RF     <- sapply(new_days$Pred_Ozone_RF, ozone_category)
print(new_days)

# One-page chart summary of the complete analysis output
png("plots/12_analysis_dashboard.png", width = 1500, height = 1100, res = 150)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Test-set regression performance
barplot(cmp[, "RMSE"], names.arg = c("Linear", "Log-linear", "Polynomial", "Random forest"),
        col = c("#8ecae6", "#219ebc", "#023047", "#2a9d8f"), cex.names = .8,
        ylab = "RMSE (ppb)", main = "Regression Model Comparison")
abline(h = min(cmp[, "RMSE"]), col = "firebrick", lty = 2)
text(seq_len(nrow(cmp)), cmp[, "RMSE"], labels = round(cmp[, "RMSE"], 1), pos = 3, cex = .8)

# Random-forest drivers
barplot(sort(rf$variable.importance), horiz = TRUE, col = "#2a6f97",
        xlab = "Permutation importance", main = "Ozone Prediction Drivers")

# Classifier result: confusion matrix
cm_table <- as.matrix(cm$table)
image(1:2, 1:2, t(cm_table[nrow(cm_table):1, ]), axes = FALSE,
      col = colorRampPalette(c("#e0f3f8", "#0077b6"))(20),
      xlab = "Reference", ylab = "Prediction", main = sprintf("High-Ozone Classifier (AUC = %.3f)", auc))
axis(1, at = 1:2, labels = colnames(cm_table))
axis(2, at = 1:2, labels = rev(rownames(cm_table)))
for (i in 1:2) for (j in 1:2)
  text(i, 3 - j, cm_table[j, i], cex = 1.3)

# New-weather-scenario output
barplot(t(as.matrix(new_days[, c("Pred_Ozone_RF", "Pred_Ozone_Poly")])), beside = TRUE,
        col = c("#2a6f97", "#f4a261"), names.arg = paste("Scenario", 1:nrow(new_days)),
        ylab = "Predicted ozone (ppb)", main = "Predictions for New Weather Days")
legend("topright", legend = c("Random forest", "Polynomial"),
       fill = c("#2a6f97", "#f4a261"), bty = "n", cex = .85)
dev.off()
cat("\nChart dashboard saved: plots/12_analysis_dashboard.png\n")

cat("\nSession info:\n"); cat(R.version.string, "\n")
