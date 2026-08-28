# 2024_DS_MACHINE_LEARNING_PRACTICAL_PROGRAMS
# Load required libraries
library(ISLR2)      # For datasets (College, Auto, Boston, Carseats)
library(ggplot2)    # For plotting
library(GGally)     # For scatterplot matrices (ggpairs)

# ==========================================
# PROBLEM 1: Basic Functions (College Dataset)[cite: 1]
# ==========================================
data("College")

# 1. Generating a numerical summary[cite: 1]
summary(College)

# 2. Visualizing pairwise relationships for select variables[cite: 1]
pairs(College[, 1:5])

# 3. Creating a new qualitative variable 'Elite'[cite: 1]
Elite <- factor(ifelse(College$Top10perc > 50, "Yes", "No"))
College <- data.frame(College, Elite)
summary(College$Elite)

# 4. Generating histograms with differing numbers of bins[cite: 1]
par(mfrow = c(1, 2))
hist(College$Apps, breaks = 10, main = "Apps (10 Bins)", xlab = "Apps", col = "skyblue")
hist(College$Apps, breaks = 30, main = "Apps (30 Bins)", xlab = "Apps", col = "coral")
par(mfrow = c(1, 1))


# ==========================================
# PROBLEM 2: EDA using Auto Dataset[cite: 1]
# ==========================================
data("Auto")
Auto <- na.omit(Auto)

# 1. Summary statistics (range, mean, std) for quantitative variables[cite: 1]
quant_vars <- Auto[, 1:7]
summary_table <- data.frame(
  Mean = colMeans(quant_vars),
  SD = apply(quant_vars, 2, sd),
  Min = apply(quant_vars, 2, min),
  Max = apply(quant_vars, 2, max),
  Range = apply(quant_vars, 2, function(x) diff(range(x)))
)
print("Summary Statistics:")
print(summary_table)

# 2. Subsetting data[cite: 1]
auto_subset <- subset(Auto, horsepower > 100)

# 3. Graphical investigation of relationships (focusing on mpg)[cite: 1]
plot(Auto$horsepower, Auto$mpg, xlab = "Horsepower", ylab = "MPG", main = "MPG vs Horsepower", pch = 19, col = "blue")
abline(lm(mpg ~ horsepower, data = Auto), col = "red", lwd = 2)


# ==========================================
# PROBLEM 3: Linear Regression using Boston Dataset[cite: 1]
# ==========================================
data("Boston")

# 1 & 2. Simple Linear Regression for each predictor against 'crim'[cite: 1]
predictors <- names(Boston)[names(Boston) != "crim"]
for (pred in predictors) {
  formula <- as.formula(paste("crim ~", pred))
  fit <- lm(formula, data = Boston)
  cat("\n--- Simple Linear Regression: crim vs", pred, "---\n")
  print(summary(fit)$coefficients)
}

# 3. Multiple Linear Regression using all predictors[cite: 1]
fit_boston_multi <- lm(crim ~ ., data = Boston)
cat("\n--- Boston Multiple Linear Regression ---\n")
summary(fit_boston_multi)


# ==========================================
# PROBLEM 4: Multiple Linear Regression using Auto Dataset[cite: 1]
# ==========================================
# 1. Scatterplot matrix & Correlation matrix (excluding 'name')[cite: 1]
auto_num <- Auto[, -which(names(Auto) == "name")]
pairs(auto_num)
cor_matrix <- cor(auto_num)
print("Correlation Matrix:")
print(cor_matrix)

# 2. Fit Multiple Linear Regression[cite: 1]
fit_auto <- lm(mpg ~ ., data = auto_num)
summary(fit_auto)

# 3. Diagnostic plots (Residuals vs Fitted, Leverage)[cite: 1]
par(mfrow = c(2, 2))
plot(fit_auto)
par(mfrow = c(1, 1))


# ==========================================
# PROBLEM 5: Multiple Linear Regression using Carseats Dataset[cite: 1]
# ==========================================
data("Carseats")

# i. Fitting a multiple linear regression model[cite: 1]
fit_carseats <- lm(Sales ~ ., data = Carseats)

# ii & iv. Interpretation and identifying statistically significant predictors[cite: 1]
summary(fit_carseats)

# v. Refitting a reduced model using only significant predictors[cite: 1]
fit_carseats_red <- lm(Sales ~ CompPrice + Income + Advertising + Price + ShelveLoc + Age, data = Carseats)
summary(fit_carseats_red)


# ==========================================
# PROBLEM 6: Logistic Regression using Auto Dataset[cite: 1]
# ==========================================
# i. Creating binary response variable mpg01[cite: 1]
mpg01 <- ifelse(Auto$mpg > median(Auto$mpg), 1, 0)
Auto_class <- data.frame(Auto, mpg01)

# ii. Graphically identifying relevant predictors[cite: 1]
par(mfrow = c(1, 2))
boxplot(horsepower ~ mpg01, data = Auto_class, main = "Horsepower by mpg01", xlab = "mpg01", ylab = "Horsepower")
boxplot(weight ~ mpg01, data = Auto_class, main = "Weight by mpg01", xlab = "mpg01", ylab = "Weight")
par(mfrow = c(1, 1))

# iii. Splitting data into training and test sets[cite: 1]
set.seed(42)
train_indices <- sample(1:nrow(Auto_class), nrow(Auto_class) * 0.7)
train_data <- Auto_class[train_indices, ]
test_data  <- Auto_class[-train_indices, ]

# iv. Fitting Logistic Regression model and evaluating test error rate[cite: 1]
fit_log <- glm(mpg01 ~ cylinders + displacement + horsepower + weight, data = train_data, family = binomial)
probs <- predict(fit_log, newdata = test_data, type = "response")
preds <- ifelse(probs > 0.5, 1, 0)

test_error <- mean(preds != test_data$mpg01)
cat("Test Error Rate:", test_error, "\n")


# ==========================================
# PROBLEM 7: Classification using Boston Dataset[cite: 1]
# ==========================================
# Response: crim above or below median[cite: 1]
crim01 <- ifelse(Boston$crim > median(Boston$crim), 1, 0)
Boston_class <- data.frame(Boston, crim01)

# Feature subsets (1-indexed matching column pairs)[cite: 1]
predictors_list <- names(Boston)[names(Boston) != "crim"]
combinations <- list(c(1, 5), c(1, 2), c(3, 4), c(5, 6), c(2, 4), c(6, 7))[cite: 1]

set.seed(42)
train_idx <- sample(1:nrow(Boston_class), nrow(Boston_class) * 0.7)
b_train <- Boston_class[train_idx, ]
b_test  <- Boston_class[-train_idx, ]

cat("\n--- Boston Subset Classification Performance ---\n")
for (pair in combinations) {
  var1 <- predictors_list[pair[1]]
  var2 <- predictors_list[pair[2]]
  
  form <- as.formula(paste("crim01 ~", var1, "+", var2))
  fit_sub <- glm(form, data = b_train, family = binomial)
  
  probs_sub <- predict(fit_sub, newdata = b_test, type = "response")
  preds_sub <- ifelse(probs_sub > 0.5, 1, 0)
  acc <- mean(preds_sub == b_test$crim01)
  
  cat(sprintf("Features [%s, %s] -> Test Accuracy: %.4f\n", var1, var2, acc))
}
