### data set preparation

# install.packages("haven")
# install.packages("plm")

library(haven)
library(dplyr)

# set working directory

data <- read_dta("data.dta")

# basic data description

ls(data)
head(data, n=10)
summary(data)
glimpse(data)

# filter application

data_filtered <- data %>%
  filter(fic == "USA")

data_filtered <- data_filtered %>%
  filter(!is.na(dltt) &!is.na(dlc) &!is.na(at) &!is.na(ebitda) &!is.na(ppent) &!is.na(ceq) &!is.na(prcc_c) &!is.na(csho) &!is.na(age))

data_filtered <- subset(data_filtered, dltt >= 0 & dlc >= 0 & at >= 0 & prcc_c >= 0 & csho >= 0 & age >= 0)

# create variables: 'leverage', 'profitability', 'tangibility', 'market_to_book_value', 'lassets', 'lage'

leverage <- (data_filtered$dltt + data_filtered$dlc) / data_filtered$at
profitability <- data_filtered$ebitda / data_filtered$at
tangibility <- data_filtered$ppent / data_filtered$at
market_to_book_value <- (data_filtered$at - data_filtered$ceq + (data_filtered$prcc_c * data_filtered$csho)) / data_filtered$at
lassets <- log(data_filtered$at)
lage <- log(data_filtered$age)

### task a) 

# sub question => total assets and age instead of 'lassets' and 'lage'

# calculate means

means <- c(mean(leverage, na.rm = TRUE),
           mean(profitability, na.rm = TRUE),
           mean(tangibility, na.rm = TRUE),
           mean(market_to_book_value, na.rm = TRUE),
           mean(lassets, na.rm = TRUE),
           mean(data_filtered$at, na.rm = TRUE),
           mean(lage, na.rm = TRUE),
           mean(data_filtered$age, na.rm = TRUE)
)

# determine 90th percentiles

percentiles_90th <- c(quantile(leverage, probs = 0.9, na.rm = TRUE),
                      quantile(profitability, probs = 0.9, na.rm = TRUE),
                      quantile(tangibility, probs = 0.9, na.rm = TRUE),
                      quantile(market_to_book_value, probs = 0.9, na.rm = TRUE),
                      quantile(lassets, probs = 0.1, na.rm = TRUE),
                      quantile(data_filtered$at, probs = 0.9, na.rm = TRUE),
                      quantile(lage, probs = 0.1, na.rm = TRUE),
                      quantile(data_filtered$age ,probs = 0.9, na.rm = TRUE)
)

# determine 10th percentiles

percentiles_10th <- c(quantile(leverage, probs = 0.1, na.rm = TRUE),
                      quantile(profitability, probs = 0.1, na.rm = TRUE),
                      quantile(tangibility, probs = 0.1, na.rm = TRUE),
                      quantile(market_to_book_value, probs = 0.1, na.rm = TRUE),
                      quantile(lassets, probs = 0.9, na.rm = TRUE),
                      quantile(data_filtered$at, probs = 0.1, na.rm = TRUE),
                      quantile(lage, probs = 0.9, na.rm = TRUE),
                      quantile(data_filtered$age ,probs = 0.1, na.rm = TRUE)
)

# determine medians

medians <- c(median(leverage, na.rm = TRUE),
             median(profitability, na.rm = TRUE),
             median(tangibility, na.rm = TRUE),
             median(market_to_book_value, na.rm = TRUE),
             median(lassets, na.rm = TRUE),
             median(data_filtered$at, na.rm = TRUE),
             median(lage, na.rm = TRUE),
             median(data_filtered$age, na.rm = TRUE)
)

# calculate standard deviations

std_deviations <- c(sd(leverage, na.rm = TRUE),
                    sd(profitability, na.rm = TRUE),
                    sd(tangibility, na.rm = TRUE),
                    sd(market_to_book_value, na.rm = TRUE),
                    sd(lassets, na.rm = TRUE),
                    sd(data_filtered$at, na.rm = TRUE),
                    sd(lage, na.rm = TRUE),
                    sd(data_filtered$age, na.rm = TRUE)
)

# find largest values

largest_values <- c(max(leverage, na.rm = TRUE),
                    max(profitability, na.rm = TRUE),
                    max(tangibility, na.rm = TRUE),
                    max(market_to_book_value, na.rm = TRUE),
                    max(lassets, na.rm = TRUE),
                    max(data_filtered$at, na.rm = TRUE),
                    max(lage, na.rm = TRUE),
                    max(data_filtered$age, na.rm = TRUE)
)

# find smallest values

smallest_values <- c(min(leverage, na.rm = TRUE),
                     min(profitability, na.rm = TRUE),
                     min(tangibility, na.rm = TRUE),
                     min(market_to_book_value, na.rm = TRUE),
                     min(lassets, na.rm = TRUE),
                     min(data_filtered$at, na.rm = TRUE),
                     min(lage, na.rm = TRUE),
                     min(data_filtered$age, na.rm = TRUE)
)

# find largest and smallest value for variable 'unrated'

largest_values_unrated <- c(max(leverage[data_filtered$unrated == 1], na.rm = TRUE),
                            max(profitability[data_filtered$unrated == 1], na.rm = TRUE),
                            max(tangibility[data_filtered$unrated == 1], na.rm = TRUE),
                            max(market_to_book_value[data_filtered$unrated == 1], na.rm = TRUE),
                            max(lassets[data_filtered$unrated == 1], na.rm = TRUE),
                            max(data_filtered$at[data_filtered$unrated == 1], na.rm = TRUE),
                            max(lage[data_filtered$unrated == 1], na.rm = TRUE),
                            max(data_filtered$age[data_filtered$unrated == 1], na.rm = TRUE)
)

smallest_values_unrated <- c(min(leverage[data_filtered$unrated == 1], na.rm = TRUE),
                             min(profitability[data_filtered$unrated == 1], na.rm = TRUE),
                             min(tangibility[data_filtered$unrated == 1], na.rm = TRUE),
                             min(market_to_book_value[data_filtered$unrated == 1], na.rm = TRUE),
                             min(lassets[data_filtered$unrated == 1], na.rm = TRUE),
                             min(data_filtered$at[data_filtered$unrated == 1], na.rm = TRUE),
                             min(lage[data_filtered$unrated == 1], na.rm = TRUE),
                             min(data_filtered$age[data_filtered$unrated == 1], na.rm = TRUE)
)

# compute number of observations and firms

num_observations <- nrow(data_filtered)
cat("Number of observations:", num_observations, "\n")

num_firms <- length(unique(data_filtered$firmid))
cat("Number of firms:", num_firms, "\n")

# create data frame and show result table

result_table_a <- data.frame(Variables = c("leverage", "profitability", "tangibility", "market_to_book_value", "log_total_assets", "total_assets","log_age", "age"), Mean = means, Median = medians, Tenth_Percentile = percentiles_10th, Ninetieth_Percentile = percentiles_90th, Standard_Deviation = std_deviations, Largest_Value = largest_values, Largest_Value_Unrated = largest_values_unrated, Smallest_Value = smallest_values, Smallest_Value_Unrated = smallest_values_unrated)
print(result_table_a)

# export result table a => csv file

# write.csv(result_table_a, file = "result_table_a.csv", row.names = FALSE)

### task b)

# create samples for rated and unrated firms

rated_firms <- data_filtered[data_filtered$unrated == 0, ]
unrated_firms <- data_filtered[data_filtered$unrated == 1, ]

# compute number of rated and unrated firms

num_rated_firms <- nrow(rated_firms)
cat("Number of rated firms:", num_rated_firms, "\n")

num_unrated_firms <- nrow(unrated_firms)
cat("Number of unrated firms:", num_unrated_firms, "\n")

# extend initial data set by variables: 'leverage', 'profitability', 'tangibility', 'market_to_book_value', 'lassets', 'lage'

data_filtered <- data_filtered %>%
  mutate(
    leverage = (data_filtered$dltt + data_filtered$dlc) / data_filtered$at,
    profitability = data_filtered$ebitda / data_filtered$at,
    tangibility = data_filtered$ppent / data_filtered$at,
    market_to_book_value = (data_filtered$at - data_filtered$ceq + (data_filtered$prcc_c * data_filtered$csho)) / data_filtered$at,
    lassets = log(data_filtered$at),
    lage = log(data_filtered$age)
  )

# apply t-test to variables

t_test_leverage <- t.test(data_filtered$leverage ~ data_filtered$unrated, data = data_filtered,var.equal = TRUE )
t_test_profitability <- t.test(data_filtered$profitability ~ data_filtered$unrated, data = data_filtered,var.equal = TRUE )
t_test_tangibility <- t.test(data_filtered$tangibility ~ data_filtered$unrated, data = data_filtered,var.equal = TRUE )
t_test_market_to_book_value <- t.test(data_filtered$market_to_book_value ~ data_filtered$unrated, data = data_filtered,var.equal = TRUE )
t_test_lassets <- t.test(data_filtered$lassets ~ data_filtered$unrated, data = data_filtered,var.equal = TRUE )
t_test_lage <- t.test(data_filtered$lage ~ data_filtered$unrated, data = data_filtered,var.equal = TRUE )

# store results of t-test in vectors

mean_diff_b <- c(t_test_leverage$estimate[2], t_test_profitability$estimate[2], t_test_tangibility$estimate[2],
                 t_test_market_to_book_value$estimate[2], t_test_lassets$estimate[2], t_test_lage$estimate[2])

t_stat_b <- c(t_test_leverage$statistic, t_test_profitability$statistic, t_test_tangibility$statistic,
              t_test_market_to_book_value$statistic, t_test_lassets$statistic, t_test_lage$statistic)

p_value_b <- c(t_test_leverage$p.value, t_test_profitability$p.value, t_test_tangibility$p.value,
               t_test_market_to_book_value$p.value, t_test_lassets$p.value, t_test_lage$p.value)

# create data frame 

result_table_b <- data.frame(
  Variable = character(),
  Mean_Difference = numeric(),
  T_Statistic = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

# fill data frame with mean_diff, t-stat. and p-values

result_table_b <- rbind(result_table_b,
                        data.frame(
                          Variable = "Leverage",
                          Mean_Difference = mean_diff_b[1],
                          T_Statistic = t_stat_b[1],
                          P_Value = p_value_b[1]
                        ),
                        data.frame(
                          Variable = "Profitability",
                          Mean_Difference = mean_diff_b[2],
                          T_Statistic = t_stat_b[2],
                          P_Value = p_value_b[2]
                        ),
                        data.frame(
                          Variable = "Tangibility",
                          Mean_Difference = mean_diff_b[3],
                          T_Statistic = t_stat_b[3],
                          P_Value = p_value_b[3]
                        ),
                        data.frame(
                          Variable = "Market_To_Book_Value",
                          Mean_Difference = mean_diff_b[4],
                          T_Statistic = t_stat_b[4],
                          P_Value = p_value_b[4]
                        ),
                        data.frame(
                          Variable = "LAssets",
                          Mean_Difference = mean_diff_b[5],
                          T_Statistic = t_stat_b[5],
                          P_Value = p_value_b[5]
                        ),
                        data.frame(
                          Variable = "LAge",
                          Mean_Difference = mean_diff_b[6],
                          T_Statistic = t_stat_b[6],
                          P_Value = p_value_b[6]
                        )
)

# add significance to result table b

result_table_b$Significance <- ifelse(result_table_b$P_Value < 0.001, "***",
                                      ifelse(result_table_b$P_Value < 0.01, "**",
                                             ifelse(result_table_b$P_Value < 0.05, "*", ""))
)

rownames(result_table_b) <- NULL
print(result_table_b)

# export result table b => csv file

# write.csv(result_table_b, file = "result_table_b.csv", row.names = FALSE)

### task c)

# regression of 'leverage' (dependent variable) on 'unrated' (independent variable)

reg_model_c <- lm(leverage ~ unrated, data = data_filtered)
print(summary(reg_model_c))

# extract slope and p-value for 'unrated'

slope_c <- coef(reg_model_c)["unrated"]
p_value_c <- summary(reg_model_c)$coefficients["unrated", "Pr(>|t|)"]

# determine statistical significance of 'unrated'

if (p_value_c < 0.05) {
  result_c <- "The estimated slope is statistically different from zero."
} else {
  result_c <- "The estimated slope is not statistically different from zero."
}

# print results of regression c

cat("Estimated Slope:", slope_c, "\n")
cat("P-value:", p_value_c, "\n")
cat(result_c, "\n")

### task d)

# regression of 'leverage' (dependent variable) on 'unrated' and 'tangibility' (independent variables)

# discussion => sign, statistical and economic significance of 'tangibility' - coefficient 

reg_model_d <- lm(leverage ~ unrated + tangibility, data = data_filtered)
print(summary(reg_model_d))

# extract coefficients for 'unrated' and 'tangibility' 

coef_unrated_d <- coef(reg_model_d)["unrated"]
coef_tangibility_d <- coef(reg_model_d)["tangibility"]

# calculate economic significance for variable 'tangibility'

sd_tangibility <- sd(data_filtered$tangibility)
one_sd_change <- sd_tangibility

economic_significance <- coef_tangibility_d * one_sd_change

# determine statistical significance of 'tangibility'

p_value_tangibility_d <- summary(reg_model_d)$coefficients["tangibility", "Pr(>|t|)"]

if (p_value_tangibility_d < 0.05) {
  result_significance_d <- "the coefficient of 'tangibility' is statistically significant."
} else {
  result_significance_d <- "the coefficient of 'tangibility' is not statistically significant."
}

# determine signs of coefficients for 'tangibility' and 'unrated'

if (coef_tangibility_d > 0) {
  sign_tangibility <- "positive"
} else if (coef_tangibility_d < 0) {
  sign_tangibility <- "negative"
} else {
  sign_tangibility <- "zero"
}

if (coef_unrated_d > 0) {
  sign_unrated <- "positive"
} else if (coef_unrated_d < 0) {
  sign_unrated <- "negative"
} else {
  sign_unrated <- "zero"
}

# check consistency of results task c) and d)

coef_unrated_univariate <- slope_c
p_value_univariate <- p_value_c

coef_unrated_multivariate <- coef_unrated_d
p_value_multivariate <- p_value_tangibility_d

if (abs(coef_unrated_univariate) < abs(coef_unrated_multivariate) && p_value_univariate < p_value_multivariate) {
  consistency_result_d <- "The results are consistent. The coefficient of 'unrated' is larger in the multivariate regression and its p-value is smaller than in the univariate regression."
} else if (abs(coef_unrated_univariate) > abs(coef_unrated_multivariate) && p_value_univariate > p_value_multivariate) {
  consistency_result_d <- "The results are consistent. The coefficient of 'unrated' is smaller in the multivariate regression and its p-value is larger than in the univariate regression."
} else {
  consistency_result_d <- "The results are not consistent."
}

# show results of task d) 

cat("Coefficient associated with 'tangibility':", coef_tangibility_d, "\n")
cat("Statistical significance of 'tangibility' - coefficient:", p_value_tangibility_d, "\n")
cat("Economic significance of 'tangibility' - coefficient:", economic_significance, "\n")
cat("The coefficient associated with 'tangibility' is", sign_tangibility, "and", result_significance_d, "\n")
cat("The coefficient associated with 'unrated' is ", sign_unrated,".\n", sep ="")
cat("Consistency check between univariate and multivariate tests:\n")
cat(consistency_result_d, "\n")

### task e)

# regression of 'leverage' (dependent variable) on 'unrated' and 'tangibility' (independent variables)

reg_model_e <- lm(leverage ~ tangibility * unrated, data = data_filtered)
summary(reg_model_e)

# perform anova on regression model e

anova_result_e <- anova(reg_model_e)

# extract f-statistic and p-value from anova

f_statistic_e <- anova_result_e$F[2]
p_value_e <- anova_result_e$`Pr(>F)`[2]

# display f-statistic and p-value to console

cat("F-statistic:", f_statistic_e, "\n")
cat("P-value:", p_value_e, "\n")

# determine statistical significance of 'tangibility' - coefficient for rated and unrated firms

if (p_value_e < 0.05) {
  result <- "The relationship between 'leverage' and 'tangibility' is different for rated and unrated firms (statistically significant)."
} else {
  result_e <- "There is no significant difference in the relationship between 'leverage' and 'tangibility' for rated and unrated firms."
}

# display result of statistical test 

cat(result_e, "\n")

### task f)

# regression of 'leverage' (dependent variable) on 'unrated' and 'tangibility' (independent variables)

reg_model_f <- lm(leverage ~ tangibility * unrated, data = data_filtered)
summary(reg_model_f)

# perform anova on regression model f

anova_result_f <- anova(reg_model_f)

# extract f-statistic and p-value from anova

f_statistic_f <- anova_result_f$F[3]
p_value_f <- anova_result_f$`Pr(>F)`[3]

# output f-statistic and p-value 

cat("F-Statistic:", f_statistic_f, "\n")
cat("P-value:", p_value_f, "\n")
cat("\n")

# conduct hypothesis - test for variable 'unrated' based on p-value (95% significance)

if (p_value_f < 0.05) {
  result_f <- "Reject the null hypothesis. 'Unrated' has a significant effect on 'leverage'."
} else {
  result_f <- "Accept the null hypothesis. 'Unrated' has no significant effect on 'leverage'."
}

cat(result_f, "\n")

# extract p-value for 'tangibility' - coefficient Task d)

p_value_tangibility_d <- summary(reg_model_d)$coefficients["tangibility", "Pr(>|t|)"]

# p-value of interaction term between 'tangibility' and 'unrated' task e)

p_value_interaction <- anova_result_f$`Pr(>F)`[3]

# output for results of task d) and e)

cat("Results for Task d):\n")
cat("P-value (Tangibility):", p_value_tangibility_d, "\n")
cat("\n")
cat("Results for Task e):\n")
cat("P-value (Interaction):", p_value_interaction, "\n")
cat("\n")

# conduct hypothesis - test for the interaction term (95% significance)

if (p_value_interaction < 0.05) {
  result_comparison <- "The interaction term is statistically significant in Task e)."
} else {
  result_comparison <- "The interaction term is not statistically significant in Task e)."
}

# conduct hypothesis test for 'tangibility' - coefficient task d)

if (p_value_tangibility_d < 0.05) {
  result_comparison_f <- paste(result_comparison, "The tangibility coefficient is statistically significant in Task d).")
} else {
  result_comparison_f <- paste(result_comparison, "The tangibility coefficient is not statistically significant in Task d).")
}

# display results of comparison between task d) and e)

cat("Comparison of results:\n")
cat(result_comparison_f, "\n")

### Task g)

# estimated regression equations for rated and unrated firms

# diagram of 'leverage' (y‐axis) on 'tangibility' (x‐axis)

# extract coefficients from regression model f

library(ggplot2)

intercept_rated <- coef(reg_model_f)["(Intercept)"]
slope_rated <- coef(reg_model_f)["tangibility"]
intercept_unrated <- coef(reg_model_f)["(Intercept)"] + coef(reg_model_f)["unrated"]
slope_unrated <- coef(reg_model_f)["tangibility"] + coef(reg_model_f)["tangibility:unrated"]

# create sequence of values for 'tangibility' (x-axis)

tangibility_values <- seq(min(data_filtered$tangibility), max(data_filtered$tangibility), length.out = 100)

# calculate y-values ('leverage') for rated and unrated firms

leverage_rated <- intercept_rated + slope_rated * tangibility_values
leverage_unrated <- intercept_unrated + slope_unrated * tangibility_values

# plot data points and regression lines

plot(data_filtered$tangibility, data_filtered$leverage, xlab = "Tangibility", ylab = "Leverage", col = ifelse(data_filtered$unrated == 1, "red", "blue"), pch = 16)
lines(tangibility_values, leverage_rated, col = "blue")
lines(tangibility_values, leverage_unrated, col = "red")

# add a legend to plot => rated and unrated firms

legend("topleft", legend = c("Rated Firms", "Unrated Firms"), col = c("blue", "red"), pch = 16, bty = "n")

# print values of intercept and slope as equations for rated and unrated firms

cat("Rated Firms: y =", round(intercept_rated, 2), "+", round(slope_rated, 2), "* β\n")
cat("Unrated Firms:  y =", round(intercept_unrated, 2), "+", round(slope_unrated, 2), "* β\n")

# ggsave("[...].png", width = 10, height = 6, units = "in", dpi = 300)

### task h)

# fit regression model with additional variables: 'profitability', 'market_to_book_value', 'lassets', 'lage'

reg_model_h <- lm(leverage ~ unrated + tangibility + profitability + market_to_book_value + lassets + lage, data = data_filtered)

# extract coefficients, standard errors and p-values

coefficients_h <- coef(summary(reg_model_h))
std_errors_h <- coefficients_h[, "Std. Error"]
p_values_h <- coefficients_h[, "Pr(>|t|)"]
coefficients_df <- as.data.frame(coefficients_h)
variable_names <- rownames(coefficients_df)

# evaluate statistical significance of each variable

for (i in seq_along(variable_names)) {
  variable_h <- variable_names[i]
  std_error_h <- std_errors_h[i]
  p_value_h <- p_values_h[i]
  if (p_value_h < 0.05) {
    cat(sprintf("Variable '%s' is statistically significant. (p-value: %.15f)\n", variable_h, p_value_h))
  } else {
    cat(sprintf("Variable '%s' is not statistically significant. (p-value: %.15f)\n", variable_h, p_value_h))
  }
}

# create data frame for result table

result_table_h <- data.frame(
  Variable = row.names(coefficients_h),
  Coefficient = coefficients_h[, "Estimate"],
  Std_Error = std_errors_h,
  P_Value = p_values_h
)

# add significance to result table h

result_table_h$Significance <- ifelse(result_table_h$P_Value < 0.001, "***",
                                      ifelse(result_table_h$P_Value < 0.01, "**",
                                             ifelse(result_table_h$P_Value < 0.05, "*", "")))
print(result_table_h)

# export result table b => csv file

# write.csv(result_table_h, file = "result_table_h.csv", row.names = FALSE)

### task i)

data_filtered$sic <- as.character(data_filtered$sic)

# create new variable 'sector' by extracting the first two digits from 'sic' - code

data_filtered$sector <- substr(data_filtered$sic, start=1, stop=2)

# create dummy variables for 'year' and 'sector'

# generate matrix => dummy variables for each category 

year_dummies <- model.matrix(~ -1 + factor(data_filtered$fyear))
sector_dummies <- model.matrix(~ -1 + factor(data_filtered$sector))

# combine dummy variables with original data set

data_filtered <- cbind(data_filtered, year_dummies, sector_dummies)

# fit regression model with dummy variables: 'year', 'sector'

reg_model_i <- lm(leverage ~ unrated + tangibility + profitability + market_to_book_value + lassets + lage + year_dummies + sector_dummies, data = data_filtered)

# extract coefficients, standard errors and p-values

coefficients_i <- coef(summary(reg_model_i))
print(coefficients_i)
std_errors_i <- coefficients_i[, "Std. Error"]
p_values_i <- coefficients_i[, "Pr(>|t|)"]

result_table_i <- data.frame(
  Variable = row.names(coefficients_i),
  Coefficient = coefficients_i[, "Estimate"],
  Std_Error = std_errors_i,
  P_Value = p_values_i
)

# add significance to result table

result_table_i$Significance <- ifelse(result_table_i$P_Value < 0.001, "***",
                                      ifelse(result_table_i$P_Value < 0.01, "**",
                                             ifelse(result_table_i$P_Value < 0.05, "*", "")))
print(result_table_i)

# export result table i => csv file

# write.csv(result_table_i, file = "result_table_i.csv", row.names = FALSE)

### task j)

# install.packages("sandwich")
# install.packages("lmtest")
# install.packages("jtools")

# fit regression model with robust standard errors

library(jtools)
library(sandwich)
library(lmtest)

reg_model_j <- lm(leverage ~ unrated + tangibility + profitability + market_to_book_value + lassets + lage, data = data_filtered)

# perform white test => test for heteroscedasticity

bptest(reg_model_j,
       ~ unrated * tangibility +
         unrated * profitability +
         unrated * market_to_book_value +
         unrated * lassets +
         unrated * lage +
         tangibility * profitability +
         tangibility * market_to_book_value +
         tangibility * lassets +
         tangibility * lage +
         profitability * market_to_book_value +
         profitability * lassets +
         profitability * lage +
         market_to_book_value * lassets +
         market_to_book_value * lage +
         lassets * lage +
         I(unrated^2) +
         I(tangibility^2) +
         I(profitability^2) +
         I(market_to_book_value^2) +
         I(lassets^2) +
         I(lage^2),
       data = data_filtered)

# obtain robust standard errors (heteroskedasticity and firm level clusters)

robust_se_clustered <- sqrt(diag(vcovCL(reg_model_j, cluster = data_filtered$firm_id, type = "HC1")))

# perform wald test => assess significance of 'unrated' - coefficient 
 
wald_test_clustered <- coeftest(reg_model_j, vcov = vcovCL(reg_model_j, cluster = data_filtered$firm_id, type = "HC1"))
print(wald_test_clustered)
coefficients_j <- coef(reg_model_j)
coeftest(reg_model_j, vcov. = vcovHC(reg_model_j, type = 'HC1'))

print(coefficients_j)
print(summary(reg_model_j))

sm1 <- summary(reg_model_j)
sm2 <- coeftest(reg_model_j, vcov. = vcovHC(reg_model_j))
print(sm1)
print(sm2)

# receive p-values using usual and robust standard errors

c(usual = sm1$coefficients[4,4],
  robust = sm2[4,4])

# create data frame for result table

result_table_j <- data.frame(
  Coefficients = coefficients_j,
  StandardErrors = summary(reg_model_j)$coefficients[, "Std. Error"],
  ClusterRobustSE = robust_se_clustered
)

print(result_table_j)

# export result table j => csv file

write.csv(result_table_j, file = "result_table_j.csv", row.names = FALSE)

print(wald_test_clustered)
print(robust_se_clustered)

# extract coefficient, t-value and p-value for variable 'unrated'

unrated_coefficient <- wald_test_clustered["unrated", "Estimate"]
unrated_t_value <- wald_test_clustered["unrated", "t value"]
unrated_p_value <- wald_test_clustered["unrated", "Pr(>|t|)"]

# show results and discuss significance

cat("Coefficient of unrated:", unrated_coefficient, "\n")
cat("t-value for unrated:", unrated_t_value, "\n")
cat("p-value for unrated:", unrated_p_value, "\n\n")
cat("Discussion:\n")
cat("The coefficient for 'unrated' represents the average change in the dependent variable 'leverage' when a firm is unrated, holding other factors constant.\n")
if (abs(unrated_t_value) > 1.96) {
  cat("Given the t-value of ", unrated_t_value, ", this effect is statistically significant at the 5% level.\n", sep = "")
} else {
  cat("Given the t-value of ", unrated_t_value, ", this effect is not statistically significant at the 5% level.\n", sep = "")
}
