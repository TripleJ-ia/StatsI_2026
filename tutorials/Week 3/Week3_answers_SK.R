# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Load any necessary packages
lapply(c("readr", "ggplot2", "dplyr", "viridis", "foreign", "haven"),  pkgTest)

# Set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Agenda
# (a.) Descriptive analysis
# (b.) Confidence intervals
# (c.) Significance test for a mean
# (d.) Significance test for a difference in means

### Research Question -----------
# Is there a relationship between education and income?

# -------------------------------#
# 2. Load & Inspect Data
# -------------------------------#

df <- read_csv("D:/Teaching Fellow_ASDS_2026/Applied Stats I_2026/GitHub/StatsI_2026/datasets/fictional_data.csv")

# Quick overview
head(df)
str(df)
summary(df)

# Variables:
# - income: Monthly net income (numeric)
# - edu: University-level education in years (numeric)
# - cap: Binary variable (1 = lives in capital, 0 = otherwise)

# Quick recap: 

# Find the mean, variance, standard deviation and standard error of income
mean(df$income)
var(df$income)
sd(df$income)
sd(df$income)/sqrt(length(df$income))
# Your answer here:
# Mean = 1860
# Variance = 464588.9
# Standard deviation =681.6076
# Standard error = sd(x)/sqrt(length(x)) = 156.3715

# -------------------------------#
# 3. Visualizing the Distribution
# -------------------------------#

# Histogram of income
hist(df$income,
     #breaks = 20,
     main = "Monthly net income",
     xlab = "Euro")

# Density plot of income
plot(density(df$income),
     main = "Monthly net income",
     xlab = "Euro")

# -----------------------------------------#
# 4. Sampling Distribution & Standard Error
# -----------------------------------------#
# Which kind of inferences can we make with regards to the population,
# based on the sample data?

# Sample mean is our estimate of the population mean

# Standard error estimates the SD of the sampling distribution

# Why do we need the standard error?
# To calculate measures of uncertainty for our point estimate
# (e.g., confidence intervals and p-values)

# -------------------------------#
# 5. Confidence Intervals
# -------------------------------#
# Definition: Point estimate +/- Margin of error, 
# where margin of error is a multiple of the standard error

# Point estimate
mean_income <- mean(df$income)

# Standard error
se_income <- sd(df$income) / sqrt(length(df$income))

# How do we find the multiple?

# ---- 95% Confidence Interval (Approximate) ----
# Looking at the normal distribution, we see that
# 95% of observations lie within ±1.96 (approx. 2)
# standard errors of the point estimate

# ---- 95% Confidence Interval  (Precise) ----
lower_95_n <- qnorm(0.025,
                    mean = mean(df$income),
                    sd   = se_income)

upper_95_n <- qnorm(0.975,
                    mean = mean(df$income),
                    sd   = se_income)

lower_95_n
mean_income
upper_95_n

# Let's talk about qnorm()
?qnorm
qnorm(0.025) # value for first 2.5%
qnorm(0.975) # value last 2.5%
qnorm(0.025, mean=2, sd=0.4) # Change mean and standard error

# ---- 99% Confidence Interval (t Distribution) ----
# t distribution is used when the sample size is small
t_score <- qt(0.995, df = length(df$income) - 1)

lower_99_t <- mean_income - t_score * se_income
upper_99_t <- mean_income + t_score * se_income

# The same but full formula
lower_99_t <- mean_income-(t_score)*(sd(df$income)/sqrt(length(df$income)))
upper_99_t <- mean_income+(t_score)*(sd(df$income)/sqrt(length(df$income)))

lower_99_t
mean_income
upper_99_t

# -------------------------------#
# 6. Significance Tests
# -------------------------------#

# In statistics, a **significance test** checks whether an observed sample
# could plausibly have come from a population with a hypothesized parameter value.
# Here we focus on:
#   (a) Testing a single population mean
#   (b) Testing the difference between two group means

# ---------------------------------------------#
# Question:
# Is the average monthly income in our sample
# different from the population mean in Ireland (from Google: 3034)?

# Hypotheses: one or two-sided? 
# Answer: two-sided
#   H0: Average monthly income is 3034 (mu is equal to 3034)
#   H1: Average monthly income is not 3034 (mu != 3034)

# The t-test compares the sample mean to the hypothesized value mu0,
# accounting for sample size and variability.

# Two-sided test: is the mean different (higher OR lower)?
t.test(df$income, mu = 3034)

# Help page (shows arguments, e.g. alternative = "less"/"greater")
?t.test

# One-sided test: is the mean LESS than 3034?
t.test(df$income, mu = 3034, alternative = "less")

# NOTE:
# - p-value < 0.05 : reject H0 (mean likely differs from 3034)
# - p-value ≥ 0.05 : do not reject H0 (sample mean compatible with 3034)

# The t.test() output also provides a confidence interval by default.
# We can change the confidence level easily:
t.test(df$income, mu = 3034, conf.level = 0.99)

# Double-check: our manually calculated 99% t-interval should match
lower_99_t
mean_income
upper_99_t

# So, we also found a much easier way to calculate the confidence intervals!
t.test(df$income, conf.level = 0.99, alternative = "two.sided")

# ---------------------------------------------#
# Question:
#   Do people living in the capital earn different
#   incomes than those living elsewhere?
#
# Hypotheses: one or two-sided?
#   H0: People living in a capital do not earn a different income than the rest. 
#   H1: People living in a capital earn a different income than the rest. 

# The two-sample t-test compares the means of two independent groups.
# By default, t.test() uses Welch’s t-test, which does NOT assume equal variances.


# Quick descriptive check: group means
mean(df[df$cap == 0, ]$income)  # Non-capital
mean(df[df$cap == 1, ]$income)  # Capital

# Subsetting step-by-step: 
df$cap                   # see the variable
df$cap == 0              # logical test: TRUE/FALSE
df[df$cap == 0, ]        # keep only non-capital rows
df[df$cap == 0, ]$income # select income column
mean(df[df$cap == 0, ]$income)

# Two-sample t-test (Welch)
t.test(df$income ~ df$cap, alternative = "two.sided")

# On average, do people earn more in the capital
# compared to people who do not reside in the capital?
# One-sided test: 
t.test(df$income ~ df$cap, alternative = "less")

# Interpretation:
# - If p-value < 0.05 : reject H0 (means differ significantly)
# - If alternative = "less" and p < 0.05 : non-capital income is significantly lower
#   than capital income.

# On average, do people earn more in the capital
# compared to people who do not reside in the capital?

# -----------------------------------------------------------#
### Extra activity with real-world data (difference in means): 
# -----------------------------------------------------------#

# Goal: Test whether mean Polity scores differ between
#       Eastern Europe vs Western Europe & North America.
# Data: polity.dta — Polity score (0–10), higher = more democratic.

# Why not load("polity.dta")?
# - load() is for .RData/.rda (R’s serialized objects), not Stata files.
# - Use haven::read_dta() for .dta files.
data <- read.dta("D:/Teaching Fellow_ASDS_2026/Applied Stats I_2026/GitHub/StatsI_2026/datasets/polity.dta")

# Quick look
head(data)
glimpse(data)
table(data$region)

# Variable of interest: fh_polity2 - numeric Polity score (0-10)

# Subset the two regions of interest:
west <- data$fh_polity2[data$region == "Western Europe and North America"]
east <- data$fh_polity2[data$region == "Eastern Europe"]

# Quick descriptive statistics - careful for missing values! 
mean_west <- mean(west)
mean_east <- mean(east)
n_west    <- length(west)
n_east    <- length(east)
sd_west   <- sd(west)
sd_east   <- sd(east)
  
mean_west; mean_east
n_west; n_east
sd_west; sd_east

# Calculate the SEs
# SE = sample SD / sqrt(n)
se_west <- sd_west/sqrt(n_west)
se_east <- sd_east/sqrt(n_east)
  
se_west; se_east

# -------------------------------------#
#  Analytical CI (Normal Approximation)
# -------------------------------------#

# By hand using:
#   Diff = mean_west - mean_east
#   SE_diff = sqrt(Var_west/n_west + Var_east/n_east)
#   95% CI = Diff ± 1.96 * SE_diff

se_diff  <- sqrt((sd_west^2 / n_west) + (sd_east^2 / n_east))
diff_hat <- mean_west - mean_east

ci_low_analytic <- diff_hat - 1.96 * se_diff
ci_up_analytic  <- diff_hat + 1.96 * se_diff
ci_analytic     <- c(ci_low_analytic, ci_up_analytic)

diff_hat
se_diff
ci_analytic

# ------------------------#
#  Welch Two-Sample t-test
# ------------------------#

t_test_res <- t.test(west, east)  # two-sided Welch test
t_test_res

# Extract the CI the tidy way (matches the test above)
ci_t <- t_test_res$conf.int[1:2]
ci_t

# Conclusion?