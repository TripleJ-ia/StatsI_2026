#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
# 1
# find mean and SD to create a CI
mean_y <- mean(y)
s_y <- sd(y)

# at 90% Confidence interval, df = 25-1 = 24, t-statistic is 1.711 for two-tail probabilities.
# first find standard error
se <- s_y / sqrt(length(y))
t <- 1.711
ci_lower <- mean_y - t*se
ci_upper <- mean_y + t*se
confident_interval <- c(ci_lower, ci_upper)

ci <- t.test(y, conf.level = 0.90)$conf.int
ci

# 2
# NuLL Hypotheses: The Average student IQ in this school is less or equal to the average 
# IQ score (100) among all the school in the country
# Alternative Hypothesis: The average student IQ in this school greater  than average
# IQ score (100) among all the school in the country

t <- (mean_y - 100) / se
# t = -0.596 means the sample mean falls -0.596 below the hypothesized population mean
# since it is a right tail test we set lower.tail = FALSE
p_value <- pt(q=t, df = length(y)-1, lower.tail = FALSE)
p_value


  
#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)
