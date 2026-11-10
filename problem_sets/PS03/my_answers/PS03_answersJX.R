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

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/incumbents_subset.csv")

#question1 1 Run a regression where the outcome variable is voteshare and the explanatory variable is difflog
regression_question1 <- lm(voteshare~difflog, data = inc.sub)
# get summary of model
summary(regression_question1)
#1111111111111111111111111111111111111111111111111111111111111111111111111111111
#question1 2
library(ggplot2)
scattor_question1 <- ggplot(inc.sub, aes(x = difflog, y = voteshare)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "difflog", y = "voteshare", title = "question1 difflog vs voteshare")
ggsave("scattor_question1.pdf")

#question1 3
res_question1 <- resid(regression_question1)
res_question1

#question1 4
# use the intercept and slope from summary to write prediction equation.

#2222222222222222222222222222222222222222222222222222222222222222222222222222222
#question2 1
regression_question2 <- lm(presvote~difflog, data = inc.sub)
#get a summary of model
summary(regression_question2)

#question2 2
scattor_question2 <- ggplot(inc.sub, aes(x = difflog, y = presvote)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "difflog", y = "presvote", title = "question2 difflog vs presvote")
#scattor_question2
ggsave("scattor_question2.pdf")

#question2 3
res_question2 <- resid(regression_question2)

#question2 4
# use the intercept and slope from summary to write prediction equation.

#3333333333333333333333333333333333333333333333333333333333333333333333333333333
#question3 1
regression_question3 <- lm(voteshare~presvote, data = inc.sub)
#get a summary of model
summary(regression_question3)

#question3 2
scattor_question3 <- ggplot(inc.sub, aes(x = presvote, y = voteshare)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "presvote", y = "voteshare", title = "question3 presvote vs voteshare")
#scattor_question3
ggsave("scattor_question3.pdf")

#question3 3
# use the intercept and slope from summary to write prediction equation.

#4444444444444444444444444444444444444444444444444444444444444444444444444444444
#question4 1
regression_question4 <- lm(res_question1~res_question2)
summary(regression_question4)

#question4 2
scattor_question4 <- ggplot(data.frame(res_question1, res_question2), aes(x = res_question2, y = res_question1)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "q2 residual", y = "q1 residual", title = "q2 residual vs q1 residual")
#scattor_question4
ggsave("scattor_question4.pdf")

#5555555555555555555555555555555555555555555555555555555555555555555555555555555
#question5 1
regression_question5 <- lm(voteshare~difflog+presvote, data = inc.sub)
summary(regression_question5)

#question5 