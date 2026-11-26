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
lapply(c("car"),  pkgTest)
#install.packages('car')
library(carData)
library('car')
data(Prestige)
help(Prestige)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Question 1 a
head(Prestige)
summary(Prestige)
Prestige$professionals <- ifelse(Prestige$type == 'prof', 1, 0)

# Question 1 b
model1 <- lm(prestige ~ income + professionals + income:professionals, data=Prestige)
summary(model1)

# Question 1 g
income <- 6000
non_professional <-  21.1422589 + 0.0031709*income +37.7812800*0 - 0.0023257*income*(0)
professional <- 21.1422589 + 0.0031709*income +37.7812800*1 - 0.0023257*income*(1)
change_in_y <- professional-non_professional

# Question 2 a
t1 <- 2.625
df <- 128
2*pt(t1, df, lower.tail = FALSE) # two tail

# Question 2 b
t2 <- 0.042/0.013
df <- 128
2*pt(t2, df, lower.tail = FALSE) # two tail
