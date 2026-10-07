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

##########################################
# Problem 1
##########################################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
# 1
# find mean and SD to create a CI
mean_y <- mean(y)
s_y <- sd(y)
se <- s_y / sqrt(length(y))
# Mean = 98.44
# Standard Deviation = 13.0929
# Standard Error = 2.6186
# sample size is less than 30, use t-distribution
t_score <- qt(0.95, df = length(y) - 1)
# t_score = 1.711
ci_lower <- mean_y - t_score*se
ci_upper <- mean_y + t_score*se
# 90% Confident Interval is (93.9599, 102.9201)


# 2
# NuLL Hypotheses: The Average student IQ in this school is less or equal to the average 
# IQ score (100) among all the school in the country
# Alternative Hypothesis: The average student IQ in this school greater  than average
# IQ score (100) among all the school in the country

t <- (mean_y - 100) / se
# t = -0.596
# it means the sample mean falls -0.596 below the hypothesized population mean
# since it is a right tail test we set lower.tail = FALSE
p_value <- pt(q=t, df = length(y)-1, lower.tail = FALSE)
# p_value = 0.7215


  
#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)
head(expenditure)
library(ggplot2)

y_label <- "(Y) per capita expenditure on shelters/housing assistance"
x1_label <- "(X1) per capita personal income"
x2_label <- "(X2) Number of residents per 100,000 that are 'financially insecure'"
x3_label <- "(X3) Number of people per thousand residing in urban areas"

# plot relationship between y and x1 using scatter plot
x1_y_scatter <- ggplot(expenditure, aes(x=X1, y=Y)) + 
  geom_point() + 
  labs(
    title = "Relationship between X1 and Y",
    x = "(X1) per capita personal income",
    y = "(Y) per capita expenditure on shelters/housing assistance"
  )
ggsave("x1_y_scatter.pdf", x1_y_scatter, width = 6, height = 4)

# relationship between y and x2
x2_y_scatter <- ggplot(expenditure, aes(x=X2, y=Y)) + 
  geom_point() + 
  labs(
    title = "Relationship between X2 and Y",
    x = x2_label,
    y = y_label
  )
ggsave("x2_y_scatter.pdf", x2_y_scatter, width = 6, height = 4)

# relationship between y and x3
x3_y_scatter <- ggplot(expenditure, aes(x=X3, y=Y)) + 
  geom_point() + 
  labs(
    title = "Relationship between X3 and Y",
    x = x3_label,
    y = y_label
  )
ggsave("x3_y_scatter.pdf", x3_y_scatter, width = 6, height = 4)

# relationship between x1 and x2
x1_x2_scatter <- ggplot(expenditure, aes(x=X1, y=X2)) + 
  geom_point() + 
  labs(
    title = "Relationship between X1 and X2",
    x = x1_label,
    y = x2_label
  )
ggsave("x1_x2_scatter.pdf", x1_x2_scatter, width = 6, height = 4)

# relationship between x1 and x3
x1_x3_scatter <- ggplot(expenditure, aes(x=X1, y=X3)) + 
  geom_point() + 
  labs(
    title = "Relationship between X1 and X3",
    x = x1_label,
    y = x3_label
  )
ggsave("x1_x3_scatter.pdf", x1_x3_scatter, width = 6, height = 4)

# relationship between x2 and x3
x2_x3_scatter <- ggplot(expenditure, aes(x=X2, y=X3)) + 
  geom_point() + 
  labs(
    title = "Relationship between X2 and X3",
    x = x2_label,
    y = x3_label
  )
ggsave("x2_x3_scatter.pdf", x2_x3_scatter, width = 6, height = 4)
#################################################################################

expenditure$Region <- factor(expenditure$Region,
  levels = c(1, 2, 3, 4),
  labels = c("Northeast (1)", "North Central (2)", "South (3)", "West (4)"))

# relationship between Y and Region
y_region_scatter <- ggplot(expenditure, aes(x=Region, y=Y)) + 
  geom_point() + 
  labs(
    title = "Relationship between Y and Region",
    x = "Region",
    y = "(Y) per capita expenditure on shelters/housing assistance"
  )
ggsave("y_region_scatter.pdf", y_region_scatter, width = 6, height = 4)

# Convert the numeric region into a labeled factor


# Use Region as factor to plot box plot.
y_region_box <- ggplot(expenditure, aes(x = Region, y = Y)) +
  geom_boxplot() +
  stat_summary(
    fun = mean,
    geom = "text",
    aes(label = after_stat(round(y,4))),
  ) +
  labs(
    title = "Per Capita Housing Assistance Expenditure by Region",
    x = "Region",
    y = "(Y) per capita expenditure on shelters/housing assistance"
  )

ggsave("y_region_box.pdf", y_region_box, width = 6, height = 4)
###################################################################################### 

# re-creating above relationship between Y and X1, including one more variable Region
y_x1_region <- ggplot(expenditure, aes(x=X1, y=Y,
                              colour = Region, shape = Region)) + 
  geom_point(size = 2) + 
  labs(
    title = "Relationship between Y, X1 and Region",
    x = "(X1) Per capita expenditure on shelters",
    y = "(Y) per capita expenditure on shelters/housing assistance",
    subtitle = "By region",
  )

ggsave("y_x1_region.pdf", y_x1_region, width = 6, height = 4)

pdf("Histogram_of_sample_y.pdf")
hist(x=y, xlab = "IQ")
dev.off()
