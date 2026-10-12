# Problem 1

# (1)
# create table
df <- matrix(data=c(14, 6, 7, 7, 7, 1), ncol = 3, byrow = TRUE)
colnames(df) <- c('Not Stopped', 'Bribe requested', 'Stopped/given warning')
rownames(df) <- c('Upper Class', 'Lower Class')

# use build in function
chisq.test(df)

# make my own
df <- addmargins(df)
chi_test <- 0
for (i in 1:(ncol(df)-1)) {
  for (j in 1:(nrow(df)-1)) {
    print(df[j,i])
    chi_test <- chi_test + (df[j,i] - (df[nrow(df),i]/df[nrow(df),ncol(df)] * df[j,ncol(df)]))^2 / (df[nrow(df),i]/df[nrow(df),ncol(df)] * df[j,ncol(df)])
  }
}
chi_test

