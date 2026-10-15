# Problem 1

# (a)
# create table
df <- matrix(data=c(14, 6, 7, 7, 7, 1), ncol = 3, byrow = TRUE)
colnames(df) <- c('Not Stopped', 'Bribe requested', 'Stopped/given warning')
rownames(df) <- c('Upper Class', 'Lower Class')

# use build in function
X <- chisq.test(df)

# make my own
df_margin <- addmargins(df)
chi_test <- 0
for (i in 1:(ncol(df_margin)-1)) {
  for (j in 1:(nrow(df_margin)-1)) {
    #print(df_margin[j,i])
    chi_test <- chi_test + (df_margin[j,i] - (df_margin[nrow(df_margin),i]/df_margin[nrow(df_margin),ncol(df_margin)] * df_margin[j,ncol(df_margin)]))^2 / (df_margin[nrow(df_margin),i]/df_margin[nrow(df_margin),ncol(df_margin)] * df_margin[j,ncol(df_margin)])
  }
}
chi_test

# (b)
P <- pchisq(chi_test, df=2, lower.tail = FALSE)
P1 <- X$p.value
# p-value = 0.1502

find_std_residual <- function(df) {
  #out <- 0
  std_residual <- matrix(, nrow = nrow(df), ncol = ncol(df))
  df_margin <- addmargins(df)
  for (c in 1:(ncol(df_margin)-1)) {
    for (r in 1:(nrow(df_margin)-1)) {
      #print(df_margin[j,c])
      observed <- df_margin[r,c]
      row_total <- df_margin[r,ncol(df_margin)]
      col_total <- df_margin[nrow(df_margin),c]
      total <- df_margin[nrow(df_margin),ncol(df_margin)]
      
      expected <- (row_total*col_total)/total
      row_prop <- row_total/total
      col_prop <- col_total/total
      
      z <- (observed-expected)/sqrt(expected*(1-row_prop)*(1-col_prop))
      #print(observed)
      #print(row_total)
      #print(col_total)
      #print(total)
      #print(expected)
      #print(z)
      #out <- out + ((observed-expected)^2)/ expected
      print(z)
      std_residual[r,c] <- z
    }
  }
  return(std_residual)
}

std_residual <- find_std_residual(df)


## Problem 2
#(a)

df <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv", header=T)
summary(df)

cor.test(df$reserved, df$water)
lm(water~reserved, data = df)
