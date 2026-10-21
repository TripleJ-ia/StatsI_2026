# Problem 1

# (a)
# create table
df <- matrix(data=c(14, 6, 7, 7, 7, 1), ncol = 3, byrow = TRUE)
colnames(df) <- c('Not Stopped', 'Bribe requested', 'Stopped/given warning')
rownames(df) <- c('Upper Class', 'Lower Class')

# make my own
df_margin <- addmargins(df)
chi_test <- 0
for (c in 1:(ncol(df_margin)-1)) {
  for (r in 1:(nrow(df_margin)-1)) {
    #print(df_margin[r,c])
    # chi_test <- chi_test + (df_margin[r,c] - (df_margin[nrow(df_margin),c]/df_margin[nrow(df_margin),ncol(df_margin)] * df_margin[r,ncol(df_margin)]))^2 / (df_margin[nrow(df_margin),c]/df_margin[nrow(df_margin),ncol(df_margin)] * df_margin[r,ncol(df_margin)])
    observed <- df_margin[r,c]
    row_total <- df_margin[r,ncol(df_margin)]
    col_total <- df_margin[nrow(df_margin),c]
    total <- df_margin[nrow(df_margin),ncol(df_margin)]
    expected <- (row_total*col_total)/total
    
    chi_test <- chi_test + ((observed-expected)^2)/expected
    }
}
chi_test
# Chi-square = 3.791168

# use build in function to check
X <- chisq.test(df)

# (b)
P <- pchisq(chi_test, df=(2-1)*(3-1), lower.tail = FALSE)
P1 <- X$p.value
P1
# p-value = 0.1502306

# (c)
find_std_residual <- function(df) {
  #out <- 0
  std_residual <- matrix(, nrow = nrow(df), ncol = ncol(df))
  df_margin <- addmargins(df)
  for (c in 1:(ncol(df_margin)-1)) {
    for (r in 1:(nrow(df_margin)-1)) {
      #print(df_margin[r,c])
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
#(b)
df_2 <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv", header=T)
summary(df_2)

pdf("reserved_water.pdf")
plot(df_2$reserved, df_2$water,xlab = "Reserved", ylab = "Water")
dev.off()

reg <- cor.test(df_2$reserved, df_2$water)
reg$p.value
# pvalue = 0.01970398

#(c)
lm(water~reserved, data = df_2)
