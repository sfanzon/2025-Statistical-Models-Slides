set.seed(21)

# Enter the data
math <- c(36, 40, 46, 54, 57, 58, 59, 60, 62, 63)
acc <- c(37, 37, 42, 44, 46, 48, 54, 56, 59, 60, 60, 64, 64)

# Calculate observed t-statistic
t.obs <- t.test(math, acc, var.equal = T)$statistic

# Compute sample size
n <- length(math)
m <- length(acc)

# Combine the samples
wages <- c(math, acc)

# Bootstrap the t-statistic B = 10,000 times
B <- 10000

t.boot <- replicate(B, {
                    # Single bootsrap sample
                    math.boot <-sample(wages, n, replace = T)
                    acc.boot <- sample(wages, m, replace = T)
                    
                    # Reuturn single bootsrap t-statistic
                    t.test(math.boot, acc.boot, var.equal = T)$statistic
                    })

# Count number of extreme statistics for two-sided test
extreme <- sum ( abs( t.boot ) > abs (t.obs) )

# Compute the p-value
p <- extreme / B

# Print
cat("The bootstrap p-value is:", p)