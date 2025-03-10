set.seed(21)

# Enter the data
math <- c(36, 40, 46, 54, 57, 58, 59, 60, 62, 63)
acc <- c(37, 37, 42, 44, 46, 48, 54, 56, 59, 60, 60, 64, 64)

# Compare variances: in the example acc has larger variance
# Larger variance has to be at numerator

# Calculate observed F-statistic (samples are swapped)
F.obs <- var.test(acc, math)$statistic

# Compute sample size (samples are swapped)
n <- length(acc)
m <- length(math)

# Combine the centered samples
wages <- c(acc - mean(acc), math - mean(math))

# Bootstrap the F-statistic B = 10,000 times
B <- 10000

F.boot <- replicate(B, {
                    # Single bootsrap sample
                    acc.boot <-sample(wages, n, replace = T)
                    math.boot <- sample(wages, m, replace = T)
                    
                    # Return single bootsrap F-statistic
                    var(acc.boot) / var(math.boot)
})

# Count number of extreme statistics for two-sided test
extreme <- sum ( (F.boot > F.obs) | (F.boot < 1/F.obs) )

# Compute the p-value
p <- extreme / B

# Print
cat("The bootstrap p-value is:", p)