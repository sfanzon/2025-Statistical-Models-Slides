# Enter counts and null hypothesis probabilities
counts <- c(35, 40 , 25)
null_probs <- c(0.35, 0.35, 0.3)

# Compute total counts
m <- sum(counts)

# Compute degrees of freedom
degrees <- length(counts) - 1

# Compute expected counts
exp_counts <- m * null_probs

# Compute chi-squared statistic
chi_squared <- sum( (counts - exp_counts)^2 / exp_counts )

# Compute p-value
p_value <- 1 - pchisq(chi_squared, df = degrees)

# Print p-value
cat("The p-value is:", p_value)