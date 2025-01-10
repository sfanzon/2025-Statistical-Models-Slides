# Enter counts and null hypothesis probabilities
counts <- c(32, 15, 9, 4)
null_probs <- c(0.5, 0.3, 0.15, 0.05)

# Compute total counts
m <- sum(counts)

# Compute expected counts
exp_counts <- m * null_probs

# Compute the observed chi-squared statistic
obs_chi_squared <- sum( (counts - exp_counts)^2 / exp_counts )

# Number of Monte Carlo simulations
N <- 100000

# Initialize counter for extreme statistics
count_extreme_statistics <- 0

# Perform Monte Carlo simulations

for (i in 1:N) {
  # Simulate multinomial counts under null hypothesis
  simul_counts <- rmultinom(1, m, null_probs)
  
  # Compute chi-squared statistic for the simulated counts
  simul_chi_squared <- sum( (simul_counts - exp_counts )^2 / exp_counts)
  
  # Check if simulated chi-squared statistic is extreme
  if (simul_chi_squared >= obs_chi_squared) {
    count_extreme_statistics <- count_extreme_statistics + 1
  }
}

# Compute Monte Carlo p-value
monte_carlo_p_value <- count_extreme_statistics / N

# Perform chi-squared test using built-in R function
chi_squared_test <- chisq.test(counts, p = null_probs, 
                               simulate.p.value = TRUE)

# Extract p-value from chi-squared test result
chisq_p_value <- chi_squared_test$p.value

# Print p-values for comparison
cat("\nCustom Monte Carlo p-value:", monte_carlo_p_value)
cat("\nR Monte Carlo p-value:", chisq_p_value)