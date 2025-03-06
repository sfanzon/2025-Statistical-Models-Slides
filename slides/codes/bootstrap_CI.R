# Enter original sample and compute size
x <- c(36, 40, 46, 54, 57, 58, 59, 60, 62, 63)
n <- length(x)

# Bootstrap sample mean B = 10,000 times
B <- 10000

xbar.star <- replicate(B, {
                       x.star <- sample(x, n, replace = TRUE)
                       xbar.star <- mean(x.star)
                      })

# Compute 95% CI from bootstrap samples
alpha <- 0.05
boot.CI <- quantile(xbar.star, probs = c(alpha/2, 1-alpha/2))

# Compute 95% t-test CI
t.test.CI <- t.test(x)$conf.int

# Print results
cat("Bootstrap Confidence Interval (95%):", boot.CI)
cat("\nt-test Confidence Interval (95%):", t.test.CI)