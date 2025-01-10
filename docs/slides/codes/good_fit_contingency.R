# Store matrix of counts into single R vector
counts <- c(27, 9, 15,
            54, 25, 24,
            84, 32, 28, 
            91, 37, 40,
            11, 10, 8,
            61, 12, 28)

# Store matrix of null probabilities into single R vector
# We use repeat to avoid copy pasting 
rows_null_probs <- c(51/1788, 103/1788, 144/1788, 
                      168/1788, 29/1788, 101/1788)

null_probs <- rep(rows_null_probs, c(3, 3, 3, 3, 3, 3))

# Perform goodness-of-fit test
ans <- chisq.test(counts, p = null_probs)

# Print answer
print(ans)