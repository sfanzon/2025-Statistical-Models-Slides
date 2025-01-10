# Enter the data
height <- c(100, 200, 300, 450, 600, 800, 1000)
distance <- c(253, 337, 395, 451, 495, 534, 573)

# Scatter plot of data
plot(height, distance, pch = 16)

# Fit linear model
linear <- lm(distance ~ height)
print(summary(linear))

# Fit quadratic model
quadratic <- lm(distance ~ height + I( height^2 ))
print(summary(quadratic))

# Fit cubic model
cubic <- lm(distance ~ height + I( height^2 ) + I (height^3))
print(summary(cubic))

# Fit quartic model
quartic <- lm(distance ~ height + I( height^2 ) + I (height^3) + I (height^4))
print(summary(quartic))

# Model selection: quadratic VS cubic
selection.1 <- anova(quadratic, cubic, test = "F")
print(selection.1)

# Model selection: cubic VS quartic
selection.2 <- anova(cubic, quartic, test = "F")
print(selection.2)

# Plot quadratic Vs Cubic
polynomial <- Vectorize(function(x, ps) {
  n <- length(ps)
  sum(ps*x^(1:n-1))
}, "x")
curve(polynomial(x, coef(quadratic)), add=TRUE, col = "red")
curve(polynomial(x, coef(cubic)), add=TRUE, col = "blue", lty = 2)
legend("topleft", legend = c("quadratic", "cubic"), 
       col = c("red", "blue"), lty = c(1,2))