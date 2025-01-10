# Remember to save data on Desktop

# Change working directory to Desktop
# In MacOS type
# setwd("~/Desktop")

# In Windows type
# setwd("C:/Users/YourUsername/Desktop")

# Load dataset
sales <- read.table(file = "fridge_sales.txt",
                    header = TRUE)

# Assign data-frame columns to vectors
fridge <- sales[ , 1]
durables <- sales[ , 2]
q1 <- sales[ , 3]
q2 <- sales[ , 4]
q3 <- sales[ , 5]
q4 <- sales[ , 6]
quarter <- sales[ , 7]

# Fit dummy variable regression model
# We drop q1 to avoid dummy variable trap
dummy.lm <- lm (fridge ~ q2 + q3 + q4)

# Fit equivalent dummy variable regression model
# Need to convert quarter to a factor
quarter.f <- factor(quarter)
factor.lm <- lm(fridge ~ quarter.f)

print(summary(dummy.lm))
print(summary(factor.lm))

# Fit ANOVA model
factor.aov <- aov(fridge ~ quarter.f)

# Print output
print(summary(factor.aov))