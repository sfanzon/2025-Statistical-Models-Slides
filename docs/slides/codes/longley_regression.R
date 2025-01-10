# Remember to save data on Desktop

# Change working directory to Desktop
# In MacOS type
# setwd("~/Desktop")

# In Windows type
# setwd("C:/Users/YourUsername/Desktop")

# Read data file
longley <- read.table(file = "longley.txt",
                      header = TRUE)

# Store columns in vectors
x2 <- longley[ , 1]
x3 <- longley[ , 2]
x4 <- longley[ , 3]
x5 <- longley[ , 4]
x6 <- longley[ , 5]
y <- longley[ , 6]

# Fit multiple regression model
model <- lm(y ~ x2 + x3 + x4 + x5 + x6)

# Print summary
print(summary(model))