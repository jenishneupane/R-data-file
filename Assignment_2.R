# Null hypothesis: p = 0.5
# Alternative hypothesis: p != 0.5

# Observed values
n <- 29164578 # total population
x <- 14253551 # male population

# Expected values under null hypothesis
p <- 0.5
mu <- n * p
sigma <- sqrt(n * p * (1 - p))

# Likelihood ratio test
LR <- 2 * (log(dbinom(x, n, p)) - log(dbinom(x, n, mu/sigma^2)))
p_value <- pchisq(LR, df = 1, lower.tail = FALSE)

# Print results
cat("LR test statistic: ", LR, "\n")
cat("P-value: ", p_value, "\n")
