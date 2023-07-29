# Total population
N <- 29164578

# Male population
n <- 14253551

# Hypothesized proportion of male population
p0 <- 0.5

# Calculate test statistic
LR <- 2 * (n * log(n/N) + (N - n) * log((N - n)/N) - N * log(1/2))

# Calculate p-value
pval <- 1 - pchisq(LR, df = 1)

# Print results
cat("LR test statistic:", LR, "\n")
cat("p-value:", pval, "\n")
if (pval < 0.05) {
  cat("Reject null hypothesis at the 5% level\n")
} else {
  cat("Fail to reject null hypothesis at the 5% level\n")
}
