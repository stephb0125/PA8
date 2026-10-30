#Stephanie Johnson
# 10/29/2026
# COP 2073
# Module 8

# Create a data frame using the provided cookie data
df <- data.frame(
  RecipeA = c(0, 4, 4, 5, 4, 1, 2, 3, 3, 5),
  RecipeB = c(7, 6, 4, 2, 3, 0, 8, 6, 2, 3),
  RecipeC = c(1, 0, 1, 0, NA, 1, 1, NA, 1, 0)
)

# Stack data and remove missing values
newdf <- na.omit(stack(df))

# Display  stacked data frame
print(newdf)

# summarize  stacked data
initial_summary <- summary(newdf)
print(initial_summary)

# Perform a one-way ANOVA
# Response variable values (zombie transformation score)
# Predictor variable ind (cookie recipe)
# Null hypothesis (H0) all three recipe means are equal
# Alternative hypothesis (H1) at least one mean is different

anova_result <- aov(values ~ ind, data = newdf)

# Summarize the ANOVA results
anova_summary <- summary(anova_result)
print(anova_summary)

# extract and display the pcvalue
pval <- anova_summary[[1]]$`Pr(>F)`[1]
cat("p_value is", pval, "\n")

# determine if i need to reject the null hypothesis
# use a significance level of 0.05
alpha <- 0.05

if (pval < alpha) {
  cat("Sufficient evidence to reject the null hypothesis.\n")
  cat("Ingredient X may have an effect on the results.\n")
} else {
  cat("Insufficient evidence to reject the null hypothesis.\n")
  cat("No significant difference was detected among recipes.\n")
}

