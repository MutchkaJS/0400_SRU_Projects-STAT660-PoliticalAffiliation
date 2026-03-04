head(logistic_5_NoOutliers)

df <- data.frame(logistic_5_NoOutliers)

# Examining distribution of Age
par(mfrow = c(2, 2), mar = c(5, 5, 3, 2))  # mar adjusts margins

hist(df$Age, 
     main = "Dist of Age", 
     xlab = "Age (years)", 
     ylab = "Frequency", 
     col = hcl.colors(20, "viridis"),  # Attractive gradient colors
     border = "white",                 # Clean white borders
     breaks = 30,                      # More bins for detail
     cex.main = 1.2,                   # Larger title
     cex.lab = 1.1,                    # Larger axis labels
     las = 1)                          # Horizontal labels for readability

# 1. Enhanced histogram with normal curve overlay
x <- df$Age[!is.na(df$Age)]
h <- hist(x, breaks=30, density=TRUE, col="lightblue", 
          main="Age Dist vs Normal", xlab="Age (years)")
curve(dnorm(x, mean=mean(x), sd=sd(x)), col="red", lwd=2, add=TRUE)

# 2. Q-Q plot (points should follow straight line)
qqnorm(x, main="Q-Q Plot: Age")
qqline(x, col="red", lwd=2)

# 3. Density plot
plot(density(x), main="Age Density", col="blue")

# Shapiro-Wilk test (best for n < 3000)
shapiro.test(x)

# Examining distribution of Income
par(mfrow = c(2, 2), mar = c(5, 5, 3, 2))  # mar adjusts margins

hist(df$Income, 
     main = "Dist of Income", 
     xlab = "Income ($)", 
     ylab = "Frequency", 
     col = hcl.colors(20, "viridis"),  # Attractive gradient colors
     border = "white",                 # Clean white borders
     breaks = 30,                      # More bins for detail
     cex.main = 1.2,                   # Larger title
     cex.lab = 1.1,                    # Larger axis labels
     las = 1)                          # Horizontal labels for readability

# 1. Enhanced histogram with normal curve overlay
x <- df$Income[!is.na(df$Income)]
h <- hist(x, breaks=30, density=TRUE, col="lightblue", 
          main="Income Dist vs Normal", xlab="Income ($)")
curve(dnorm(x, mean=mean(x), sd=sd(x)), col="red", lwd=2, add=TRUE)

# 2. Q-Q plot (points should follow straight line)
qqnorm(x, main="Q-Q Plot: Income")
qqline(x, col="red", lwd=2)

# 3. Density plot
plot(density(x), main="Income Density", col="blue")

# Shapiro-Wilk test (best for n < 3000)
shapiro.test(x)


# Descriptive analysis for Gender, Educational Level, Race and Party
library(dplyr)
library(ggplot2)

# 1. Frequency tables with percentages
cat_vars <- c("Gender", "EdLevel", "Race", "Party")
for(var in cat_vars) {
  print(paste("=== ", var, " ==="))
  tab <- table(df[[var]])
  print(tab)
  print(round(prop.table(tab)*100, 1))
  cat("\n")
}

# 2. Professional bar charts (all on one page)
par(mfrow=c(2,2))
for(var in cat_vars) {
  ggplot(df, aes(x = !!sym(var))) +
    geom_bar(fill = "steelblue", color = "white", alpha = 0.8) +
    geom_text(aes(label = paste0(round(..count../sum(..count..)*100, 1), "%")), 
              stat="count", vjust=-0.5, size=3.5) +
    labs(title = paste("Distribution:", var), 
         x = var, y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45, hjust=1))
}

library(dplyr)
library(knitr)
library(kableExtra)

cat_vars <- c("Gender", "EdLevel", "Race", "Party")

# Create summary data frames for each variable
summary_tables <- lapply(cat_vars, function(var) {
  tab <- table(df[[var]])
  data.frame(
    Category = names(tab),
    Count = as.numeric(tab),
    Percentage = round(as.numeric(prop.table(tab)) * 100, 1)
  ) %>%
    mutate(Percentage = paste0(Percentage, "%"))
})

# Display as professional tables
for(i in seq_along(cat_vars)) {
  cat("\n##", toupper(cat_vars[i]), "\n")
  print(kable(summary_tables[[i]], 
              col.names = c("Category", "Count", "Percentage"),
              digits = 0) %>%
          kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                        full_width = FALSE) %>%
          column_spec(1, bold = TRUE, width = "2cm") %>%
          row_spec(0, bold = TRUE, color = "white", background = "#365a8c"))
  
  # All variables in one publication-ready table
  library(flextable)
  
  combined_summary <- bind_rows(
    summary_tables[[1]] %>% mutate(Variable = cat_vars[1]),
    summary_tables[[2]] %>% mutate(Variable = cat_vars[2]),
    summary_tables[[3]] %>% mutate(Variable = cat_vars[3]),
    summary_tables[[4]] %>% mutate(Variable = cat_vars[4])
  )
  
  combined_summary %>%
    flextable() %>%
    autofit() %>%
    theme_vanilla() %>%
    bold(part = "header") %>%
    bg(bg = "#eff3f8", part = "body") %>%
    fontsize(size = 11) %>%
    set_caption("Univariate Categorical Variable Summary")
}