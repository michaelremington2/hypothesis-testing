# Load necessary libraries
library(ggplot2)
library(datasets)
library(pwr)

data(iris)

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  labs(title = "Sepal Length Distribution by Species", x = "Species", y = "Sepal Length (cm)") +
  theme_minimal()

ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(aes(fill = Species), alpha = 0.5, position = 'identity', bins = 10) +
  facet_wrap(~ Species) +
  labs(title = "Distribution of Sepal Lengths by Species", x = "Sepal Length", y = "Count") +
  theme_minimal()

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_bar(stat = "summary", fun = "mean", position = position_dodge(), color = "black") +
  geom_errorbar(stat = "summary", fun.data = mean_sdl, fun.args = list(mult = 1), 
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Sepal Lengths by Species with Standard Deviation",
       x = "Species",
       y = "Sepal Length (cm)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_bar(stat = "summary", fun = "mean", position = position_dodge(), color = "black") +
  geom_errorbar(stat = "summary", fun.data = mean_cl_boot, fun.args = list(conf.int = 0.68), # Using 68% CI to approximate SE
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Sepal Lengths by Species with Standard Error",
       x = "Species",
       y = "Sepal Length (cm)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")


ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_bar(stat = "summary", fun = "mean", position = position_dodge(), color = "black") +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, fun.args = list(conf.int = 0.95), 
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Sepal Lengths by Species with 95% Confidence Intervals",
       x = "Species",
       y = "Sepal Length (cm)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")

for(species in unique(iris$Species)) {
  cat("Shapiro-Wilk Normality Test for", species, "\n")
  print(shapiro.test(iris$Sepal.Length[iris$Species == species]))
  cat("\n")
}

# Note: mean_cl_normal automatically calculates the 95% CI (alpha = 0.05)

anova_result <- aov(Sepal.Length ~ Species, data = iris)
summary(anova_result)

group_size <- nrow(iris) / length(unique(iris$Species))

# Perform power analysis for ANOVA
power_result <- pwr.anova.test(
  k = 3,
  n = group_size,
  f = 0.25,  
  sig.level = 0.05
)

print(power_result)
