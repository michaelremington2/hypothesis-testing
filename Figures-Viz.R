library(dplyr)
library(ggplot2)

#set.seed(42)

n_samples <- 10  
mean_yield_no_pesticides <- 50  
std_dev_yield_no_pesticides <- 10  
mean_yield_with_pesticides <- 60 
std_dev_yield_with_pesticides <- 8


yield_no_pesticides <- rnorm(n_samples, mean_yield_no_pesticides, std_dev_yield_no_pesticides)
yield_with_pesticides <- rnorm(n_samples, mean_yield_with_pesticides, std_dev_yield_with_pesticides)

df_garden <- data.frame(
  Yield = c(yield_no_pesticides, yield_with_pesticides),
  Treatment = c(rep("No Pesticides", n_samples), rep("With Pesticides", n_samples))
)

head(df_garden)

mean_yields <- aggregate(Yield ~ Treatment, data = df_garden, FUN = mean)

ggplot(df_garden, aes(x=Treatment, y=Yield, color=Treatment)) +
  # geom_errorbarh(data = mean_yields, aes(xmin = Treatment, xmax = Treatment, y = Yield, height = 0.1),
  #                color = "blue", size = 4, alpha = 0.8) +  
  geom_jitter(width = 0.2, size = 2, alpha = 0.5) +  
  stat_summary(fun.y=mean, geom="point", shape=23, size=4, color="blue", fill="blue") + 
  theme_minimal() +  # Use a minimal theme
  labs(title = "Plant Yields with and without Pesticides", x = "Treatment", y = "Yield") +
  scale_color_manual(values = c("No Pesticides" = "red", "With Pesticides" = "green"))+
  ylim(25,75)

#########################################
##### P Values
#########################################


curve_df <- data.frame(x = seq(-4, 4, length.out = 1000))
curve_df$y <- dnorm(curve_df$x)


z_score_right <- qnorm(0.95)


ggplot(curve_df, aes(x = x, y = y)) + 
  geom_line() +  # Plot the normal distribution curve
  geom_area(data = subset(curve_df, x > z_score_right), aes(x = x, y = y), fill = "red", alpha = 0.5) +  # Highlight critical region (right tail)
  geom_area(data = subset(curve_df, x <= z_score_right), aes(x = x, y = y), fill = "blue", alpha = 0.5) +  # Highlight non-critical region
  geom_vline(xintercept = z_score_right, linetype = "dashed", color = "black", size = 0.5) +  # Add dashed line for the critical value
  annotate("text", x = z_score_right + 0.51, y = 0.18, label = "α = 0.05", size = 4) +
  annotate("text", x = -2, y = 0.2, label = "Non-Critical Region", size = 4) +
  labs(title = "Alpha = 0.05", x = "Z-Score", y = "Density") +
  theme_minimal()
