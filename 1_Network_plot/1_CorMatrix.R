# Example match factor data frame
# Match_Factors should contain: goal_difference, crossing, finishing, possession, etc.
library(dplyr)

# Remove rows with NA
match_factors_clean <- DF_match_factors %>%
  drop_na(yellow_cards, red_cards, crosses, corners, fouls_committed,
          shots_on_target, shots_off_target)


# Only numeric columns
cor_data <- match_factors_clean %>%
  select(goal_difference, yellow_cards, red_cards, crosses, corners,
         fouls_committed, shots_on_target, shots_off_target)

cor_matrix <- cor(cor_data, use = "complete.obs")

# View correlations with goal difference
cor_with_goal_diff <- cor_matrix[, "goal_difference"]


# Select only numeric columns
numeric_data <- match_factors_clean %>%
  select(where(is.numeric))

# Correlation with goal_difference
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Convert to tidy format
cor_goal_diff <- data.frame(
  factor = rownames(cor_matrix),
  correlation = cor_matrix[, "goal_difference"]
) %>%
  filter(factor != "goal_difference") %>%
  mutate(
    abs_corr = abs(correlation),
    color = ifelse(correlation > 0, "green", "red")
  ) %>%
  arrange(desc(abs_corr))
