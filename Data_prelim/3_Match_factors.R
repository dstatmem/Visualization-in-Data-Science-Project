library(dplyr)

# Base from Match table
home_df <- Match_data$Match %>%
  select(id, home_team_api_id, away_team_api_id, date, home_team_goal, away_team_goal) %>%
  rename(
    match_id = id,
    team = home_team_api_id,
    opponent_team_api_id = away_team_api_id,
    goals_scored = home_team_goal,
    goals_conceded = away_team_goal
  ) %>%
  mutate(is_home = TRUE)

away_df <- Match_data$Match %>%
  select(id, home_team_api_id, away_team_api_id, date, home_team_goal, away_team_goal) %>%
  rename(
    match_id = id,
    team = away_team_api_id,
    opponent_team_api_id = home_team_api_id,
    goals_scored = away_team_goal,
    goals_conceded = home_team_goal
  ) %>%
  mutate(is_home = FALSE)

# Combine home and away into one long format
match_base <- bind_rows(home_df, away_df) %>%
  mutate(goal_difference = goals_scored - goals_conceded)


###### cards #######

cards_summary <- Match_data$Match_Cards %>%
  group_by(match_id, team) %>%
  summarise(
    yellow_cards = sum(card_type == "y", na.rm = TRUE),
    red_cards = sum(card_type == "r", na.rm = TRUE),
    .groups = "drop"
  )

cards_summary <- cards_summary %>%
  mutate(
    match_id = as.factor(match_id),
    team = as.numeric(team)
  )

###### Crosses #####

crosses_summary <- Match_data$Match_Cross %>%
  group_by(match_id, team) %>%
  summarise(
    crosses = n(),
    .groups = "drop"
  )

###### corners #######

corners_summary <- Match_data$Match_Corner %>%
  group_by(match_id, team) %>%
  summarise(
    corners = n(),
    .groups = "drop"
  )


###### fouls commited #####

fouls_summary <- Match_data$Match_FoulsCommitted %>%
  group_by(match_id, team) %>%
  summarise(
    fouls_committed = n(),
    .groups = "drop"
  )


##### shots on target #####
shots_on_summary <- Match_data$Match_Shots_On %>%
  group_by(match_id, team) %>%
  summarise(
    shots_on_target = n(),
    .groups = "drop"
  )


##### shots off target #####
shots_off_summary <- Match_data$Match_Shots_Off %>%
  group_by(match_id, team) %>%
  summarise(
    shots_off_target = n(),
    .groups = "drop"
  )



##### Goals difference ####

library(dplyr)

# Count the number of goal events per team per match
team_goals <- Match_data[["Match_Goals"]] %>%
  mutate(
    match_id = match_id,
    team = team
  ) %>%
  group_by(match_id, team) %>%
  summarise(goals = n(), .groups = "drop")

# Self join to get opponent goals
goal_diff_data <- team_goals %>%
  rename(team_api_id = team, team_goals = goals) %>%
  left_join(
    team_goals %>% rename(opponent_team_api_id = team, opponent_goals = goals),
    by = "match_id"
  ) %>%
  filter(team_api_id != opponent_team_api_id) %>%  # Remove self-joins
  group_by(match_id, team_api_id) %>%
  slice(1) %>%  # Avoid duplicate rows (each match appears twice)
  ungroup() %>%
  mutate(goal_difference = team_goals - opponent_goals)


####### Merge match factor data ######
match_factors <- cards_summary %>%
  left_join(crosses_summary, by = c("match_id", "team")) %>%
  left_join(corners_summary, by = c("match_id", "team")) %>%
  left_join(fouls_summary, by = c("match_id", "team")) %>%
  left_join(shots_on_summary, by = c("match_id", "team")) %>%
  left_join(shots_off_summary, by = c("match_id", "team")) %>%
  left_join(goal_diff_data, by = c("match_id" , "team" = "team_api_id"))

summary(match_factors)


# Create unique match_id-date pairs
match_dates <- match_base %>%
  select(match_id = match_id, date) %>%
  distinct(match_id, .keep_all = TRUE)  # Keep only one row per match_id

# join
DF_match_factors <- match_factors %>%
  left_join(match_dates, by = "match_id")

library(dplyr)
library(tidyr)



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

