
# libraries
library(lubridate)
library(dplyr)


##### Merge player data #####
Player_Data <- Player_Attributes %>%
  left_join(Player, by = "player_api_id")


# Join and compute age
Player_Data <- Player_Attributes %>%
  left_join(Player, by = "player_api_id") %>%
  mutate(
    birthday = as.Date(birthday),
    date = as.Date(date),
    age = as.numeric(difftime(date, birthday, units = "days")) / 365.25
  )

Player_Data$age_round <- round(Player_Data$age, 0)


##### Dataframe with goal differences ######
# Home team data
home_df <- Match_data$Match %>%
  select(match_api_id, season, date,
         home_team_api_id, away_team_api_id,
         home_team_goal, away_team_goal) %>%
  rename(
    team_api_id = home_team_api_id,
    goals_scored = home_team_goal,
    goals_conceded = away_team_goal
  ) %>%
  mutate(is_home = TRUE,
         goal_diff = goals_scored - goals_conceded)

# Away team data
away_df <- Match_data$Match %>%
  select(match_api_id, season, date,
         home_team_api_id, away_team_api_id,
         home_team_goal, away_team_goal) %>%
  rename(
    team_api_id = away_team_api_id,
    goals_scored = away_team_goal,
    goals_conceded = home_team_goal
  ) %>%
  mutate(is_home = FALSE,
         goal_diff = goals_scored - goals_conceded)

# Combine
Match_Scores <- bind_rows(home_df, away_df) %>%
  select(match_api_id, season, date, team_api_id, is_home,
         goals_scored, goals_conceded, goal_diff)

# Annotate result as Win (1), Draw (0), Loss (-1)
Match_Scores <- Match_Scores %>%
  mutate(
    match_result = case_when(
      goal_diff > 0 ~ 1,
      goal_diff == 0 ~ 0,
      goal_diff < 0 ~ -1
    )
  )

# Convert 'date' column to Date type and extract year-month
Match_Scores <- Match_Scores %>%
  mutate(
    date = as.Date(date, format = "%d/%m/%Y %H:%M"),
    year_month = format(date, "%Y-%m")
  )

# Calculate win rate per team per month
Monthly_WinRate <- Match_Scores %>%
  group_by(team_api_id, year_month) %>%
  summarise(
    games_played = n(),
    wins = sum(match_result == 1),
    draws = sum(match_result == 0),
    losses = sum(match_result == -1),
    win_rate = wins / games_played
  ) %>%
  ungroup()

library(zoo)  # For rollapply

# Make sure year_month is in date format for ordering
Monthly_WinRate <- Monthly_WinRate %>%
  mutate(year_month_date = as.Date(paste0(year_month, "-01"))) %>%
  arrange(team_api_id, year_month_date)

# Apply rolling average per team
Monthly_WinRate <- Monthly_WinRate %>%
  group_by(team_api_id) %>%
  mutate(
    win_rate_rolling_6 = rollapply(win_rate, width = 6, align = "right", fill = NA, FUN = mean, na.rm = TRUE)
  ) %>%
  ungroup()



