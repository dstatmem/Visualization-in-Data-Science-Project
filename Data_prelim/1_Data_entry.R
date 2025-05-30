##### This code assumes a map Data within the project ####


# Load necessary packages
library(tidyverse)
library(readr)
library(here)


# File paths
  file_paths <- list(
  Country = "Data/Country.csv",
  League = "Data/League.csv",
  Match_Position = "Data/Match.csv",
  Match_Goals = "Data/Match_Goals.csv",
  Match_Cards = "Data/Match_Cards.csv",
  Match_Corner = "Data/Match_Corner.csv",
  Match_Cross = "Data/Match_Cross.csv",
  Match_FoulsCommitted = "Data/Match_Fouls_Committed.csv",
  Match_Possesion = "Data/Match_Possesion.csv",
  Match_Shots_Off = "Data/Match_Shots_Off.csv",
  Match_Shots_On = "Data/Match_Shots_On.csv",
  Match = "Data/Match.csv",
  Player = "Data/Player.csv",
  Player_Attributes = "Data/Player_Attributes.csv",
  PositionReference = "Data/PositionReference.csv",
  Team = "Data/Team.csv",
  Team_Attributes = "Data/Team_Attributes.csv"
)


# Initialize list for match data
Match_data <- list()

# Load data
for (name in names(file_paths)) {
  df <- read_csv(file_paths[[name]], show_col_types = FALSE)

  # Convert *_id columns to factors
  id_cols <- grep("id$", names(df), value = TRUE)
  df[id_cols] <- lapply(df[id_cols], as.factor)

  # Save Match_ datasets into Match_data list, others as separate objects
  if (startsWith(name, "Match")) {
    Match_data[[name]] <- df
  } else {
    assign(name, df, envir = .GlobalEnv)
  }
}

#### Convert data ########

# date
Match_data$Match <- Match_data$Match %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y %H:%M"))

# player ID
Player$player_api_id <- as.character(Player$player_api_id)
Player_Attributes$player_api_id <- as.character(Player_Attributes$player_api_id)


#### Get club info #####
# Get Málaga CF row
malaga_row <- Team %>% filter(team_long_name == Club) %>% slice(1)

# Create club info list
Club_id <- list(
  Club_name = malaga_row$team_long_name,
  Club_team_id = malaga_row$team_api_id,
  Club_fifa_id = malaga_row$team_fifa_api_id,
  Club_short_name = malaga_row$team_short_name
)


