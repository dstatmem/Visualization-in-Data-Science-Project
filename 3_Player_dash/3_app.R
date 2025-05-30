library(lubridate)
library(plotly)


# Filter U23
Player_Data_U23 <- Player_Data %>%
  filter(age < 23)

Player_Data_U23_info <- Player_Data %>%
  filter(age < 23) %>%
  group_by(player_api_id) %>%
  slice_max(order_by = age, n = 1, with_ties = FALSE) %>%  # Keep row with max age
  ungroup() %>%
  mutate(age = round(age, 0))

# Simplify radar data
Radar_Data <- Player_Data_U23 %>%
  filter(!is.na(player_name)) %>%
  select(player_name, date, crossing, finishing, short_passing, dribbling, age) %>%
  distinct()

source(file = "Visualization_Rcode/3_Player_dash/3_server.R")
source(file = "Visualization_Rcode/3_Player_dash/3_ui.R")

shinyApp(ui_3, server_3)
