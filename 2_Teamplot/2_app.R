#install.packages(c("shiny", "dplyr", "ggplot2", "lubridate", "scales"))

library(scales)
library(lubridate)

# team ID
our_team_id <- 10260

# --- UI ---
ui_2 <- fluidPage(
  titlePanel("Team Performance Trends"),

  # Dropdown above the plots
  fluidRow(
    column(
      width = 12,
      # Centered dropdown
      div(
        style = "display: flex; justify-content: center; margin-bottom: 20px;",
        selectInput(
          "match_factor",
          "Select Match Factor:",
          choices = c("Crosses" = "crosses",
                      "Shots on Target" = "shots_on_target",
                      "Fouls Committed" = "fouls_committed",
                      "Corners" = "corners"),
          selected = "crosses",
          width = "300px"
        )
      )
    )
  ),

  # Two-column layout for plots
  fluidRow(
    column(
      width = 6,
      plotOutput("trendPlot", height = "500px")
    ),
    column(
      width = 6,
      plotOutput("heatmapPlaceholder", height = "500px")
    )
  )
)


draw_pitch <- function() {
  list(
    # Pitch outline and center line
    geom_rect(aes(xmin = 0, xmax = 100, ymin = 0, ymax = 100), fill = "palegreen4", color = "white", size = 1),
    annotate("segment", x = 50, xend = 50, y = 0, yend = 100, color = "white"),

    # Center circle
    annotate("path",
             x = 50 + 10 * cos(seq(0, 2 * pi, length.out = 100)),
             y = 50 + 10 * sin(seq(0, 2 * pi, length.out = 100)),
             color = "white", size = 0.5),

    # Penalty areas
    geom_rect(aes(xmin = 0, xmax = 16.5, ymin = 21.1, ymax = 78.9), color = "white", fill = NA),
    geom_rect(aes(xmin = 100 - 16.5, xmax = 100, ymin = 21.1, ymax = 78.9), color = "white", fill = NA),

    # Goals
    geom_rect(aes(xmin = 0, xmax = 5.5, ymin = 44.5, ymax = 55.5), color = "white", fill = NA),
    geom_rect(aes(xmin = 94.5, xmax = 100, ymin = 44.5, ymax = 55.5), color = "white", fill = NA)
  )
}

# --- SERVER ---
server_2 <- function(input, output, session) {

  # Preprocess: Add season and filter last 3
  match_data <- reactive({
    req(input$match_factor)

    DF_match_factors %>%
      mutate(
        date = as.Date(date),
        season = case_when(
          year(date) == 2013 ~ "2012/2013",
          year(date) == 2014 ~ "2013/2014",
          year(date) == 2015 ~ "2014/2015",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(
        !is.na(season),
        !is.na(!!sym(input$match_factor))
      )
  })

  output$trendPlot <- renderPlot({

    req(input$match_factor)
    df <- match_data()

    factor_col <- sym(input$match_factor)

    # Aggregate by match (your team vs others)
    df_summary <- df %>%
      select(date, season, team, !!factor_col, goal_difference) %>%
      mutate(
        team_type = ifelse(team == our_team_id, "Our Team", "Other Teams"),
        goals_scored = ifelse(team == our_team_id, pmax(goal_difference, 0), NA)
      ) %>%
      group_by(date, season, team_type) %>%
      summarise(
        match_factor_value = mean(!!factor_col, na.rm = TRUE),
        goals_scored = sum(goals_scored, na.rm = TRUE),
        .groups = "drop"
      )

    # Determine season outcome for shading
    season_results <- df %>%
      filter(team == our_team_id) %>%
      group_by(season) %>%
      summarise(net_goal_diff = sum(goal_difference, na.rm = TRUE)) %>%
      mutate(color = ifelse(net_goal_diff >= 0, "green", "red"))

    # Base plot
    p <- ggplot(df_summary, aes(x = date, y = match_factor_value, group = team_type, color = team_type)) +
      geom_line(aes(linetype = team_type), size = 1) +
      scale_color_manual(values = c("Our Team" = "blue", "Other Teams" = "orange")) +
      scale_linetype_manual(values = c("Our Team" = "solid", "Other Teams" = "dashed")) +
      labs(
        title = paste("Match Factor Trend:", input$match_factor),
        x = "Date", y = "Match Factor Value",
        color = "Team", linetype = "Team"
      ) +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank())

#    # Add goal points (only our team)
#    p <- p + geom_point(
#      data = df_summary %>% filter(team_type == "Our Team", !is.na(goals_scored)),
#      aes(x = date, y = match_factor_value, size = goals_scored),
#      color = "gray50", alpha = 0.7
#    )


    # Add background shading
    for (i in 1:nrow(season_results)) {
      season_info <- season_results[i, ]
      season_dates <- df_summary %>%
        filter(season == season_info$season) %>%
        summarise(start = min(date), end = max(date))

      p <- p +
        annotate("rect",
                 xmin = season_dates$start, xmax = season_dates$end,
                 ymin = -Inf, ymax = Inf,
                 fill = season_info$color, alpha = 0.1)
    }

    p
  })

  output$heatmapPlaceholder <- renderPlot({
    req(input$match_factor)

    # Map input match factor to actual Match_data datasets
    data_map <- list(
      "crosses" = Match_data$Match_Cross,
      "shots_on_target" = Match_data$Match_Shots_On,
      "shots_off_target" = Match_data$Match_Shots_Off,
      "fouls_committed" = Match_data$Match_FoulsCommitted,
      "goals" = Match_data$Match_Goals
    )

    selected_factor <- input$match_factor

    # If factor doesn't have position data (e.g., possession), return message
    if (!selected_factor %in% names(data_map)) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = paste("No positional data for", selected_factor),
                   size = 6, hjust = 0.5) +
          theme_void()
      )
    }

    # Get correct dataset
    df <- data_map[[selected_factor]]

    # Normalize position data to 0–100 for plotting on the field
    df <- df %>%
      filter(!is.na(pos_x), !is.na(pos_y)) %>%
      mutate(
        pos_x = scales::rescale(pos_x, to = c(0, 100)),
        pos_y = scales::rescale(pos_y, to = c(0, 100))
      )


    # Handle empty cases
    if (nrow(df) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = paste("No valid position data for", selected_factor),
                   size = 6, hjust = 0.5) +
          theme_void()
      )
    }


    # Heatmap on top of football pitch
    ggplot(df, aes(x = pos_x, y = pos_y)) +
      draw_pitch() +  # Add the pitch elements
      stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE, alpha = 0.7) +
      scale_fill_gradient(low = "white", high = "red") +
      coord_fixed() +
      theme_void() +
      labs(title = paste("Heatmap for", selected_factor))

  })

}

# Run app
shinyApp(ui_2, server_2)

