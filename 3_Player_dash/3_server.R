server_3 <- function(input, output, session) {

  # Reactive: Filter players by selected footedness
  filtered_players <- reactive({
    Player_Data_U23 %>%
      filter(preferred_foot == input$foot_select)
  })

  # Reactive value to store selected match factor (default is "crossing")
  selected_metric <- reactiveVal("crossing")

  # Update player selector dynamically
  output$player_selector <- renderUI({
    selectInput("player_select", "Choose up to 3 Players:",
                choices = unique(filtered_players()$player_name),
                selected = unique(filtered_players()$player_name)[1],
                multiple = TRUE)
  })

  # Radar chart with click event support
  output$radarPlot <- renderPlotly({
    req(input$player_select)

    player_names <- input$player_select
    plot <- plot_ly(type = 'scatterpolar')

    # Add trace for each selected player
    for (i in seq_along(player_names)) {
      player_data <- Radar_Data %>%
        filter(player_name == player_names[i]) %>%
        summarise(across(c(crossing, finishing, short_passing, dribbling), mean, na.rm = TRUE))

      plot <- plot %>%
        add_trace(
          r = as.numeric(player_data),
          theta = names(player_data),
          name = player_names[i],
          mode = "lines",
          fill = 'toself',
          line = list(shape = "linear"),
          fillcolor = 'rgba(0, 0, 255, 0.001)'
        )
    }



    # Warning this is now still overall average and not our team average!!
    # Add team average trace
    team_avg <- Radar_Data %>%
      summarise(across(c(crossing, finishing, short_passing, dribbling), mean, na.rm = TRUE))

    plot <- plot %>%
      add_trace(
        r = as.numeric(team_avg),
        theta = names(team_avg),
        name = "Team Avg",
        mode = "lines",
        fill = 'toself',
        line = list(shape = "linear", dash = 'dash', color = "gray"),
        fillcolor = 'rgba(128, 128, 128, 0.2)'
      )

    plot %>%
      layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))),
             showlegend = TRUE)
  })

  # Capture clicks on the radar chart to update selected metric
  observe({
    click_data <- event_data("plotly_click", source = "radar")

    # Check if theta (axis label) is available
    if (!is.null(click_data) && !is.null(click_data$theta)) {
      metric_clicked <- click_data$theta

      # Only update if it's one of the allowed metrics
      if (metric_clicked %in% c("crossing", "finishing", "short_passing", "dribbling")) {
        selected_metric(metric_clicked)
      }
    }
  })


  # Display selected metric label
  output$selected_metric_label <- renderText({
    paste("Selected Match Factor:", selected_metric())
  })

  # Time Series plot for the selected metric
  output$trendPlot <- renderPlotly({
    req(input$player_select, input$match_factor)

    plot <- plot_ly()

    # Add lines for each player
    for (player in input$player_select) {
      player_ts <- Radar_Data %>%
        filter(player_name == player) %>%
        arrange(date)

      plot <- plot %>%
        add_lines(data = player_ts, x = ~date, y = ~.data[[input$match_factor]],
                  name = player)
    }

    # Team average
    team_avg_ts <- Radar_Data %>%
      group_by(date) %>%
      summarise(avg_value = mean(.data[[input$match_factor]], na.rm = TRUE))

    plot %>%
      add_lines(data = team_avg_ts, x = ~date, y = ~avg_value,
                name = "Team Avg", line = list(dash = 'dash', color = 'gray')) %>%
      layout(title = paste("Time Series -", input$match_factor),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Value"))
  })


  # Player Info display
  output$playerInfo <- renderUI({
    req(input$player_select)

    player_data <- Player_Data_U23_info %>%
      filter(player_name %in% input$player_select) %>%
      arrange(player_name)

    info_blocks <- lapply(seq_len(nrow(player_data)), function(i) {
      player <- player_data[i, ]
      HTML(paste0(
        "<b>Name:</b> ", player$player_name, "<br/>",
        "<b>Age:</b> ", player$age, "<br/>",
        "<b>Height:</b> ", player$height, " cm<br/>",
        "<b>Weight:</b> ", player$weight, " pound<br/>",
        "<b>Rating:</b> ", player$overall_rating, "<br/>",
        "<b>Potential:</b> ", player$potential, "<hr/>"
      ))
    })

    do.call(tagList, info_blocks)
  })


}
