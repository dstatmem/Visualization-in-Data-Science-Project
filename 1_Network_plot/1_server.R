# Server
server_1 <- function(input, output, session) {

  match_numeric <- DF_match_factors %>%
    select(goal_difference, yellow_cards, red_cards, crosses, corners,
           fouls_committed, shots_on_target, shots_off_target) %>%
    drop_na()

  match_data_scaled <- match_numeric

  # Compute correlation matrix
  cor_mat <- correlate(match_numeric)

  # Convert to long format
  cor_long <- cor_mat %>%
    stretch(na.rm = TRUE) %>%
    filter(x != y)  # remove self-correlations

  # Update dropdown choices based on correlation matrix
  observe({
    updateSelectInput(session, "selected_var",
                      choices = unique(c(cor_long$x, cor_long$y)),
                      selected = "goal_difference")
  })

  # Render visNetwork plot
  output$networkPlot <- renderVisNetwork({

    filtered_cor <- cor_long %>%
      filter(abs(r) > input$corThreshold)

    # Create nodes
    nodes <- tibble(
      id = unique(c(filtered_cor$x, filtered_cor$y)),
      label = id,
      color = ifelse(id == "goal_difference", "orange", "lightblue")
    )

    # Create edges
    edges <- filtered_cor %>%
      rename(from = x, to = y, value = r) %>%
      mutate(width = abs(value) * 5,
             color = ifelse(value > 0, "green", "red"),
             title = paste("Correlation:", round(value, 2)))

    visNetwork(nodes, edges, height = "600px", width = "100%") %>%
      visEdges(smooth = TRUE) %>%
      visNodes(font = list(size = 20)) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 42)
  })

  # Pair plot based on dropdown
  output$pair_plot <- renderPlot({
    selected <- input$networkPlot_selected
    req(selected)

    related_vars <- cor_long %>%
      filter((x == selected | y == selected) & abs(r) > input$corThreshold) %>%
      mutate(variable = ifelse(x == selected, y, x)) %>%
      pull(variable)

    # Variables to plot
    plot_vars <- unique(c(selected, related_vars))

    # Ensure they exist in the dataset
    valid_vars <- plot_vars[plot_vars %in% colnames(match_data_scaled)]

    if (length(valid_vars) >= 2) {
      GGally::ggpairs(
        match_data_scaled[, valid_vars],
        lower = list(continuous = wrap("points", alpha = 0.6, size = 3, color = "dodgerblue")),
        diag = list(continuous = wrap("densityDiag", fill = "skyblue", alpha = 0.4)),
        upper = list(continuous = wrap("cor", size = 5))
      ) +
        theme_minimal(base_size = 16) +  # Increase text size
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank()
        )
    } else {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Not enough correlated variables", size = 6, hjust = 0.5) +
        theme_void()
    }
  })


}
