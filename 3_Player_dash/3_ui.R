ui_3 <- fluidPage(
  titlePanel("U23 Player Dashboard"),

  sidebarLayout(
    sidebarPanel(
      # Select preferred foot
      selectInput("foot_select", "Select Preferred Foot:",
                  choices = unique(Player_Data_U23$preferred_foot),
                  selected = unique(Player_Data_U23$preferred_foot)[1]),

      # Dynamically update player choices based on selected foot
      uiOutput("player_selector"),

      # NEW: Dropdown to select match factor
      selectInput("match_factor", "Select Match Factor:",
                  choices = c("crossing", "finishing", "short_passing", "dribbling"),
                  selected = "crossing")
    ),

    mainPanel(
      fluidRow(
        column(8, plotlyOutput("radarPlot")),
        column(4,
               h4("Player Information"),
               uiOutput("playerInfo")
        )
      ),
      br(), br(),
      h4("Performance Over Time"),
      plotlyOutput("trendPlot", height = "350px")
    )
  )
)
