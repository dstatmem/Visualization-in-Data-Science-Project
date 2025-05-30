library(shiny)
library(dplyr)
library(tibble)
library(corrr)
library(visNetwork)

ui_1 <- fluidPage(
  titlePanel("Match Factor Correlation Network"),
  sidebarLayout(
    sidebarPanel(
      width = 2,  # sidebar width
      sliderInput("corThreshold",
                  "Minimum Absolute Correlation:",
                  min = 0.01, max = 1, value = 0.01, step = 0.05)
    ),
    mainPanel(
      visNetworkOutput("networkPlot", height = "600px"),
      h3("Pair Plot of Selected Factor and Correlated Variables"),
      plotOutput("pair_plot", height = "400px")
    )
  )
)
