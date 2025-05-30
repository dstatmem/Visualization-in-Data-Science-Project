# load libraries
library(shiny)
library(dplyr)
library(ggplot2)

##### Settings #####
# Set working directory
{
  setwd("C:/Users/maria/Documents/UHasselt/MscDataScience/Year1/P2/Visualisation/4_Code")

  # Select club
  Club <- "Málaga CF"
}


##### Data #######
# run data entry and cleaning code
source(file = "Visualization_Rcode/Data_prelim/1_Data_entry.R")
source(file = "Visualization_Rcode/Data_prelim/2_Data_prelim.R")
source(file = "Visualization_Rcode/Data_prelim/3_Match_factors.R")


##### Visualizations #####

# run network plot for question 1:
source(file = "Visualization_Rcode/1_Network_plot/1_app.R")
shinyApp(ui_1, server_1)

# run time series for question 2:
source("Visualization_Rcode/2_Teamplot/2_app.R")
shinyApp(ui_2, server_2)

# run player dashboard for question 3:
source("Visualization_Rcode/3_Player_dash/3_app.R")
shinyApp(ui_3, server_3)
