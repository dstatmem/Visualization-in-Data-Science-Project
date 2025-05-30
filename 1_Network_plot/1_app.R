# install.packages("visNetwork")
# install.packages("GGally")   # For ggpairs


# Load required libraries
library(tibble)
library(corrr)
library(visNetwork)
library(GGally)

source(file = "Visualization_Rcode/1_Network_plot/1_server.R")
source(file = "Visualization_Rcode/1_Network_plot/1_ui.R")
shinyApp(ui_1, server_1)
