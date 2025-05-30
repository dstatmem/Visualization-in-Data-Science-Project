# Visualization-in-Data-Science-Project
Repo for Visualization in Data Science collaborative project for group 6.

This project presents an interactive **Shiny dashboard** to explore football match data using advanced data visualizations including **network graphs**, **radar charts**, **time series**, **pair plots**, and **heatmaps**. The dashboard is built as part of the course *Visualization in Data Science* and focuses on the analysis of team performance, player statistics, and match factors.


Use code:

Download the full map structure and unzip the Data.zip.
Change work directory in the main file. 
Run main file.


project/
│
├── Main.R                      # Main code to run \
├── Data_prelim/                # UI code for Shiny dashboard \
│   └── Data_entry.R            # Loads data needed for the project\
│   └── Data_prelim.R           # Merges data \
│   └── Match_factors.R         # Creates data frame based on selected match factors\
├── 1_Network_plot/             # Create graph 1: network plot\
│   └── 1_app.R                 # Creates shiny dashboard\
│   └── 1_server.R              # Logic behind graph 1\
│   └── 1_ui.R                  # User interface graph 1\
├── 2_Teamplot/                 # Create graph 2: Team plot\
│   └── 2_app.R                 # Creates shiny dashboard\
├── 3_Player_dash/              # Create graph 3: Player dashboard\
│   └── 3_app.R                 # Creates shiny dashboard\
│   └── 3_server.R              # Logic behind graph 3\
│   └── 3_ui.R                  # User interface graph 3\
├── README.md                   # Project documentation\
├── data.zip/\
│   └── data.csv                # All data-files needed\


