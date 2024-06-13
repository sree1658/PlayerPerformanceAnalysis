

# setwd('C:\\Users\\sreeramr\\Downloads\\IPL_Analysis')

# 1. Packages -------------------------------------------------------------
source("utl/library.R")

# 2. Data Source ----------------------------------------------------------

source("utl/data_source.R")

source("utl/player_analysis.R")

# 3. UI  ----------------------------------------------------------

source("utl/ui.R")

# 4. Server  ----------------------------------------------------------

server <- function(input, output, session) {
  
  
  source(file.path("table", "player_table.R"),  local = TRUE, encoding = "UTF-8" )$value
  
  source(file.path("table", "perf_table.R"),  local = TRUE, encoding = "UTF-8" )$value
  

}

# 5.  Run the application  ----------------------------------------------------------

shinyApp(ui = ui, server = server)

