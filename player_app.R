

# setwd('C:\\Users\\sreeramr\\Downloads\\IPL_Analysis')

# 1. Packages -------------------------------------------------------------
source("utl/library.R")

# 2. Data Source ----------------------------------------------------------
source("utl/data_source.R")

# 3. UI  ----------------------------------------------------------

ui <- fluidPage(
  
  # App Title
  titlePanel("IPL Player Analysis"),
  
  # Shiny Theme
  theme = shinytheme("superhero"),
  
  useShinyjs(),
  
  #Final Table
  shinycssloaders::withSpinner(reactableOutput("player_table"))
)


# 4. Server  ----------------------------------------------------------

server <- function(input, output) {
  
  source(file.path("server", "player_table.R"),  local = TRUE, encoding = "UTF-8" )$value


}

# 5.  Run the application  ----------------------------------------------------------

shinyApp(ui = ui, server = server)

