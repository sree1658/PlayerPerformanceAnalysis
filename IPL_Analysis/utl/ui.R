


header = dashboardHeader(title = span(img(src = "tata_ipl.jpg", height = 50), "Player Analysis"),titleWidth = 300) #logo_title

# Define the sidebar content
sidebarContent <- div(
  selectInput("phase", "Select Phase:",
              choices = c("All", unique(deliveries$PHASE)),
              selected = "All"),
  selectInput("team", "Select Team:",
              choices = c("All", unique(deliveries$BATTING_TEAM)),
              selected = "All")
)


ui <- dashboardPage(
  
  title = "IPL2024",
  header,


    dashboardSidebar(
    # Placeholder for conditional sidebar content
    conditionalPanel(
      condition = "input.tabs == 'batting_analysis'",
      sidebarContent
    ),collapsed = TRUE
  ),
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    tabsetPanel(
      id = "tabs", # Assign an ID to the tabsetPanel for referencing in conditionalPanel
      # First Tab
      tabPanel(
        icon = icon("user-friends"),
        "Player Table",
        value = "player_table",
        # Final Table
        shinycssloaders::withSpinner(reactableOutput("player_table"), image = "loading.gif",image.width = '400px')
      ),
      
      
      # Second Tab with Batting Analysis
      tabPanel(
        icon = icon("chart-line"),
        title = "Batting Analysis",
        value = "batting_analysis",
        fluidRow(
          column(
            width = 12,
            # Create a collapsible box with a spinner and a plotly chart
            box(
              shinycssloaders::withSpinner(reactableOutput("perf_table"), image = "loading.gif",image.width = '400px'),
              title = "Player Performance by Season",
              width = 12,
              solidHeader = TRUE,
              status = "primary"
            )
          )
        )
      ),  
      
      # Footer
      tags$div(
        class = "footer-container",
        tags$div(class = "footer-linkedin", 
                 HTML('<a style="color:white" href="https://www.linkedin.com/in/sreeram-ravindranathan/" target=_blank>
                      <i class="fa fa-linkedin" style="font-size: 30px;"></i></a>')
        ), 
        tags$div(class = "footer-title", 
                 HTML("Sreeram Ravindranathan")
        ),
        tags$div(class = "footer-github", 
                 HTML('<a style="color:white" href="https://github.com/sree1658" target=_blank>
                      <i class="fa fa-github" style="font-size: 30px;"></i></a>')
        )
      )
    )
  )
)

