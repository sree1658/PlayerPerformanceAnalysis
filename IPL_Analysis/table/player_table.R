

output$player_table <- renderReactable({
  reactable(
    height = 540,
    combined_data,
    defaultSorted = list("TotalRuns"="desc"),
    striped = TRUE,
    highlight = TRUE,
    columns = list(
      SR =  colDef(
        format = colFormat(digits = 2),
        style = JS("function(rowInfo) {
      const value = rowInfo.values['SR']
      let color
      if (value > 125) {
        color = 'green'
      } else if (value < 100) {
        color = 'red'
      } else {
        color = 'orange'
      }
      return { color: color, fontWeight: 'bold' }
    }")
      ),
      
      
      Runs = colDef(minWidth = 200,name="Runs over seasons",
                    align = "center", vAlign = "center",
                    cell = function(values) {
                      if (length(values) == 1) {
                        values <- c(0, values)  # Add a 0 to create a "starting point" for the sparkline
                      }
                      sparkline(values, type = "bar", chartRangeMin = 0,height = 50,width=200,barColor = "#00bfc4", barWidth = 15, barSpacing = 3)
                    }),
      StrRate = colDef(minWidth = 200, name="SR over seasons",
                       align = "center", vAlign = "center",
                       cell = function(value, index) {
                         if (length(strk_df$StrRate[[index]]) == 1) {
                           values <- c(0, strk_df$StrRate[[index]])  # Add a 0 to create a "starting point" for the sparkline
                         } else {
                           values <- strk_df$StrRate[[index]]
                         }
                         sparkline(values,height = 50,width=200)
                       }),
      
      "TeamLogo" = colDef(align = "center", vAlign = "center", html = TRUE, name = "Team",  filterable = TRUE, width = 100, sticky = "left"),
      "PlayerName" = colDef(name = "Player",align = "center", vAlign = "center", html = TRUE,   filterable = TRUE, width = 350, sticky = "left"),
      "player_type" = colDef(align = "center", vAlign = "center", name = "Specialisation",  filterable = TRUE, width = 150, 
                             style = function(value) {
                               if (value == "Batter") {
                                 color <- "#008000"  # Green
                               } else if (value == "Bowler") {
                                 color <- "#000000"  # Red
                               } else if (value == "Wicketkeeper") {
                                 color <- "#ff69b4"  # Pink
                               } else {
                                 color <- "#e00000"  # Blue
                               }
                               list(color = color, fontWeight = "bold")
                             })
),
    # sortable = TRUE,
    # filterable = TRUE,
    showPagination = TRUE,
    pagination = TRUE,
    defaultPageSize = 7,
    pageSizeOptions = c(10, 15, 25, 50, 100, 250),
    # showSortable = TRUE,
    # searchable = FALSE,
    # showPageSizeOptions = TRUE,
    # showSortIcon = TRUE,
    showPageInfo = TRUE,
    compact = TRUE,
    bordered = TRUE
    
    # # selection = "multiple",
    # # onClick = "select",
    # # Style
    , style = "z-index: 0; width:100%; font-size:78%;  font-family: Repo, sans-serif;"
    # Theme
    ,theme = reactableTheme(
      highlightColor = "lightblue",color = 'black',
      rowSelectedStyle = list(backgroundColor = "lightblue", boxShadow = "inset 2px 0 0 0 #000000"),
      filterInputStyle = list(color="black"), selectStyle = list(sticky="left")
    )
    # Default Col Def
    ,defaultColDef = colDef(headerVAlign = "center",
                            align = "center", vAlign = "center",
                            headerStyle = list(color = "black", backgroundColor="lightblue",
                                               footer = function(values, name) {
                                                 htmltools::div(name, style = list(fontWeight = 400))
                                               })
    )
  )
})
