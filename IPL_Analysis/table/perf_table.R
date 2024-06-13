

output$perf_table <- renderReactable({
  
  # Filter data based on selected phase and team
  
  filtered_deliveries <- preprocess_deliveries_data(deliveries)
  
  
  if(input$phase != "All") {
    
    filtered_deliveries <- filtered_deliveries[filtered_deliveries$PHASE == input$phase, ]
    
  }
  if(input$team != "All") {
    
    filtered_deliveries <- filtered_deliveries[filtered_deliveries$BATTING_TEAM == input$team, ]
    
  }
  
  
  pp_df <- calculate_player_statistics(filtered_deliveries)
  
  pp_df <- pp_df[order(-pp_df$Runs), ]
  
  pp_df <- pp_df[pp_df$Innings > 5, ]
  
  pp_df <- pp_df[pp_df$Balls > 12, ]
  
  
  pp_order <- c("Striker","Season","Innings","NOTOUT","DBP","Runs","Centuries","Fifties","HighScore","SR","AVG","BPB","Sixes","Fours")
  
  
  # Reassign the DataFrame with the new column order
  pp_df <- pp_df[, pp_order]
  
  
  # Render the output as a reactable without row numbers
  reactable(pp_df,
            height = 450,
            groupBy = "Striker",

            defaultSorted = list("Season"="desc","Runs"="desc"),
            columns = list(
              "Striker" = colDef(align = "center", vAlign = "center", name = "Batsman", sticky = "left"),
              Innings = colDef(aggregate = "sum"),
              Runs = colDef(aggregate = "sum"),
              Fours = colDef(aggregate = "sum"),
              Sixes = colDef(aggregate = "sum"),
              Centuries = colDef(aggregate = "sum"),
              Fifties = colDef(aggregate = "sum"),
              HighScore = colDef(aggregate = "max"),
              SR =  colDef(aggregate = "mean",
                           format = colFormat(digits = 2),
                           style = JS("function(rowInfo) {
      const value = rowInfo.values['SR']
      let color
      if (value > 130) {
        color = 'green'
      } else if (value < 100) {
        color = 'red'
      } else {
        color = 'orange'
      }
      return { color: color, fontWeight: 'bold' }
    }")
              ),
              NOTOUT =  colDef(aggregate = "sum"),
              RPI =  colDef(aggregate = "mean", format = colFormat(digits = 2)),
              AVG  = colDef(aggregate = "mean", format = colFormat(digits = 2)),
              BPD = colDef(aggregate = "mean", format = colFormat(digits = 2)),
              DBP = colDef(aggregate = "mean", format = colFormat(digits = 2)),
              BPB = colDef(aggregate = "mean", format = colFormat(digits = 2))
  
            ),
            defaultPageSize = 17,
            searchable = TRUE, sortable = TRUE, 
            paginateSubRows = TRUE,
            highlight = TRUE,
            outlined  = TRUE,
            bordered  = TRUE,    
            # selection = "multiple", onClick = "select",
            # Style
            style = "z-index: 0; width:100%; font-size:78%;  font-family: Repo, sans-serif;",
            # Theme
            theme = reactableTheme(
              highlightColor = "lightblue",
              rowSelectedStyle = list(backgroundColor = "cornsilk", boxShadow = "inset 2px 0 0 0 #ffa62d"),
              filterInputStyle = list(color="black"),selectStyle = list(sticky="left")
              
            ),
            # Default Col Def
            defaultColDef = colDef(headerVAlign = "center",
                                   align = "center",vAlign = "center",
                                   minWidth = 100,
                                   headerStyle = list(color = "black", backgroundColor="#097969")
            )
            
  )
})
