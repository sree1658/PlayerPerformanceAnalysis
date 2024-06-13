



## Preprocess datasets -  It replaces NaN values with 0 in specified columns, calculates the 'total_runs' for each ball, and restructures the DataFrame to a standardized format

preprocess_deliveries_data <- function(df) {
  # Replace NA values with integer 0 in the specified columns
  columns_to_replace <- c('RUNS_OFF_BAT', 'EXTRAS', 'WIDES', 'NOBALLS', 'BYES', 'LEGBYES', 'PENALTY')
  df[columns_to_replace][is.na(df[columns_to_replace])] <- 0
  
  # Calculate 'total_runs' by summing up the specified columns
  df$TOTAL_RUNS <- rowSums(df[columns_to_replace])
  
  # Convert 'BALL' to string and split it into 'over' and 'BALL' columns
  df$BALL <- as.character(df$BALL)
  df$OVER <- as.integer(sapply(strsplit(df$BALL, '\\.'), function(x) as.integer(x[1]) + 1))
  df$BALL <- as.integer(sapply(strsplit(df$BALL, '\\.'), function(x) as.integer(x[2])))
  
  # # Reorder the columns
  # column_order <- c("MATCH_ID", "SEASON", "START_DATE", "VENUE", "INNINGS", "OVER", "BALL", "BATTING_TEAM", "BOWLING_TEAM", "STRIKER", "NON_STRIKER", "BOWLER", "RUNS_OFF_BAT", "EXTRAS", "WIDES", "NOBALLS", "BYES", "LEGBYES", "PENALTY", "TOTAL_RUNS", "WICKET_TYPE", "PLAYER_DISMISSED", "OTHER_WICKET_TYPE", "OTHER_PLAYER_DISMISSED", "CRICSHEET_ID")
  # 
  # 
  # # Reassign the DataFrame with the new column order
  # df <- df[, column_order]
  # 
  return(df)
}


# deliveries <- preprocess_deliveries_data(deliveries)


BALLS_PER_DISMISSAL <- function(BALLS, DISMISSALS) {
  if (DISMISSALS > 0) {
    return(BALLS / DISMISSALS)
  } else {
    return(BALLS / 1)
  }
}


BALLS_PER_BOUNDARY <- function(BALLS, BOUNDARIES) {
  if (BOUNDARIES > 0) {
    return(BALLS / BOUNDARIES)
  } else {
    return(BALLS / 1)
  }
}


calculate_player_statistics <- function(df) {
  
  deliveries_aggregated <- aggregate(RUNS_OFF_BAT ~ MATCH_ID + STRIKER + SEASON, data = df, FUN = sum)
  
  # Calculate HighScore for each player in each season
  high_score <- aggregate(RUNS_OFF_BAT ~ STRIKER + SEASON, data = deliveries_aggregated, FUN = max)
  names(high_score)[3] <- "HighScore"
  
  # Filter deliveries_aggregated for centuries, fifties, and thirties
  centuries <- subset(deliveries_aggregated, RUNS_OFF_BAT >= 100)
  fifties <- subset(deliveries_aggregated, RUNS_OFF_BAT >= 50 & RUNS_OFF_BAT < 100)
  thirties <- subset(deliveries_aggregated, RUNS_OFF_BAT >= 30 & RUNS_OFF_BAT < 50)
  
  centuries_count <- data.frame(STRIKER = character(), SEASON = character(), Centuries = integer())
  fifties_count <- data.frame(STRIKER = character(), SEASON = character(), Fifties = integer())
  thirties_count <- data.frame(STRIKER = character(), SEASON = character(), Thirties = integer())
  
  
  if (dim(centuries)[1] >0){
  # Count the number of centuries, fifties, and thirties for each player in each season
  centuries_count <- aggregate(RUNS_OFF_BAT ~ STRIKER + SEASON, data = centuries, FUN = length)
  names(centuries_count)[3] <- "Centuries"
  }
  
  if (dim(fifties)[1] >0){
  fifties_count <- aggregate(RUNS_OFF_BAT ~ STRIKER + SEASON, data = fifties, FUN = length)
  names(fifties_count)[3] <- "Fifties"
  }
  
  
  if (dim(thirties)[1] >0){
  thirties_count <- aggregate(RUNS_OFF_BAT ~ STRIKER + SEASON, data = thirties, FUN = length)
  names(thirties_count)[3] <- "Thirties"
  }
  
  # Merge all dataframes to create the final dataframe
  
  summary_df <- merge(high_score, thirties_count, by = c("STRIKER", "SEASON"), all = TRUE)
  summary_df <- merge(summary_df, fifties_count, by = c("STRIKER", "SEASON"), all = TRUE)
  summary_df <- merge(summary_df, centuries_count, by = c("STRIKER", "SEASON"), all = TRUE)
  
  # colnames(summary_df)
  
  # Rename columns
  colnames(summary_df) <- c("Striker", "Season","HighScore", "Thirties", "Fifties", "Centuries" )
  
  # Fill NA values with 0
  summary_df[is.na(summary_df)] <- 0
  
  
  # Calculate dot BALLS, ones, twos, threes, fours, and sixes
  df$isDot <- as.integer(df$RUNS_OFF_BAT == 0)
  df$isOne <- as.integer(df$RUNS_OFF_BAT == 1)
  df$isTwo <- as.integer(df$RUNS_OFF_BAT == 2)
  df$isThree <- as.integer(df$RUNS_OFF_BAT == 3)
  df$isFour <- as.integer(df$RUNS_OFF_BAT == 4)
  df$isSix <- as.integer(df$RUNS_OFF_BAT == 6)
  
  # Calculate runs, innings, BALLS, DISMISSALS, and other statistics
  runs <- aggregate(df$RUNS_OFF_BAT, by = list(df$STRIKER, df$SEASON), FUN = sum)
  colnames(runs) <- c("Striker", "Season", "Runs")
  
  Innings <- aggregate(df$MATCH_ID, by = list(df$STRIKER, df$SEASON), FUN = function(x) length(unique(x)))
  colnames(Innings) <- c("Striker", "Season", "Innings")
  
  BALLS <- aggregate(MATCH_ID ~ STRIKER + SEASON, data = subset(df, WIDES == 0), FUN = length)
  colnames(BALLS) <- c("Striker", "Season", "Balls")
  
  DISMISSALS <- aggregate(as.integer(!is.na(df$PLAYER_DISMISSED)), by = list(df$STRIKER, df$SEASON), FUN = sum)
  colnames(DISMISSALS) <- c("Striker", "Season", "Dismissals")
  
  dots <- aggregate(df$isDot, by = list(df$STRIKER, df$SEASON), FUN = sum)
  colnames(dots) <- c("Striker", "Season", "Dots")
  
  ones <- aggregate(df$isOne, by = list(df$STRIKER, df$SEASON), FUN = sum)
  colnames(ones) <- c("Striker", "Season", "Ones")
  
  twos <- aggregate(df$isTwo, by = list(df$STRIKER, df$SEASON), FUN = sum)
  colnames(twos) <- c("Striker", "Season", "Twos")
  
  threes <- aggregate(df$isThree, by = list(df$STRIKER, df$SEASON), FUN = sum)
  colnames(threes) <- c("Striker", "Season", "Threes")
  
  fours <- aggregate(df$isFour, by = list(df$STRIKER, df$SEASON), FUN = sum)
  colnames(fours) <- c("Striker", "Season", "Fours")
  
  sixes <- aggregate(df$isSix, by = list(df$STRIKER, df$SEASON), FUN = sum)
  colnames(sixes) <- c("Striker", "Season", "Sixes")
  
  df <- merge(Innings, runs, by = c("Striker", "Season"))
  df <- merge(df, BALLS, by = c("Striker", "Season"))
  df <- merge(df, DISMISSALS, by = c("Striker", "Season"))
  df <- merge(df, dots, by = c("Striker", "Season"))
  df <- merge(df, ones, by = c("Striker", "Season"))
  df <- merge(df, twos, by = c("Striker", "Season"))
  df <- merge(df, threes, by = c("Striker", "Season"))
  df <- merge(df, fours, by = c("Striker", "Season"))
  df <- merge(df, sixes, by = c("Striker", "Season"))
  df <- merge(df, summary_df, by = c("Striker", "Season"))
  
  
  # Calculate Strike Rate (SR)
  df$SR <- round(100 * (df$Runs / df$Balls),2)
  
  df$NOTOUT <- df$Innings - df$Dismissals
  
  # Calculate Runs per Inning (RPI)
  df$RPI <- round(df$Runs / df$Innings,2)
  
  df$AVG <- ifelse(df$Dismissals > 0, round(df$Runs / df$Dismissals,2), df$Runs / 1)
  
  # Calculate BALLS per Dismissal (BPD)
  df$BPD <- ifelse(df$Dismissals > 0, round(df$Balls / df$Dismissals,2), df$Balls / 1)
  
  
  df$DBP <- round(100 * ifelse(df$Balls > 0, round(df$Dots / df$Balls,2), 1),2)
  
  # Calculate BALLS per Boundary (BPB)
  df$BPB <- ifelse((df$Fours + df$Sixes)>0,round(df$Balls / (df$Fours + df$Sixes)),NA)
  
  
  return(df)
  
}




