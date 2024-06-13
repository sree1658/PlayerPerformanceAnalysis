

setwd('C:\\Users\\sreeramr\\Downloads\\IPL_Analysis')

## Read Base data ------------------------------------------------------------- 

player_data <- read_excel("data/player_list_2024.xlsx", col_types = c("text", "text", "text", "text", "text", "text", "text")) %>% 
  mutate(
    TeamLogo = paste0('<a href="', TeamLink , '" target="_blank"><img src="', TeamLogo, '" height="50"></img></a>'),
    PlayerCombined = paste0(
      '<a href="', PlayerLink, '" target="_blank">', Playername, '</a>',
      ' ',
      '<a href="', PlayerLink, '" target="_blank"><img src="', PlayerLogo, '" height="80"></img></a>'
    )
  ) %>% select(TeamLogo, PlayerCombined, Playername, player_type)


## Read Strike Rate and Runs data for Bar Graphs ---------------------------------------

strk_df <- read.csv("data/player_runs_strk.csv")


strk_df <- strk_df %>% select(Playername,Runs,StrRate)%>%
  group_by(Playername) %>%
  summarise(Runs = list(Runs),StrRate = list(StrRate))

## Combine with player data ---------------------------------------

combined_data <- left_join(strk_df, player_data, by = c("Playername" = "Playername"))%>%
  select(TeamLogo, Playername,PlayerCombined, player_type,Runs,StrRate)

## Read Player Performance data  ---------------------------------------

perf_df <- read_excel("data/player_perf.xlsx")%>%rename(TotalRuns = Runs)

combined_data <- left_join(combined_data, perf_df, by = c("Playername" = "Playername"))%>%
  select(TeamLogo, PlayerCombined, player_type,Innings,TotalRuns,SR,Runs,StrRate,Sixes,Fours,BPB,DBP) %>%
  rename(PlayerName = PlayerCombined)

## Read Match summary data and deliveries data  ---------------------------------------

matches <- read.csv( "data/match_info_data.csv")

deliveries <- read_parquet("data/match_data.parquet")

colnames(deliveries)<-stringr::str_to_upper((colnames(deliveries)))

colnames(matches)<-stringr::str_to_upper((colnames(matches)))


conditions <- c("Rising Pune Supergiant", "Delhi Daredevils","Kings XI Punjab")

replacement_values <- c("Rising Pune Supergiants","Delhi Capitals","Punjab Kings")

deliveries$BATTING_TEAM <- replace(deliveries$BATTING_TEAM, deliveries$BATTING_TEAM %in% conditions, replacement_values)

deliveries$BOWLING_TEAM <- replace(deliveries$BOWLING_TEAM, deliveries$BOWLING_TEAM %in% conditions, replacement_values)


matches$TEAM1 <-replace(matches$TEAM1,matches$TEAM1 %in% conditions, replacement_values )

matches$TEAM2 <-replace(matches$TEAM2,matches$TEAM2 %in% conditions, replacement_values )


VENUE_names <- c("Sardar Patel Stadium","Feroz Shah Kotla")

correct_names<- c("Narendra Modi Stadium","Arun Jaitley Stadium")

deliveries$VENUE   <- replace(deliveries$VENUE  , deliveries$VENUE  %in% VENUE_names, correct_names)

matches$VENUE  <- replace(matches$VENUE , matches$VENUE  %in% VENUE_names, correct_names)


deliveries$PHASE <- ifelse(deliveries$BALL <= 6, "Powerplay",
                           ifelse(deliveries$BALL <= 15, "Middle", "Death"))



