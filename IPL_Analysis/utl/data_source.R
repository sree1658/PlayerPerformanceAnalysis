

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

