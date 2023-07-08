# Script to assess MLS player re-rostering
# Author: Eric Vidoni
# Date: 2023-7-07


library(tidyverse)
library(rio)
library(ggrepel)

root <- 'C:/your_path/'

for(yr in 2023:2007){
  
  tmp_dat <- import(file.path(root, 'FBREF_player_data_20230707.xlsx'),  sheet = as.character(yr), col_names = FALSE) %>%
    as_tibble() %>%
    mutate(year = yr)
  
  if(yr == 2023){
    full_dat <- tmp_dat
  } else {
    full_dat <- rbind(full_dat, tmp_dat)
  }
  
}

full_dat_cleaned <- full_dat %>%
  rename(. , num = `...1`,
          player = `...2`,
         nationality = `...3`,
         position = `...4`,
         club = `...5`,
         age = `...6`,
         salary = `...7`,
         salary_wk = `...8`) %>%
  mutate(club = ifelse(club == "Dynamo FC","Dynamo",
                       ifelse(club == "KC Wizards","Sporting KC",
                              ifelse(club == "Montreal Impact", "CF Montréal", club)))) %>%
  select(player, club, year) %>%
  arrange(player,year) %>%
  group_by(player, year, club) %>% 
  slice_head(n = 1L) %>%  #Filters out a few weird players that have multiple same-year entries
  ungroup() %>%
  arrange(player, year)

players <- unique(full_dat_cleaned$player)


#Builds a df for each player that spans a complete set of years they were active in MLS
for(p in 1:length(players)){
  #print(p)
  
  tmp_p <- full_dat_cleaned %>%
    filter(player == players[p]) 
  
  yr_range <- range(tmp_p$year, na.rm = TRUE)
  yr_seq <- seq(yr_range[1], yr_range[2])
  
  tmp_p2 <- data.frame(player = rep(players[p],length(yr_seq)), year = yr_seq)

  if(p == 1){
    full_dat_player <- tmp_p2
  } else {
    full_dat_player <- rbind(full_dat_player, tmp_p2)
  }
  
  rm(tmp_p, yr_range, yr_seq, tmp_p2)
}
  
#Binds the df of each player with their full playing time, and the cleaned rostering dataset 
#such that the full dataset has each player and the full compliment of the years they played 
#at least between their first and last year in MLS
full_dat_player2 <- left_join(full_dat_player, full_dat_cleaned) %>%
  mutate(club = ifelse(is.na(club),"Unknown",club)) %>%
  group_by(player) %>%
  mutate(club_change = ifelse(club != lag(club, n = 1L),1,0)) %>% #Notes a club change, mostly used as a proof for the  year lag estimate below
  ungroup() %>%
  arrange(player, club, year) %>%
  group_by(player,club) %>%
  mutate(club_yr_gap = year - lag(year, n = 1L)) #Calculates the difference in rostered year by club. >1 is assumed to be a return to the club


tot_players_club <- full_dat_cleaned %>%
  group_by(club,player) %>%
  filter(!duplicated(interaction(club,player))) %>%  #Filters to unique names at each club
  group_by(club) %>%
  summarise(n_total_unique_club_players = n())


returning_players <- full_dat_player2 %>%
  filter(club_yr_gap > 1) %>%
  filter(club != "Unknown") 

returning_players_club <- returning_players %>%
  group_by(club) %>%
  summarise(n_total_returning_players = n())

final <- left_join(tot_players_club, returning_players_club, by = 'club') %>%
  mutate(n_total_returning_players = ifelse(is.na(n_total_returning_players),0, n_total_returning_players)) %>%
  mutate(pct_returning = round((n_total_returning_players/n_total_unique_club_players),3)*100)


#Proof of tot_players_club  190 above, 190 here
#skc <- full_dat_cleaned %>%
#  filter(club == "Sporting KC") %>%
#  pull(player)
# unique(skc)



###Plots scatter of returnees against total # of unique players
final %>%
  ggplot() + 
  geom_point(aes(x = n_total_unique_club_players, y = n_total_returning_players)) +
  geom_text_repel(aes(x = n_total_unique_club_players, y = n_total_returning_players,label = club),
                  box.padding = 1,
                  show.legend = FALSE) +
  labs(x = "Total  # of Unique Players",
       y = "# of Players Returning to Former Club") +
  theme_bw()

final %>%
  ggplot() + 
  geom_bar(aes(x = reorder(club,pct_returning, decreasing = TRUE),
               y = pct_returning),
           stat = 'identity') +
  labs(x = "Club",
       y = "% of Players Returning to Club") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) 


