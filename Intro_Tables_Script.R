

################################
# LOAD LIBRARIES & DATA IMPORT
################################

library(tidyverse)

Key_Points_URL <- "https://raw.githubusercontent.com/JeffSackmann/tennis_MatchChartingProject/refs/heads/master/charting-m-stats-KeyPointsServe.csv"
KP_Serve_Data <- read_csv(Key_Points_URL)



################################################
# CREATE FUNCTION TO CALCULATE PERCENTAGE STATS
################################################

Calculate_Percentages_KPS <- function(df) {
  
  df <- df %>% 
    mutate(
      Pct_Pts_Won = pts_won / pts *100,
      Pct_FS_In = first_in / pts *100
    )
  
  df <- df[-c(7, 8), ]
  
  return(df)
}


#################################################
# TABLE1 - SERVE STATS FOR FIRST 3 GRAND SLAMS
#################################################

# Create list of DFs for each of three grand slam finals
KPS_US_F2008 <- KP_Serve_Data %>% 
  filter(match_id == "20080908-M-US_Open-F-Roger_Federer-Andy_Murray") %>% 
  select(-c(rally_winners, rally_forced, unforced))

KPS_AUS_F2010 <- KP_Serve_Data %>% 
  filter(match_id == "20100131-M-Australian_Open-F-Roger_Federer-Andy_Murray") %>% 
  select(- c(rally_winners, rally_forced, unforced))

KPS_AUS_F2011 <- KP_Serve_Data %>% 
  filter(match_id == "20110130-M-Australian_Open-F-Novak_Djokovic-Andy_Murray") %>% 
  select(-c(rally_winners, rally_forced, unforced))

T1_dfs <- list(KPS_US_F2008, KPS_AUS_F2010, KPS_AUS_F2011)




# Apply function to list of DFs and combine all rows into one DF
T1_Combined_Matches_DF <- lapply(T1_dfs, Calculate_Percentages_KPS)
T1_Combined_Matches_DF <- bind_rows(T1_Combined_Matches_DF)



# Create and remove columns and edit Match IDs 
T1_Combined_Matches_DF <- T1_Combined_Matches_DF %>%
  mutate( player = ifelse(player == "Andy Murray", "Murray", "Opponent")) %>% 
  select(-c(aces, svc_winners, dfs, Pct_Pts_Won, Pct_FS_In)) %>% 
  mutate(
    match_id = ifelse(match_id == "20080908-M-US_Open-F-Roger_Federer-Andy_Murray", "US_2008",
                      ifelse(match_id == "20100131-M-Australian_Open-F-Roger_Federer-Andy_Murray", "AUS_2010", "AUS_2011")
    ),
    sets = ifelse(match_id == "US_2008", 3,
                  ifelse(match_id == "AUS_2010", 3, 3))
    ) 


# Pivot DF wider, remove Match IDs and rename columns
T1_Combined_Matches_DF <- T1_Combined_Matches_DF %>% 
  pivot_wider(
    names_from = row,
    values_from = c(pts, pts_won, first_in),
  ) %>% 
  select(-match_id)
  


# Create Summary Table of metrics 
T1_Combined_Matches_DF <- T1_Combined_Matches_DF %>% 
  group_by(player) %>%
  summarise(across(everything(), ~sum(.x, na.rm = TRUE)))


# Add new calculated columns for final stats, and remove unneeded columns
T1_Combined_Matches_DF <- T1_Combined_Matches_DF %>% 
  mutate(
    'Number of Deuce Points on Serve (per set)' = pts_Deuce / sets,
    'Number of Game Points on Serve (per set)' = pts_GP / sets,
    'Number of Break Points on Serve (per set)' = pts_BP / sets,
    
    'First Serve Percentage on Deuce Points' = first_in_Deuce / pts_Deuce *100,
    'First Serve Percentage on Game Points' = first_in_GP / pts_GP *100,
    'First Serve Percentage on Break Points' = first_in_BP / pts_BP *100,
    
    'Percentage of Deuce Points Won' = pts_won_Deuce / pts_Deuce *100,
    'Percentage of Game Points Won' = pts_won_GP / pts_GP *100,
    'Percentage of Break Points Won' = pts_won_BP / pts_BP *100,
    
  ) %>% 
  select(-c(pts_BP, pts_GP, pts_Deuce, pts_won_BP, pts_won_GP, pts_won_Deuce, 
            first_in_BP, first_in_GP, first_in_Deuce, sets,))


# Pivot table to create final columns
Final_Table1 <- T1_Combined_Matches_DF %>% 
  pivot_longer(
    cols = -player,
    names_to = "Statistic",
    values_to = "value"
  ) %>% 
  pivot_wider(
    names_from = player,
    values_from = value,
  )


# Adjust final table to round to 2dp
Final_Table1 <- Final_Table1 %>% 
  mutate(
    across(
      c(`Murray`, Opponent),
      ~ round(.x, 2)
    )
  )


# Tidy environment
to_keep <- c("KP_Serve_Data", "Final_Table1", "Calculate_Percentages_KPS")
rm(list = setdiff(ls(), to_keep)) 





########################################################
# TABLE2 - SERVE STATS FOR WIMBLEDON AND OLYMPICS 2012
########################################################

# Create list of DFs for each of three grand slam finals
KPS_WIM_F2012 <- KP_Serve_Data %>% 
  filter(match_id == "20120708-M-Wimbledon-F-Roger_Federer-Andy_Murray") %>% 
  select(-c(rally_winners, rally_forced, unforced))

KPS_OLY_F2012 <- KP_Serve_Data %>% 
  filter(match_id == "20120804-M-Olympics-F-Andy_Murray-Roger_Federer") %>% 
  select(- c(rally_winners, rally_forced, unforced))


T2_dfs <- list(KPS_WIM_F2012, KPS_OLY_F2012)




# Apply function to list of DFs and combine all rows into one DF
T2_Combined_Matches_DF <- lapply(T2_dfs, Calculate_Percentages_KPS)
T2_Combined_Matches_DF <- bind_rows(T2_Combined_Matches_DF)



# Create and remove columns and edit Match IDs 
T2_Combined_Matches_DF <- T2_Combined_Matches_DF %>%
  select(-c(aces, svc_winners, dfs, Pct_Pts_Won, Pct_FS_In)) %>% 
  mutate(
    match_id = ifelse(match_id == "20120708-M-Wimbledon-F-Roger_Federer-Andy_Murray", 'Wimbledon 2012 Final', 'Olympic 2012 Final'),
    sets = ifelse(match_id == 'Wimbledon 2012 Final', 3, 3),
    player = ifelse(player == 'Andy Murray', "Murray", "Federer")
  )

  

# Pivot DF wider, remove Match IDs and rename columns
T2_Combined_Matches_DF <- T2_Combined_Matches_DF %>% 
  pivot_wider(
    names_from = row,
    values_from = c(pts, pts_won, first_in),
  ) 



# Create Summary Table of metrics 
T2_Combined_Matches_DF <- T2_Combined_Matches_DF %>% 
  group_by(player, match_id) %>%
  summarise(across(everything(), ~sum(.x, na.rm = TRUE)), .groups = "drop")


# Add new calculated columns for final stats, and remove unneeded columns
T2_Combined_Matches_DF <- T2_Combined_Matches_DF %>% 
  mutate(
    'Number of Deuce Points on Serve (per set)' = pts_Deuce / sets,
    'Number of Game Points on Serve (per set)' = pts_GP / sets,
    'Number of Break Points on Serve (per set)' = pts_BP / sets,
    
    'First Serve Percentage on Deuce Points' = first_in_Deuce / pts_Deuce *100,
    'First Serve Percentage on Game Points' = first_in_GP / pts_GP *100,
    'First Serve Percentage on Break Points' = first_in_BP / pts_BP *100,
    
    'Percentage of Deuce Points Won' = pts_won_Deuce / pts_Deuce *100,
    'Percentage of Game Points Won' = pts_won_GP / pts_GP *100,
    'Percentage of Break Points Won' = pts_won_BP / pts_BP *100
  ) %>% 
  select(-c(pts_BP, pts_GP, pts_Deuce, pts_won_BP, pts_won_GP, pts_won_Deuce, 
            first_in_BP, first_in_GP, first_in_Deuce, sets,))






# Pivot table to create final columns for all 4 player and match combinations
Final_Table2 <- T2_Combined_Matches_DF %>% 
pivot_longer(
    cols = -c(player, match_id),
    names_to = "Statistic",
    values_to = "value"
  ) %>% 
  mutate(
    match_label = gsub(" ", " ", match_id),
    player_label = gsub(" ", " ", player),
    column_name = paste0(player_label, " ", match_label)
  ) %>%
  select(Statistic, column_name, value) %>%
  pivot_wider(
    names_from = column_name,
    values_from = value
  )


# Adjust final table to correct order and then round to 2dp
Final_Table2 <- Final_Table2 %>% 
  select(
    Statistic,
    `Murray Wimbledon 2012 Final`,
    `Federer Wimbledon 2012 Final`,
    `Murray Olympic 2012 Final`, 
    `Federer Olympic 2012 Final`
  ) %>% 
  mutate(
    across(
      c(`Murray Wimbledon 2012 Final`,
        `Federer Wimbledon 2012 Final`,
        `Murray Olympic 2012 Final`, 
        `Federer Olympic 2012 Final`),
      ~ round(.x, 2)
    )
  )



# Tidy environment
to_keep <- c("Final_Table1", "Final_Table2")
rm(list = setdiff(ls(), to_keep)) 


# Write tables to csv for use in dissertation
write.csv(Final_Table1, file = "B1706_Table1.csv", row.names = FALSE)
write.csv(Final_Table2, file = "B1706_Table2.csv", row.names = FALSE)
