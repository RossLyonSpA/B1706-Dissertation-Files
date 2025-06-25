
################################
# LOAD LIBRARIES & DATA IMPORT
################################

library(tidyverse)
library(slider)
library(xgboost)
library(caret)
library(pROC)
library(SHAPforxgboost)


# Read in 2010s match charting point by point data file from GitHub
url <- "https://raw.githubusercontent.com/JeffSackmann/tennis_MatchChartingProject/refs/heads/master/charting-m-points-2010s.csv"
atp_charting_points <- read_csv(url)


# List of match IDs for matches of interest
match_name_list <- c(
  
  # TRAINING DATASET MATCHES
  "20120705-M-Wimbledon-QF-Roger_Federer-Mikhail_Youzhny",
  "20120706-M-Wimbledon-SF-Andy_Murray-Jo_Wilfried_Tsonga",
  "20120706-M-Wimbledon-SF-Roger_Federer-Novak_Djokovic",
  
  "20110629-M-Wimbledon-QF-Roger_Federer-Jo_Wilfried_Tsonga",
  "20110701-M-Wimbledon-SF-Andy_Murray-Rafael_Nadal",
  "20110701-M-Wimbledon-SF-Novak_Djokovic-Jo_Wilfried_Tsonga",
  "20110703-M-Wimbledon-F-Novak_Djokovic-Rafael_Nadal",
  
  "20100630-M-Wimbledon-QF-Roger_Federer-Tomas_Berdych",
  "20100702-M-Wimbledon-SF-Andy_Murray-Rafael_Nadal",
  "20100702-M-Wimbledon-SF-Tomas_Berdych-Novak_Djokovic",
  "20100704-M-Wimbledon-F-Tomas_Berdych-Rafael_Nadal",
  
  #TESTING DATASET MATCHES
  "20120804-M-Olympics-F-Andy_Murray-Roger_Federer",
  "20120708-M-Wimbledon-F-Roger_Federer-Andy_Murray"
  
)

# Create list which will hold new dfs
match_dfs <- list()

# For loop for importing df of each match into df list
for (id in match_name_list) {
  match_dfs[[id]] <- atp_charting_points %>% filter(match_id == id)
}



####################################################
# 1 - CREATE FUNCTION TO PROCESS GAME WINNER/OUTCOME
####################################################

Prepare_Dfs <- function(df) {
  
  # PERFORM INITIAL PROCESSING
  # Rename columns
  df <- df %>% 
    rename("First_Serve_Point" = '1st',
           "Second_Serve_Point" = '2nd',
           "Gm" = 'Gm#',
           "Pt_Winner" = "PtWinner",
           "Player_A_Sets" = "Set1",
           "Player_B_Sets" = "Set2",
           "Player_A_Games" = "Gm1",
           "Player_B_Games" = "Gm2")
  
  # remove tiebreak rows
  df <- df %>% 
    filter(!(Player_A_Games == 6 & Player_B_Games == 6)) %>% 
    arrange(Pt) %>% 
    mutate(Pt = row_number())
  
  # Server and Point Winner column now filled with A or B instead of 1 or 2
  df$Svr <- ifelse(df$Svr == 1, "A", "B")
  df$Pt_Winner <- ifelse(df$Pt_Winner == 1, "A", "B")
  
  # Create server points and games columns
  df <- df %>% 
    group_by(Svr) %>% 
    mutate(Server_Pt = row_number(),
           Server_Gm = cumsum(!duplicated(Gm))) %>% 
    ungroup() %>% 
    select(-TbSet)
  
  # Create set number column and game number in set column
  df <- df %>% 
    mutate(Set = Player_A_Sets + Player_B_Sets + 1) %>%
    group_by(Set) %>%
    arrange(Set, Gm) %>%
    mutate(Game_Number_Within_Set = dense_rank(Gm)) %>%
    ungroup()
  
  
  
  # CREATE GAME WINNER & GAME OUTCOME COLUMNS
  # Look ahead one row for Pts, Gm1, Gm2, Set1, Set2
  df <- df %>% 
    arrange(Pt)
  df$Next_Pts  <- c(df$Pts[-1], NA)
  df$Next_Player_A_Games  <- c(df$Player_A_Games[-1], NA)
  df$Next_Player_B_Games <- c(df$Player_B_Games[-1], NA)
  df$Next_Player_A_Sets  <- c(df$Player_A_Sets[-1], NA)
  df$Next_Player_B_Sets  <- c(df$Player_B_Sets[-1], NA)
  
  # Identify game-ending rows
  df$Game_End <- df$Next_Pts == "0-0" & 
    (as.numeric(df$Next_Player_A_Games) > as.numeric(df$Player_A_Games)   | 
       as.numeric(df$Next_Player_B_Games) > as.numeric(df$Player_B_Games) |
       as.numeric(df$Next_Player_A_Sets) > as.numeric(df$Player_A_Sets)   |
       as.numeric(df$Next_Player_B_Sets) > as.numeric(df$Player_B_Sets))
  
  # Assign GameWinner only at game-ending rows
  df$Game_Winner <- ifelse(df$Game_End, 
                           df$Pt_Winner, 
                           NA)
  
  # Deal with NA issue on final row
  last_row <- nrow(df)
  
  if (is.na(df$Game_Winner[last_row])) {
    df$Game_Winner[last_row] <- df$Pt_Winner[last_row]
    df$Game_End[last_row] <- TRUE
  }
  
  # Fill Game_Winner column, then assign Game_Outcome across all points in game
  df <- df %>% 
    group_by(Gm) %>% 
    fill(Game_Winner, .direction = "downup") %>% 
    ungroup() %>% 
    mutate(
      Game_Outcome = ifelse(Svr == Game_Winner, "HOLD", "BREAK")
    )
  
}



###############################################################
# 2 - CREATE FUNCTION TO IDENTIFY SERVE DIRECTION AND OUTCOMES
###############################################################

Find_Serve_Direction_Outcome <- function(df) {
  # Pull out serve directions from point strings
  df <- df %>% 
    mutate(First_Dir = str_sub(First_Serve_Point, 1, 1),
           Second_Dir = str_sub(Second_Serve_Point, 1, 1),
           
           First_Direction = ifelse(
             First_Dir == 4, "Wide", 
             ifelse(First_Dir == 5, "Body", "T")),
           
           Second_Direction = ifelse(
             Second_Dir == 4, "Wide", 
             ifelse(Second_Dir == 5, "Body", "T"))
    ) %>% 
    select(-c(First_Dir, Second_Dir))
  
  
  # Pull out First Serve outcomes from point strings
  df <- df %>% 
    mutate(First_Out = str_sub(First_Serve_Point, 1, 2),
           First_Outcome = 
             ifelse( First_Out %in% c("4n", "5n", "6n"), "Missed Net",
                     
                     ifelse( First_Out %in% c("4w", "5w", "6w"), "Missed Wide",
                             
                             ifelse( First_Out %in% c("4d", "5d", "6d"), "Missed Deep",
                                     
                                     ifelse( First_Out %in% c("4x", "5x", "6x"), "Missed Wide and Deep",
                                             
                                             ifelse( First_Out %in% c("4#", "5#", "6#"), "Service Winner",
                                                     
                                                     ifelse( First_Out %in% c("4*", "5*", "6*"), "Ace", "In-Play")
                                             )
                                     )
                             )
                     )
             )     
    ) %>% select(-First_Out)
  
  
  # Pull out Second Serve outcomes from point strings
  df <- df %>% 
    mutate(Second_Out = str_sub(Second_Serve_Point, 1, 2),
           Second_Outcome = 
             ifelse( Second_Out %in% c("4n", "5n", "6n"), "Missed Net",
                     
                     ifelse( Second_Out %in% c("4w", "5w", "6w"), "Missed Wide",
                             
                             ifelse( Second_Out %in% c("4d", "5d", "6d"), "Missed Deep",
                                     
                                     ifelse( Second_Out %in% c("4x", "5x", "6x"), "Missed Wide and Deep",
                                             
                                             ifelse( Second_Out %in% c("4#", "5#", "6#"), "Service Winner",
                                                     
                                                     ifelse( Second_Out %in% c("4*", "5*", "6*"), "Ace", 
                                                             
                                                             ifelse( is.na(Second_Out), NA, "In-Play") #ensures first serve points are assigned NA value
                                                     )
                                             )
                                     )
                             )
                     )
             )
    ) %>% select(-Second_Out)
}



####################################################
# 3 - CREATE FUNCTION TO CONVERT RALLY DESCRIPTIONS
####################################################

# list of valid characters for rally description
code_lookup <- list(
  
  # Serve Directions
  '4' = "Serve Out Wide",
  '5' = "Serve to Body",
  '6' = "Serve Down the T",
  
  # Rally Shots
  'f' = "forehand",
  'u' = "forehand drop shot",
  'b' = "backhand",
  'y' = "forehand drop shot",
  'r' = "forehand slice",
  's' = "backhand slice",
  'v' = "forehand volley",
  'z' = "backhand volley",
  'o' = "forehand overhead/smash",
  'p' = "backhand overhead/smash",
  'h' = "forehand half-volley",
  'i' = "backhand half-volley",
  'j' = "forehand swinging volley",
  'k' = "backhand swinging volley",
  'l' = "forehand lob",
  'm' = "backhand lob",
  't' = "trickshot",
  'q' = "unknown shot",
  
  # Error Descriptions
  'n' = "missed in net",
  'd' = "missed deep",
  'w' = "missed wide",
  'x' = "missed wide and deep",
  '!' = "shank",
  
  # Rally End Descriptions
  '@' = "unforced error",
  '#' = "forced error",
  '*' = "winner",
  
  # unusual ones
  '^' = "drop volley"
)


# Create function to be used to convert rallies from char string to text string
convert_rally <- function(code_str, lookup) {
  chars <- unlist(strsplit(code_str, ""))
  chars <- chars[chars %in% names(lookup)]
  n <- length(chars)
  rally_end_symbols <- c('@', '#', '*')
  
  # Serve Faults
  if (n == 2 && chars[2] %in% c('n','w','d','x')) {
    serve_desc <- lookup[[chars[1]]]
    fault_desc <- lookup[[chars[2]]]
    return(paste(serve_desc, fault_desc, sep = ", "))
  }
  
  
  # Rallies containing end symbols
  end_idx <- which(chars %in% rally_end_symbols)
  if (length(end_idx) == 0) return(NA)
  rally_end_pos <- end_idx[length(end_idx)]
  
  # Check for a fault code before the rally end symbol
  if (rally_end_pos > 1 && chars[rally_end_pos - 1] %in% c('n','w','d','x')) {
    shots <- chars[1:(rally_end_pos - 2)]
    fault_code <- chars[rally_end_pos - 1]
    rally_end_symbol <- chars[rally_end_pos]
    shot_desc <- sapply(shots, function(x) lookup[[x]])
    fault_desc <- lookup[[fault_code]]
    end_desc <- lookup[[rally_end_symbol]]
    paste0(paste(shot_desc, collapse="; "), " (", fault_desc, "), ", end_desc)
  } else {
    shots <- chars[1:(rally_end_pos - 1)]
    rally_end_symbol <- chars[rally_end_pos]
    shot_desc <- sapply(shots, function(x) lookup[[x]])
    end_desc <- lookup[[rally_end_symbol]]
    paste0(paste(shot_desc, collapse="; "), ", ", end_desc)
  }
}


# Create function to apply convert_rally function to specific columns
Apply_Convert_Rally <- function(df) {
  
  # apply convert_rally function to translate first and second serve rally descriptions
  df$First_Rally_Description <- sapply(df$First_Serve_Point, convert_rally, lookup = code_lookup)
  df$Second_Rally_Description <- sapply(df$Second_Serve_Point, convert_rally, lookup = code_lookup)
  
  # combine columns together
  df$Full_Rally_Description <- ifelse(
    is.na(df$Second_Rally_Description),
    df$First_Rally_Description,
    paste(df$First_Rally_Description, df$Second_Rally_Description, sep = ". ")
  )
  
  # remove old string columns
  df <- df %>% 
    select(-c(First_Serve_Point, Second_Serve_Point))
  
  
  # First Serve Point, Second Serve Point or Double Fault Flag columns
  # Assign 1 as Flag if true, 0 if not true
  df <- df %>% 
    mutate(
      FS_Point_Flag = ifelse(
        First_Outcome %in% c("Ace", "In-Play", "Service Winner"), 1, 0
      ),
      SS_Point_Flag = ifelse(
        Second_Outcome %in% c("Ace", "In-Play", "Service Winner"), 1, 0
      ),
      DF_Flag = ifelse(
        Second_Outcome %in% c("Missed Net", "Missed Deep", "Missed Wide", "Missed Wide and Deep"), 1, 0
      )
    )
  
}



###################################################################
# 4 - CREATE FUNCTION EXTRACT RALLY LENGTH FROM RALLY DESCRIPTIONS
###################################################################

# Create rally length function
get_rally_length <- function(desc) {
  if (is.na(desc) || desc == "") return(NA_integer_) #deal with NAs
  
  shots <- str_split(desc, ";")[[1]] %>% str_trim() #use ; to separate shots in description
  n <- length(shots) 
  
  if (n == 0) return(0)
  last_shot <- tolower(shots[n]) #makes last shot description and makes it lowercase
  
  if (str_detect(last_shot, "unforced error|forced error") && n >= 2) { #protects against issues with varied notation of service winners 
    n <- n - 1
  }
  return(n)
}


# Apply rally length function 
Apply_Rally_Length <- function(df) {
  df$Rally_Length <- NA_integer_
  df$Rally_Length[df$DF_Flag == 1] <- 0L
  df$Rally_Length[df$FS_Point_Flag == 1] <- sapply(df$First_Rally_Description[df$FS_Point_Flag == 1], get_rally_length)
  df$Rally_Length[df$SS_Point_Flag == 1] <- sapply(df$Second_Rally_Description[df$SS_Point_Flag == 1], get_rally_length)
  df
}



###########################################################
# 5 - CREATE FUNCTION TO PREPARE FOR STATISTIC CALCULATION
###########################################################

# Create function to create dummy variables for calculating statistics
Create_Dummy_Variables <- function(df) {
  
  df <- df %>% 
    mutate(
      
      # Serve-based logicals  
      FS_In = First_Outcome %in% c("In-Play", "Ace", "Service Winner"),
      FS_Ace = First_Outcome == "Ace",
      FS_Unreturned = First_Outcome %in% c("Ace, Service Winner") | (First_Outcome == "In-Play" & Rally_Length == 1),
      SS_In = Second_Outcome %in% c("In-Play", "Ace", "Service Winner"),
      Double_Fault = Second_Outcome %in% c("Missed Wide", "Missed Deep", "Missed Wide and Deep", "Missed Net"),
      Game_Points = Pts %in% c("40-0", "40-15", "40-30", "AD-40"),
      Deuce_Points = Pts == "40-40",
      Break_Points = Pts %in% c("0-40", "15-40", "30-40", "40-AD"),
      
      
      
      # Rally based logicals
      Short_Rally = Rally_Length < 5,
      Medium_Rally = Rally_Length >= 5 & Rally_Length <= 9,
      Long_Rally = Rally_Length > 9,
      
    )
}



#############################################################
# 6 - CREATE FUNCTION TO CREATE SUMMARY TABLE FOR EACH MATCH
#############################################################

Create_Summary_Table <- function(df) {
  
  summary_df <- df %>% 
    group_by(Svr, Gm, Game_Outcome) %>% 
    summarise(
      
      # Game number within set
      Game_Number_Within_Set = first(Game_Number_Within_Set),
      
      # Number of points in each game
      Game_Number_of_Points = n(),
      Game_FS_Points = sum(FS_In, na.rm = TRUE),
      Game_SS_Points = sum(SS_In, na.rm = TRUE),
      Game_DF_Points = sum(Double_Fault, na.rm = TRUE),
      Game_SGP_Points = sum(Game_Points, na.rm = TRUE),
      Game_DP_Points = sum(Deuce_Points, na.rm = TRUE),
      Game_RGP_Points = sum(Break_Points, na.rm = TRUE),
      
      
      # 1st serve % on Server Game Points
      Game_FS_SGP_N = sum(Game_Points == TRUE & FS_In == TRUE, na.rm = TRUE),
      Game_FS_SGP_Pct = ifelse(
        Game_SGP_Points == 0,
        NA,
        Game_FS_SGP_N / Game_SGP_Points *100),
      
      
      # 1st serve % on Deuce Points
      Game_FS_DP_N = sum(Deuce_Points == TRUE & FS_In == TRUE, na.rm = TRUE),
      Game_FS_DP_Pct = ifelse(
        Game_DP_Points == 0,
        NA,
        Game_FS_DP_N / Game_DP_Points *100),
      
      
      # 1st serve % on Returner Game Points (break points)
      Game_FS_RGP_N = sum(Break_Points == TRUE & FS_In == TRUE, na.rm = TRUE),
      Game_FS_RGP_Pct = ifelse(
        Game_RGP_Points == 0,
        NA,
        Game_FS_RGP_N / Game_RGP_Points *100),
      
      
      # Percentage of Server Game Points won by server
      Game_SGP_Won_N = sum(Game_Points == TRUE & Pt_Winner == Svr, na.rm = TRUE),
      Game_SGP_Won_Pct = ifelse(
        Game_SGP_Points == 0,
        NA,
        Game_SGP_Won_N / Game_SGP_Points *100),
      
      
      # Percentage of Deuce Points won by server
      Game_DP_Won_N = sum(Deuce_Points == TRUE & Pt_Winner == Svr, na.rm = TRUE),
      Game_DP_Won_Pct = ifelse(
        Game_DP_Points == 0,
        NA,
        Game_DP_Won_N / Game_DP_Points *100),
      
      
      # Percentage of Returner Game Points (break points) won by server
      Game_RGP_Won_N = sum(Break_Points == TRUE & Pt_Winner == Svr, na.rm = TRUE),
      Game_RGP_Won_Pct = ifelse(
        Game_RGP_Points == 0,
        NA,
        Game_RGP_Won_N / Game_RGP_Points *100),
      
      
      # 1st-serve %  =   # of first‐serves in / total attempt
      Game_FS_Pct = (Game_FS_Points / Game_Number_of_Points)*100,
      
      
      # % of 1st-serve points won by server
      Game_FS_Won_N = sum(FS_In == TRUE & Pt_Winner == Svr, na.rm = TRUE),
      Game_FS_Won_Pct = ifelse(
        Game_FS_Points == 0,
        NA,
        Game_FS_Won_N / Game_FS_Points * 100),
      
      
      # % of 2nd-serve points won by server
      Game_SS_Won_N = sum(SS_In == TRUE & Pt_Winner == Svr, na.rm = TRUE),
      Game_SS_Won_Pct = ifelse(
        Game_SS_Points == 0,
        NA,
        Game_SS_Won_N / Game_SS_Points * 100),
      
      
      # % of 1st-serves that were not returned in court
      Game_FS_Unreturned_N = sum(FS_Unreturned, na.rm = TRUE),
      Game_FS_Unreturned_Pct = ifelse(
        Game_FS_Points == 0,
        NA,
        sum(Game_FS_Unreturned_N, na.rm = TRUE) / Game_FS_Points * 100),
      
      
      # Average rally length in game
      Game_Avg_Rally_Length = mean(Rally_Length, na.rm = TRUE),
      
      
      # Average rally length of first serve points in game
      Game_FS_Avg_Rally_Length = sum(Rally_Length[FS_In == TRUE], na.rm = TRUE) / Game_FS_Points,
      
      
      # Average rally length of second serve points in game
      Game_SS_Avg_Rally_Length = sum(Rally_Length[SS_In == TRUE], na.rm = TRUE) / Game_SS_Points,
      
      
      # Longest rally length in game
      Game_Max_Rally_Length = max(Rally_Length, na.rm = TRUE),
      
      
      # Percentage of rallies under 5 shots in each game
      Game_Short_Rally_N = sum(Short_Rally, na.rm = TRUE),
      Game_Short_Rally_Pct = mean(Short_Rally, na.rm = TRUE)*100,
      
      
      # Percentage of rallies between 5 and 9 shots in each game
      Game_Medium_Rally_N = sum(Medium_Rally, na.rm = TRUE),
      Game_Medium_Rally_Pct = mean(Medium_Rally, na.rm = TRUE)*100,
      
      
      # Percentage of rallies over 9 shots in each game
      Game_Long_Rally_N = sum(Long_Rally, na.rm = TRUE),
      Game_Long_Rally_Pct = mean(Long_Rally, na.rm = TRUE)*100,
      
      
      # Percentage 1st serve rallies under 5 shots
      Game_FS_Short_Rally_N = sum(FS_In & Short_Rally, na.rm = TRUE),
      Game_FS_Short_Rally_Pct = ifelse(
        Game_FS_Points == 0,
        NA,
        Game_FS_Short_Rally_N / sum(FS_In, na.rm = TRUE) * 100
      ),
      
      
      # Percentage 1st serve rallies between 5 and 9 shots
      Game_FS_Medium_Rally_N = sum(FS_In & Medium_Rally, na.rm = TRUE),
      Game_FS_Medium_Rally_Pct = ifelse(
        Game_FS_Points == 0,
        NA,
        Game_FS_Medium_Rally_N / sum(FS_In, na.rm = TRUE) * 100
      ),
      
      
      # Percentage 1st serve rallies over 9 shots
      Game_FS_Long_Rally_N = sum(FS_In & Long_Rally, na.rm = TRUE),
      Game_FS_Long_Rally_Pct = ifelse(
        Game_FS_Points == 0,
        NA,
        Game_FS_Long_Rally_N / sum(FS_In, na.rm = TRUE) * 100
      ),
      
      
      # Percentage 1st serve rallies under 5 shots won by server
      Game_FS_Short_Rally_Win_N = sum(FS_In & Short_Rally & Pt_Winner == Svr),
      Game_FS_Short_Rally_Win_Pct = ifelse(
        Game_FS_Short_Rally_N == 0, 
        NA,
        sum(FS_In & Short_Rally & (Pt_Winner == Svr), na.rm = TRUE) / Game_FS_Short_Rally_N * 100
      ),
      
      
      # Percentage 1st serve rallies between 5 and 9 shots won by server
      Game_FS_Medium_Rally_Win_N = sum(FS_In & Medium_Rally & Pt_Winner == Svr),
      Game_FS_Medium_Rally_Win_Pct = ifelse(
        Game_FS_Medium_Rally_N == 0,
        NA,
        sum(FS_In & Medium_Rally & (Pt_Winner == Svr), na.rm = TRUE) / Game_FS_Medium_Rally_N * 100
      ),
      
      
      # Percentage 1st serve rallies over 9 shots won by server
      Game_FS_Long_Rally_Win_N = sum(FS_In & Long_Rally & Pt_Winner == Svr),
      Game_FS_Long_Rally_Win_Pct = ifelse(
        Game_FS_Long_Rally_N == 0,
        NA,
        sum(FS_In & Long_Rally & (Pt_Winner == Svr), na.rm = TRUE) / Game_FS_Long_Rally_N * 100
      )
      
    ) %>%
    ungroup()
  return(summary_df)
}



#######################################################
# 7 - CREATE FUNCTION TO ADD ROLLING 3-GAME STATISTICS
#######################################################


Create_Rolling_Metrics <- function (df) {
  
  df <- df %>% 
    arrange(Svr, Gm) %>%   # Order by server and game
    group_by(Svr) %>%
    mutate(
      
      
      # 3-Game Totals for Different Point Types
      Roll3_Number_of_Points = slide_dbl(Game_Number_of_Points, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      Roll3_FS_Points = slide_dbl(Game_FS_Points, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      Roll3_SS_Points = slide_dbl(Game_SS_Points, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      Roll3_DF_Points = slide_dbl(Game_DF_Points, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      Roll3_SGP_Points = slide_dbl(Game_SGP_Points, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      Roll3_DP_Points = slide_dbl(Game_DP_Points, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      Roll3_RGP_Points = slide_dbl(Game_RGP_Points, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      
      
      # 3-Game Number and Percentage of Server Game Points where server completed first serve
      Roll3_FS_SGP_N = slide_dbl(Game_FS_SGP_N, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      Roll3_FS_SGP_Pct = ifelse(
        Roll3_SGP_Points == 0,
        NA,
        (Roll3_FS_SGP_N / Roll3_SGP_Points) * 100
      ),
      
      
      # 3-Game Number and Percentage of Deuce Points where server completed first serve
      Roll3_FS_DP_N = slide_dbl(Game_FS_DP_N, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      Roll3_FS_DP_Pct = ifelse(
        Roll3_DP_Points == 0,
        NA,
        (Roll3_FS_DP_N / Roll3_DP_Points) * 100
      ),
      
      
      # 3-Game Number and Percentage of Returner Game Points where server completed first serve
      Roll3_FS_RGP_N = slide_dbl(Game_FS_RGP_N, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      Roll3_FS_RGP_Pct = ifelse(
        Roll3_RGP_Points == 0,
        NA,
        (Roll3_FS_RGP_N / Roll3_RGP_Points) * 100
      ),
      
      
      # Number and Percentage of Server Game Points won by the server
      Roll3_SGP_Won_N = slide_dbl(Game_SGP_Won_N, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      Roll3_SGP_Won_Pct = ifelse(
        Roll3_SGP_Points == 0,
        NA,
        (Roll3_SGP_Won_N / Roll3_SGP_Points) * 100
      ),
      
      
      # Number and Percentage of Deuce Points won by the server
      Roll3_DP_Won_N = slide_dbl(Game_DP_Won_N, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      Roll3_DP_Won_Pct = ifelse(
        Roll3_DP_Points == 0,
        NA,
        (Roll3_DP_Won_N / Roll3_DP_Points) * 100
      ),
      
      
      # Number and Percentage of Returner Game Points (break points) won by the server
      Roll3_RGP_Won_N = slide_dbl(Game_RGP_Won_N, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      Roll3_RGP_Won_Pct = ifelse(
        Roll3_RGP_Points == 0,
        NA,
        (Roll3_RGP_Won_N / Roll3_RGP_Points) * 100
      ),
      
      
      # 3 game 1st serve %
      Roll3_FS_Pct = (Roll3_FS_Points / Roll3_Number_of_Points) * 100, 
      
      
      # 3 game % of 1st serve points won by the server
      Roll3_FS_Won_N = slide_dbl(Game_FS_Won_N, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      Roll3_FS_Won_Pct = ifelse(
        Roll3_FS_Points == 0,
        NA,
        (Roll3_FS_Won_N / Roll3_FS_Points) * 100
      ),
      
      
      # 3 game % of 2nd serve points won by the server
      Roll3_SS_Won_N = slide_dbl(Game_SS_Won_N, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      Roll3_SS_Won_Pct = ifelse(
        Roll3_SS_Points == 0,
        NA,
        (Roll3_SS_Won_N / Roll3_SS_Points )* 100
      ),
      
      
      # 3 game % of 1st-serves that were not returned in court
      Roll3_FS_Unreturned_N = slide_dbl(Game_FS_Unreturned_N, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      Roll3_FS_Unreturned_Pct = ifelse(
        Roll3_FS_Points == 0,
        NA,
        (Roll3_FS_Unreturned_N / Roll3_FS_Points) * 100
      ),
      
      
      # Average rally length in three game window 
      Roll3_Avg_Rally_Length = slide_dbl(.x = seq_along(Game_Avg_Rally_Length),
                                         .f = function(idx) {
                                           total_rally =  sum(Game_Avg_Rally_Length[idx] * Game_Number_of_Points[idx], na.rm = TRUE)
                                           total_points = sum(Game_Number_of_Points[idx], na.rm = TRUE)
                                           total_rally / total_points
                                         },
                                         .before = 2,
                                         .complete = FALSE),
      
      
      # Average first serve rally length in three game window
      Roll3_FS_Avg_Rally_Length = slide_dbl(.x = seq_along(Game_FS_Avg_Rally_Length),
                                            .f = function(idx) {
                                              total_fs_rally = sum(Game_FS_Avg_Rally_Length[idx] * Game_FS_Points[idx], na.rm = TRUE)
                                              total_fs_points = sum(Game_FS_Points[idx], na.rm = TRUE)
                                              total_fs_rally / total_fs_points
                                            },
                                            .before = 2,
                                            .complete = FALSE),
      
      
      # Average second serve rally length in three game window
      Roll3_SS_Avg_Rally_Length = slide_dbl(.x = seq_along(Game_SS_Avg_Rally_Length),
                                            .f = function(idx) {
                                              total_ss_rally = sum(Game_SS_Avg_Rally_Length[idx] * Game_SS_Points[idx], na.rm = TRUE)
                                              total_ss_points = sum(Game_SS_Points[idx], na.rm = TRUE)
                                              total_ss_rally / total_ss_points
                                            },
                                            .before = 2,
                                            .complete = FALSE),
      
      
      # Longest rally length in three game window
      Roll3_Max_Rally_Length = slide_dbl(Game_Max_Rally_Length, max, .before = 2, .complete = FALSE, na.rm = TRUE),
      
      
      # Percentage of rallies under 5 shots in three game window
      Roll3_Short_Rally_N = slide_dbl(Game_Short_Rally_N, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      Roll3_Short_Rally_Pct = (Roll3_Short_Rally_N / Roll3_Number_of_Points) * 100,
      
      
      # Percentage of rallies between 5 and 9 shots in three game window
      Roll3_Medium_Rally_N = slide_dbl(Game_Medium_Rally_N, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      Roll3_Medium_Rally_Pct = (Roll3_Medium_Rally_N / Roll3_Number_of_Points) * 100,
      
      
      # Percentage of rallies over 9 shots in three game window
      Roll3_Long_Rally_N = slide_dbl(Game_Long_Rally_N, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      Roll3_Long_Rally_Pct = (Roll3_Long_Rally_N / Roll3_Number_of_Points) * 100,
      
      
      # Percentage of first serve rallies under 5 shots in three game window
      Roll3_FS_Short_Rally_N = slide_dbl(Game_FS_Short_Rally_N, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      Roll3_FS_Short_Rally_Pct = (Roll3_FS_Short_Rally_N / Roll3_FS_Points) * 100,
      
      
      # Percentage of first serve rallies between 5 and 9 shots in three game window
      Roll3_FS_Medium_Rally_N = slide_dbl(Game_FS_Medium_Rally_N, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      Roll3_FS_Medium_Rally_Pct = (Roll3_FS_Medium_Rally_N / Roll3_FS_Points) * 100, 
      
      
      # Percentage of first serve rallies over 9 shots in three game window
      Roll3_FS_Long_Rally_N = slide_dbl(Game_FS_Long_Rally_N, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      Roll3_FS_Long_Rally_Pct = (Roll3_FS_Long_Rally_N / Roll3_FS_Points) * 100,
      
      
      # Percentage of first serve rallies under 5 shots won by the server
      Roll3_FS_Short_Rally_Win_N = slide_dbl(Game_FS_Short_Rally_Win_N, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      Roll3_FS_Short_Rally_Win_Pct = ifelse(
        Roll3_FS_Short_Rally_N == 0,
        NA,
        (Roll3_FS_Short_Rally_Win_N / Roll3_FS_Short_Rally_N) * 100
      ), 
      
      
      # Percentage of first serve rallies between 5 and 9 shots won by the server
      Roll3_FS_Medium_Rally_Win_N = slide_dbl(Game_FS_Medium_Rally_Win_N, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      Roll3_FS_Medium_Rally_Win_Pct = ifelse(
        Roll3_FS_Medium_Rally_N == 0,
        NA,
        (Roll3_FS_Medium_Rally_Win_N / Roll3_FS_Medium_Rally_N) * 100
      ),
      
      
      # Percentage of first serve rallies over 9 shots won by the server
      Roll3_FS_Long_Rally_Win_N = slide_dbl(Game_FS_Long_Rally_Win_N, sum, .before = 2, .complete = FALSE, na.rm = TRUE),
      Roll3_FS_Long_Rally_Win_Pct = ifelse(
        Roll3_FS_Long_Rally_N == 0,
        NA,
        (Roll3_FS_Long_Rally_Win_N / Roll3_FS_Long_Rally_N) * 100
      )
      
    ) %>%
    ungroup()
}






#############################
# APPLY FUNCTIONS TO DF LIST
#############################

match_dfs1 <- lapply(match_dfs, Prepare_Dfs)
match_dfs2 <- lapply(match_dfs1, Find_Serve_Direction_Outcome)
match_dfs3 <- lapply(match_dfs2, Apply_Convert_Rally)
match_dfs4 <- lapply(match_dfs3, Apply_Rally_Length)
match_dfs5 <- lapply(match_dfs4, Create_Dummy_Variables)


# Create summary tables of data
list_summaries <- lapply(match_dfs5, Create_Summary_Table)
list_summaries1 <- lapply(list_summaries, Create_Rolling_Metrics)


# Add Match ID column to all dfs in list
match_ids <- c(
  
  # Training Matches
  "WIM2012QF_FEDvYOU",
  "WIM2012SF_MURvTSO",
  "WIM2012SF_FEDvDJO",
  "WIM2011QF_FEDvTSO",
  "WIM2011SF_MURvNAD",
  "WIM2011SF_DJOvTSO",
  "WIM2011F_DJOvNAD",
  "WIM2010QF_FEDvBER",
  "WIM2010SF_MURvNAD",
  "WIM2010SF_BERvDJO",
  "WIM2010F_BERvNAD", 
  
  # Testing Matches
  "OLY2012F_MURvFED",
  "WIM2012F_FEDvMUR")


# Run loop to add Match ID column to every df
for (i in seq_along(list_summaries1)) {
  list_summaries1[[i]]$Match_ID <- match_ids[i]
}


# Combine tibbles into full dataset of game 
full_data <- bind_rows(list_summaries1)


# Move Match_ID to first column
full_data <- full_data %>% 
  relocate(Match_ID)


# Clean datasets in environment
objects_to_keep <- "full_data"
rm(list = setdiff(ls(), objects_to_keep))



#############################################
# PROCESSING OF DATA FOR FIRST XGBOOST MODEL
############################################

# Inspect dataset, ensure all columns are numeric
str(full_data)
summary(full_data)
colSums(is.na(full_data)) # no NA's in columns


# Convert game outcome to factor
full_data$Game_Outcome <- factor(full_data$Game_Outcome, levels = c("HOLD", "BREAK"))
full_data$Game_Outcome <- ifelse(full_data$Game_Outcome == "BREAK", 1, 0)


# Remove Server variable and rename Gm variable
full_data1 <- full_data %>% 
  select(-Svr) %>%  #no longer required
  rename(Game_Number_in_Match = Gm) #for clarity


# Remove Numeric Count columns
full_data1 <- full_data1 %>% 
  select(-ends_with("_N"))


# Define testing matches
testing_matches <- c("OLY2012F_MURvFED", "WIM2012F_FEDvMUR")


# Create training dataset 
training_data <- full_data1 %>% 
  filter(!Match_ID %in% testing_matches) %>% 
  select(-Match_ID)


# Create list of game outcomes for training dataset, then remove game outcomes from training dataset
training_labels <- training_data$Game_Outcome
training_features <- training_data %>% 
  select(-Game_Outcome)


# create testing dataset
testing_data <- full_data1 %>% 
  filter(Match_ID %in% testing_matches) %>% 
  select(-Match_ID)

# Create list of game outcomes for training dataset, then remove game outcomes from training dataset
testing_labels <- testing_data$Game_Outcome
testing_features <- testing_data %>% 
  select(-Game_Outcome)


# Create initial matrixes for xgboost 
train_feature_matrix <- as.matrix(training_features) 
test_feature_matrix <- as.matrix(testing_features)


# Create Datasets for use in XGBOOST
xgb_training_matrix <- xgb.DMatrix(data = train_feature_matrix, label = training_labels)
xgb_testing_matrix <- xgb.DMatrix(data = test_feature_matrix, label = testing_labels)



####################################
# CREATION OF INITIAL XGBOOST MODEL
####################################

# create list of learning rates to test through
learning_rates <- c(0.01, 0.05, 0.1, 0.2, 0.3)

best_eta <- NULL     #create placeholders in environment
best_nrounds <- NULL 
best_logloss <- Inf  

# for loop to identify 
for (eta in learning_rates) {
  parameters <- list(
    objective = "binary:logistic",  # for classifying binary outcomes
    eval_metric = "logloss",       # for predicting probability of classes
    eta = eta,                      # will iterate through different eta's
    seed = 123 
  )
  
  folds <- createFolds(training_labels, k = 5)
  
  cv <- xgb.cv(
    params = parameters,
    data = xgb_training_matrix,
    nrounds = 100,
    nfold = 5,
    early_stopping_rounds = 10,
    verbose = 1,
    showsd = TRUE,
    print_every_n = 10
  )
  
  # Check the structure of cv and extract the logloss for each iteration
  if ("evaluation_log" %in% names(cv)) {
    # This contains logloss values for each fold and iteration
    logloss_values <- cv$evaluation_log$test_logloss_mean
    
    # Find the minimum logloss and corresponding iteration
    min_logloss <- min(logloss_values, na.rm = TRUE)
    best_iteration <- which.min(logloss_values)
    
    # If this is the best logloss, store the parameters
    if (min_logloss < best_logloss) {
      best_logloss <- min_logloss
      best_eta <- eta
      best_nrounds <- best_iteration
    }
    
    # Print debugging output for current eta
    cat("Results of Testing Listed Below\n")
    cat("Chosen Eta = ", eta, "\n")
    cat("Min Logloss = ", min_logloss, "\n")
    cat("Best nRounds = ", best_iteration, "\n")
    cat("\n")
  }
}


# After the loop, print the best eta and best nrounds found
cat("Best eta:", best_eta, "\n")
cat("Best nrounds:", best_nrounds, "\n")
cat("Best logloss:", best_logloss, "\n")


# Set final parameters
final_parameters <- list(            #updated parameter list with best nround and eta values
  objective = "binary:logistic",
  eval_metric = "logloss",     
  eta = best_eta) 


# Set seed for reproducibility
set.seed(123)  # Ensures reproducibility of random processes


# Train final model with best number of rounds
xgb_model <- xgb.train(
  params = final_parameters,
  data = xgb_training_matrix,
  nrounds = best_nrounds,
  watchlist = list(train = xgb_training_matrix),
  verbose = 1,
)


# Predict class labels
pred_labels <- predict(xgb_model, xgb_testing_matrix)
pred_labels_class <- ifelse(pred_labels > 0.5, 1, 0) #assign breaks where prob is over 0.5


# Make basic Confusion Matrix of performance
ConfMatrix1 <- confusionMatrix(
  factor(pred_labels_class, levels = 0:1, labels = c("Hold", "Break")),
  factor(testing_labels, levels = 0:1, labels = c("Hold", "Break"))
)
print(ConfMatrix1) # results show accurate classification of all games in dataset


# Calculate SHAP values of model 
shap_values1 <- shap.values(xgb_model = xgb_model, X_train = test_feature_matrix)
shap_importance1 <- shap_values1$mean_shap_score
print(shap_importance1)


# Violin chart
shap_long <- shap.prep(shap_contrib = shap_values1$shap_score, X_train = test_feature_matrix)
shap.plot.summary(shap_long)

# the above SHAP values indicate that critical point metrics are strong predictors of outcome.
# broad game level win % stats may also result in data leakage
# a refined model will include these statistics only at the 3 game rolling window level




##############################################
# PROCESSING OF DATA FOR SECOND XGBOOST MODEL
##############################################

# Remove game level critical point measures and game level 1st/2nd serve win %'s
full_data2 <- full_data %>% 
  select(-c(Game_SGP_Points, Game_DP_Points, Game_RGP_Points,
            Game_FS_SGP_Pct, Game_FS_DP_Pct, Game_FS_RGP_Pct,
            Game_SGP_Won_Pct, Game_DP_Won_Pct, Game_RGP_Won_Pct,
            Game_FS_Won_Pct, Game_SS_Won_Pct)) %>% 
  select(-ends_with("_N"))


# Create copy for use after classification
full_data_final <- full_data2


# Create training dataset 
training_data2 <- full_data2 %>% 
  select(-Svr) %>% 
  filter(!Match_ID %in% testing_matches) %>% 
  select(-Match_ID)

# Create list of game outcomes for training dataset, then remove game outcomes from training dataset
training_labels2 <- training_data2$Game_Outcome
training_features2 <- training_data2 %>% 
  select(-Game_Outcome)


# create testing dataset
testing_data2 <- full_data2 %>%
  select(-Svr) %>% 
  filter(Match_ID %in% testing_matches) %>% 
  select(-Match_ID)

# Create list of game outcomes for training dataset, then remove game outcomes from training dataset
testing_labels2 <- testing_data2$Game_Outcome
testing_features2 <- testing_data2 %>% 
  select(-Game_Outcome)


# Create initial matrixes for xgboost 
train_feature_matrix2 <- as.matrix(training_features2) 
test_feature_matrix2 <- as.matrix(testing_features2)


# Create Datasets for use in XGBOOST
xgb_training_matrix2 <- xgb.DMatrix(data = train_feature_matrix2, label = training_labels2)
xgb_testing_matrix2 <- xgb.DMatrix(data = test_feature_matrix2, label = testing_labels2)



###################################
# CREATION OF SECOND XGBOOST MODEL
###################################

# create list of learning rates to test through
learning_rates <- c(0.01, 0.05, 0.1, 0.2, 0.3)

best_eta2 <- NULL     #create placeholders in environment
best_nrounds2 <- NULL 
best_logloss2 <- Inf  

# for loop to identify 
for (eta in learning_rates) {
  parameters <- list(
    objective = "binary:logistic",  # for classifying binary outcomes
    eval_metric = "logloss",       # for predicting probability of classes
    eta = eta,                      # will iterate through different eta's
    seed = 123 
  )
  
  folds <- createFolds(training_labels2, k = 5)
  
  cv <- xgb.cv(
    params = parameters,
    data = xgb_training_matrix2,
    nrounds = 100,
    nfold = 5,
    early_stopping_rounds = 10,
    verbose = 1,
    showsd = TRUE,
    print_every_n = 10
  )
  
  # Check the structure of cv and extract the logloss for each iteration
  if ("evaluation_log" %in% names(cv)) {
    # This contains logloss values for each fold and iteration
    logloss_values <- cv$evaluation_log$test_logloss_mean
    
    # Find the minimum logloss and corresponding iteration
    min_logloss <- min(logloss_values, na.rm = TRUE)
    best_iteration <- which.min(logloss_values)
    
    # If this is the best logloss, store the parameters
    if (min_logloss < best_logloss2) {
      best_logloss2 <- min_logloss
      best_eta2 <- eta
      best_nrounds2 <- best_iteration
    }
    
    # Print debugging output for current eta
    cat("Results of Testing Listed Below\n")
    cat("Chosen Eta = ", eta, "\n")
    cat("Min Logloss = ", min_logloss, "\n")
    cat("Best nRounds = ", best_iteration, "\n")
    cat("\n")
  }
}

# After the loop, print the best eta and best nrounds found
cat("Best eta:", best_eta2, "\n")
cat("Best nrounds:", best_nrounds2, "\n")
cat("Best logloss:", best_logloss2, "\n")


# Set final parameters
final_parameters <- list(            #updated with best nround and eta values
  objective = "binary:logistic",
  eval_metric = "logloss",     
  eta = best_eta2) 


# Set seed for reproducibility
set.seed(123)  # Ensures reproducibility of random processes


# Train final model with best number of rounds
xgb_model2 <- xgb.train(
  params = final_parameters,
  data = xgb_training_matrix2,
  nrounds = best_nrounds2,
  watchlist = list(train = xgb_training_matrix2),
  verbose = 1,
)


# Predict class labels
pred_labels2 <- predict(xgb_model2, xgb_testing_matrix2)
pred_labels_class2 <- ifelse(pred_labels2 > 0.5, 1, 0) #assign breaks where prob is over 0.75


# Make basic Confusion Matrix of performance
ConfMatrix2 <- confusionMatrix(
  factor(pred_labels_class2, levels = 0:1, labels = c("Hold", "Break")),
  factor(testing_labels2, levels = 0:1, labels = c("Hold", "Break"))
)
print(ConfMatrix2)



############################################
# FIND & APPLY BEST THRESHOLD VIA ROC CURVE
############################################

# Create ROC Curve Plot, calculate and add AUC value
roc_obj <- roc(response = testing_labels2, predictor = pred_labels2)
plot(roc_obj, main = "ROC Curve for XGBoost Model")
auc_value <- auc(roc_obj)
print(auc_value)


# Find best threshold for model using ROC Curve
best_coords <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity", "accuracy"), transpose = FALSE)
best_threshold <- best_coords$threshold


# Re classify outcomes on best threshold 
pred_labels_class3 <- ifelse(pred_labels2 > best_threshold, 1, 0)


# Make basic Confusion Matrix of performance
ConfMatrix3 <- confusionMatrix(
  factor(pred_labels_class3, levels = 0:1, labels = c("Hold", "Break")),
  factor(testing_labels2, levels = 0:1, labels = c("Hold", "Break"))
)
print(ConfMatrix3)


# Calculate SHAP values of model 2
shap_values2 <- shap.values(xgb_model = xgb_model2, X_train = test_feature_matrix2)
shap_importance2 <- shap_values2$mean_shap_score
print(shap_importance2)


# Violin chart
shap_long <- shap.prep(shap_contrib = shap_values2$shap_score, X_train = test_feature_matrix2)
shap.plot.summary(shap_long, min_color_bound = "#7cfc00")


# Top 5 Features by SHAP Value
top5_shap_values <- sort(shap_importance2, decreasing = TRUE)[1:5]
top5_shap_features <- names(sort(shap_importance2, decreasing = TRUE)[1:5])
print(top5_shap_features)



###########################################
# CREATING FINAL CASE STUDY RESULTS TABLE
###########################################

# Create final dataset for comparing results
testing_matches_final <- full_data2 %>% 
  filter(Match_ID %in% testing_matches)


# Add Predicted Game Outcomes Column to testing dataset
testing_matches_final$Pred_Game_Outcome <- pred_labels_class3
testing_matches_final <- testing_matches_final %>% 
  relocate(Pred_Game_Outcome) %>% 
  relocate(Game_Outcome) %>% 
  relocate(Match_ID)


# Convert predicted binary outcomes to text outcomes
testing_matches_final <- testing_matches_final %>% 
  mutate(Pred_Game_Outcome = ifelse(as.numeric(Pred_Game_Outcome) == 0, "HOLD", "BREAK"),
         Game_Outcome = ifelse(as.numeric(Game_Outcome) == 0, "HOLD", "BREAK") )


# Create summary table for use in results
Summary_Table_Features <- testing_matches_final %>% 
  select(
    Game_Outcome, Pred_Game_Outcome, all_of(top5_shap_features)
  ) %>% 
  mutate(
    Prediction_Outcome = 
      ifelse(Game_Outcome == "HOLD" & Pred_Game_Outcome == "HOLD", "HOLD Correctly Classified",
             ifelse(Game_Outcome == "BREAK" & Pred_Game_Outcome == "BREAK", "BREAK Correctly Classified", "HOLD Incorrectly Classified")
    )
  ) %>% 
  relocate(Prediction_Outcome) %>%
  select(-c(Game_Outcome, Pred_Game_Outcome))


# Pivot longer so feature columns are in a single column
Summary_Table_Features <- Summary_Table_Features %>%
  pivot_longer(
    cols = -Prediction_Outcome,
    names_to = "Feature",
    values_to = "Value"
  )


# Group and summarize by feature and outcome, maintaining feature order
original_feature_order <- unique(Summary_Table_Features$Feature)

Summary_Table_Features <- Summary_Table_Features %>%
  filter(!is.na(Value)) %>%
  mutate(Feature = factor(Feature, levels = original_feature_order)) %>%
  group_by(Feature, Prediction_Outcome) %>%
  summarise(Mean_Value = mean(Value), .groups = "drop") %>%
  pivot_wider(
    names_from = Prediction_Outcome,
    values_from = Mean_Value
  )


# Add feature importance scores, reorder columns to 
Summary_Table_Features <- Summary_Table_Features %>% 
  mutate(
    'Mean SHAP Value' = top5_shap_values
  ) %>% 
  relocate(
    Feature,
    `Mean SHAP Value`
  ) %>% 
  rename( 'Feature Name' = Feature)


# Round values to 2 dp for mean values
Summary_Table_Features <- Summary_Table_Features %>% 
  mutate(
    across(
      c(`Mean SHAP Value`, `BREAK Correctly Classified`, `HOLD Correctly Classified`, `HOLD Incorrectly Classified`),
      ~ round(.x, 2)
    )
  )


# Write to csv for use in dissertation
write.csv(Summary_Table_Features, file = "B1706_Table3.csv", row.names = FALSE)


################################
# FINAL CLASSIFICATION RESULTS
################################

# Find incorrectly predicted games
Classification_Errors <- testing_matches_final %>% 
  filter(Game_Outcome == "HOLD" & Pred_Game_Outcome == "BREAK" |
           Game_Outcome == "BREAK" & Pred_Game_Outcome == "HOLD" )

# Rename Server column to player names
Classification_Errors <- Classification_Errors %>%
  mutate(Svr = case_when(
    Match_ID == "OLY2012F_MURvFED" & Svr == "A" ~ "MURRAY",
    Match_ID == "OLY2012F_MURvFED" & Svr == "B" ~ "FEDERER",
    Match_ID == "WIM2012F_FEDvMUR" & Svr == "A" ~ "FEDERER",
    Match_ID == "WIM2012F_FEDvMUR" & Svr == "B" ~ "MURRAY",
    TRUE ~ Svr  # Keep original value if none match
  ))
view(Classification_Errors)


# Successful Predictions of Holds
Classification_Hold_Correct <- testing_matches_final %>% 
  filter(Game_Outcome == "HOLD" & Pred_Game_Outcome == "HOLD")


# Rename Server column to player names
Classification_Hold_Correct <- Classification_Hold_Correct %>%
  mutate(Svr = case_when(
    Match_ID == "OLY2012F_MURvFED" & Svr == "A" ~ "MURRAY",
    Match_ID == "OLY2012F_MURvFED" & Svr == "B" ~ "FEDERER",
    Match_ID == "WIM2012F_FEDvMUR" & Svr == "A" ~ "FEDERER",
    Match_ID == "WIM2012F_FEDvMUR" & Svr == "B" ~ "MURRAY",
    TRUE ~ Svr  # Keep original value if none match
  ))
view(Classification_Hold_Correct)


# Successful Predictions of Breaks
Classification_Break_Correct <- testing_matches_final %>% 
  filter(Game_Outcome == "BREAK" & Pred_Game_Outcome == "BREAK")

# Rename Server column to player names
Classification_Break_Correct <- Classification_Break_Correct %>%
  mutate(Svr = case_when(
    Match_ID == "OLY2012F_MURvFED" & Svr == "A" ~ "MURRAY",
    Match_ID == "OLY2012F_MURvFED" & Svr == "B" ~ "FEDERER",
    Match_ID == "WIM2012F_FEDvMUR" & Svr == "A" ~ "FEDERER",
    Match_ID == "WIM2012F_FEDvMUR" & Svr == "B" ~ "MURRAY",
    TRUE ~ Svr  # Keep original value if none match
  ))
view(Classification_Break_Correct)

# Summary Stats for table
summary(Classification_Errors)
summary(Classification_Hold_Correct)
summary(Classification_Break_Correct)

# identify dfs to keep and clean environment
dfs_to_keep <-  c("shap_values2", "shap_importance2", "ConfMatrix3",
                  "Summary_Table_Features", "Classification_Errors", 
                  "Classification_Hold_Correct", "Classification_Break_Correct")

rm(list = setdiff(ls(), dfs_to_keep))
