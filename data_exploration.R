###############################################################################_
############### Knowledge Graphs Portfolio - Data Preperation ----
###############################################################################_

#' All the initial data exploration and manipulation is handled in this file.
rm(list = ls())
getwd()
setwd("C:/mahmoud uni/TU/SS2024/KGs/Portfolio")

library(tidyverse)
library(stringr)

players_df <- read.csv2("dataset/player_dataset.csv", sep = ",")
teams_df <- read.csv2("dataset/team_dataset.csv", sep = ",")


############################# Choose Columns and Fix initial issues ----


#' First we will remove all the redundant columns
#' Wir gehen mal die Columns durch und schauen welche Variablen wir rausschmeißen 
#' und welche wir manipulierne bevor wir uns um die NAs kümmern
#'  - Club_position will be changed to whether player is a starter or not.
#'  - player_position will be changed so that its just GK, DEF, MF, Attack
#'  - Club_loaned_from will be transformed into a binary variable on_loan
#'  work rate aufteilen in workrate off und workrate def
#'  Wir werfen die positions spezifischen Ratings weg da 
#'  die relevante information sich im allgemeinen Rating sowie 
#'  in den einzelnen stats befindet. Wir lassen die Nationalmannschafts 
#'  daten weg, weil fifa nicht alle nationalitäten beinhaltet und somit 
#'  information verloren gehen würde 
#'  Release Clause wird gedroppt weil diese daten nicht der realität entsprechen

red_cols <- c('fifa_update', 'update_as_of', 'short_name', 'club_jersey_number', 'nation_team_id', 
              'nation_position', 'dob', 'nation_jersey_number', 'player_tags',
              'player_traits', 'real_face', "release_clause_eur",'ls', 'st', 'rs', 'lw', 'lf',
              'cf', 'rf', 'rw', 'lam', 'cam', 'ram', 'lm', 'lcm', 'cm',  'rcm', 'rm', 'lwb', 'ldm', 
              'cdm', 'rdm', 'rwb', 'lb', 'lcb', 'cb', 'rcb', 'rb', 'gk')

manipulated_cols <- c('club_loaned_from')


summary(players_df$club_loaned_from)
sum(players_df$club_loaned_from == "")
sum(players_df$club_loaned_from != "")

players_clean <- players_df %>%
  select(-all_of(red_cols)) %>%
  mutate(
    club_joined_date = as.Date(club_joined_date),
    on_loan = ifelse(club_loaned_from == "", 0, 1),
    club_position = case_when(
      club_position == "RES" ~ 0,
      club_position == "SUB" ~ 1,
      is.na(club_position) ~ NA_real_,
      TRUE ~ 2
    ),
    fifa_version =  as.integer(fifa_version)
  ) %>%
  select(-all_of(manipulated_cols))

#' club_position has the issue that if players are injured or unavailable for some reason,
#' they won’t be in the lineup even though they are actually regular starters (e.g., De Bruyne).
#' In reality, a different scoring method should be used → e.g., the ratio of starting XI appearances
#' to games available (or > 60 minutes/game played) .


#' mapping function used to map the more granular positions to the categories
#' "GK" → "GK" 
#' "ST", "CF" → "ATT"
#' "RW", "LW", "RWB", "LWB" → "WING"
#' "CM", "CAM", "CDM", "LM", "RM" → "MID"
#' "CB", "RB", "LB" → "DEF"
#' We use the first position as we assume that to be the main position of the player
#' the reason wingers get an own category is because in some systems such as the one played by
#' Conte RWB/LWB are more attacking positions


map_position <- function(position) {
  if (is.na(position)) {
    return(NA)
  }
  first_position <- str_split(position, ", ")[[1]][1]  # Get the first position
  if (first_position == "GK") {
    return("GK")
  } else if (first_position %in% c("ST", "CF")) {
    return("ATT")
  } else if (first_position %in% c("RW", "LW", "RWB", "LWB")) {
    return("WING")
  } else if (first_position %in% c("CM", "CAM", "CDM", "LM", "RM")) {
    return("MID")
  } else if (first_position %in% c("CB", "RB", "LB")) {
    return("DEF")
  } else {
    return(NA)  # Default case for unmatched values
  }
}

# create key, position and captain
players_clean <- players_clean %>%
  mutate(position_category = sapply(player_positions, map_position),
         key = paste0(player_id, "@", fifa_version) # ACHTUNG: we create a unique key here
         ) %>%
  select(key, all_of(colnames(players_clean)[1:6]), position_category, everything())

# There seems to be some issues with the league names and ids
league_check <- players_clean %>%
  group_by(league_name) %>%
  summarise(unique_league_ids = n_distinct(league_id),
            unique_level_ids = n_distinct(league_level))%>%
  filter(unique_league_ids > 1 | unique_level_ids > 1) 
# we will have to apply the league levels manually 

unique(players_clean$league_name) # also there is one empty league name

#### teams df
colnames(teams_df)

red_teams_cols <- c(
  "fifa_update", "update_as_of", "coach_id", "home_stadium","captain",
  "short_free_kick", "long_free_kick", "left_short_free_kick", "right_short_free_kick", "penalties",
  "left_corner", "right_corner"
)

teams_clean <- teams_df %>%
  select(-all_of(red_teams_cols)) %>%
  filter(league_name != "Friendly International") %>%
  mutate(
    league_nationality_name = case_when(
      league_name == "Ligue 1" & nationality_name == "Monaco" ~ "France",
      league_name == "Ligue 2" & nationality_name == "Austria" ~ "France",
      league_name == "Championship" & nationality_name == "Wales" ~ "England",
      league_name == "League Two" & nationality_name == "Wales" ~ "England",
      league_name == "Premier Division" & nationality_name == "Northern Ireland" ~ "Republic of Ireland",
      league_name == "Major League Soccer" & nationality_name == "Canada" ~ "United States",
      league_name == "A-League" & nationality_name == "New Zealand" ~ "Australia",
      .default = nationality_name
    ),
    
    fifa_version =  as.integer(fifa_version),
    domestic_prestige =  as.integer(domestic_prestige),
    international_prestige =  as.integer(international_prestige),
    transfer_budget_eur =  as.numeric(transfer_budget_eur),
    club_worth_eur =  as.numeric(club_worth_eur)
    
  ) %>%
  select(-nationality_name)


##############################_
####### Missing value Handling -----

free_agent_cols <- c("league_name", "club_name")
check_names <- players_clean %>%
  select(key, where(~ !is.numeric(.))) %>%
  filter(if_any(where(~ !is.numeric(.)), ~ . == "")) %>%
  filter(!if_any(any_of(free_agent_cols), ~ . == ""))

#' checking the sofifa database (which is the source of the data) we see that
#' for players that are free agents in the FIFA game we get missing values for the 
#' team and league name. We will be filtering these players due to them often having 
#' having clubs irl that are not part of the game
#' Missing player values will be imputed


players_clean <- players_clean %>%
  filter(!if_any(any_of(free_agent_cols), ~ . == ""))


# it seems that the russian premier league has the same name as the english prem
check_PL <- players_clean %>%
  filter(league_name == "Premier League") %>%
  select(club_name) %>%                        
  distinct()

# Clubs without a league are added to the premier league for some reason,
og_prem <- c(check_PL$club_name[1:18], "Sheffield United", "Luton Town")
russian_prem <- c(check_PL$club_name[26:47])

# we get the nationalities from the leagues

# furthermore we will be dropping leagues with too little teams
check_size <- players_clean %>%
  select(league_name, club_name) %>%
  distinct() %>%
  group_by(league_name)%>%
  summarise(N = n()) %>%
  filter(N <= 8) 


players_clean <- players_clean %>%
  left_join(
    teams_clean %>%
      select(team_id, team_name, league_name, league_nationality_name) %>%
      distinct(team_id, team_name, league_name, .keep_all = TRUE),
    by = c("club_team_id" = "team_id", "league_name" = "league_name", "club_name" = "team_name")
  ) %>%
  #rename(league_nationality_name = nationality_name.y)%>%
  select(colnames(players_clean)[1:19], league_nationality_name, everything())%>%
  mutate(
    league_name = case_when(
      league_name == "Premier League"  & club_name %in% russian_prem ~ "Russian Premier League",
      league_name == "Premier League" & !(club_name %in% og_prem | club_name %in% russian_prem) ~ "filter",
      # rename the primera division leagues
      #league_name == "Primera División" ~ paste0(league_name, "_", substr(league_nationality_name, 1, 4)), # wir kombinieren fürs imputieren dann mit der Nationalität
      #league_name == "Primera Division" ~ paste0(league_name, "_", substr(league_nationality_name, 1, 4)),
      league_name %in% check_size$league_name ~ "filter",
      .default = league_name
    )
  ) %>%
  filter(league_name != "filter")
  


teams_clean <- teams_clean %>%
  mutate(
  league_name = case_when(
    league_name == "Premier League"  & team_name %in% russian_prem ~ "Russian Premier League",
    league_name == "Premier League" & !(team_name %in% og_prem | team_name %in% russian_prem) ~ "filter",
    league_name %in% check_size$league_name ~ "filter",
    .default = league_name
  )
) %>%
  filter(league_name != "filter")
  
  

check_duplicates <- players_clean %>%
  select(league_nationality_name, league_name, league_id) %>%
  distinct() %>%  # Ensure we work with unique combinations
  group_by(league_nationality_name, league_name) %>%
  summarise(num_ids = n_distinct(league_id), .groups = "drop") %>%
  filter(num_ids > 1)



check_duplicates_teams <- teams_clean %>%
  select(league_nationality_name, league_name, league_id) %>%
  distinct() %>%  # Ensure we work with unique combinations
  group_by(league_nationality_name, league_name) %>%
  summarise(num_ids = n_distinct(league_id), .groups = "drop") %>%
  filter(num_ids > 1)


# ids and league levels will be mapped and fixed
# manually map league_level and create new league_id
league_names <- players_clean %>%
  select(league_name, league_nationality_name) %>%
  distinct()

write.csv2(league_names, "dataset/league_names.csv")

league_names_edited <- read.csv2("dataset/league_names_edited.csv")

players_clean <- players_clean %>%
  select(-league_id, -league_level) %>%
  left_join(league_names_edited, by = c("league_name"="league_name", "league_nationality_name" = "league_nationality_name")) %>%
  select(colnames(.)[1:18], league_id, league_level, everything())
  


#league_check[is.na(league_check$league_id), ] 
#unique(teams_df$nationality_name)


check_empty_vals <- players_clean %>%
  select(key, where(~ !is.numeric(.))) %>%  # Select only non-numeric columns
  #filter(if_any(where(~ !is.numeric(.)), ~ . == ""))
  summarise(across(everything(), ~ sum(. == "", na.rm = TRUE), .names = "{col}")) %>%
  pivot_longer(2:ncol(.), names_to = "column", values_to = "values")

# impute empty value_eur 
players_clean$value_eur <- as.numeric(ifelse(players_clean$value_eur == "", NA, players_clean$value_eur))

# add age_range so that we can impute more easily
players_clean <- players_clean %>%
  mutate(age_group = case_when(
    age <= 18 ~ "18-",
    age >= 18 & age <= 20 ~ "18-20",
    age >= 21 & age <= 25 ~ "21-24",
    age >= 25 & age <= 29 ~ "25-29",
    age >= 30 ~ "30+",
    TRUE ~ NA_character_ 
  ),
  
  overall_range = case_when(
    overall <= 50 ~ "50-",
    overall >= 51 & overall <= 55 ~ "51-55",
    overall >= 56 & overall <= 60 ~ "56-60",
    overall >= 61 & overall <= 65 ~ "61-65",
    overall >= 66 & overall <= 70 ~ "66-70",
    overall >= 71 & overall <= 75 ~ "71-75",
    overall >= 76 & overall <= 80 ~ "76-80",
    overall >= 81 & overall <= 85 ~ "81-85",
    overall >= 86 & overall <= 90 ~ "86-90",
    overall > 90 ~ "90+",
    TRUE ~ NA_character_  # Handle missing or invalid overall values
  )
  
  
  )

# Impute missing values in value_eur
players_clean <- players_clean %>%
  group_by(position_category, age_group, overall_range, league_level) %>% 
  mutate(value_eur = ifelse(is.na(value_eur), mean(value_eur, na.rm = TRUE), value_eur)) %>%
  ungroup() %>%# Remove grouping
  mutate(value_eur = ifelse(is.na(value_eur), min(value_eur, na.rm = TRUE), value_eur)) #this only concerns one observation


#### teams df
na_summary_teams <- teams_clean %>%
  summarise(across(everything(), ~ sum(is.na(.)), .names = "{col}")) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "na_count") %>%
  mutate(
    na_fraction_perc = na_count / nrow(teams_clean) * 100
  ) %>%
  filter(na_count > 0)


na_summary_teams

rem_na <- na_summary_teams$column[na_summary_teams$na_fraction_perc >= 30] 

teams_clean <- teams_clean %>%
  select(-all_of(rem_na), -league_id, -league_level) %>% # too many NAs - RL relevance of these stats are also to be doubted
  left_join(league_names_edited, by = c("league_name"="league_name", "league_nationality_name" = "league_nationality_name")) %>%
  select(colnames(.)[1:8], league_nationality_name, league_id, league_level, everything())


check_names_teams <- teams_clean %>%
  select(where(~ !is.numeric(.))) %>%
  filter(if_any(everything(), ~ . == "")) %>%
  summarise(across(everything(), ~ sum(. == ""), .names = "{col}"), 
            summarise(across(everything(), ~ sum(is.na(.)), .names = "{col}")))  %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "miss_count") %>%
  mutate(
    miss_fraction_perc = miss_count / nrow(teams_clean) * 100
  ) %>%
  filter(miss_count > 0)

check_names_teams


rem_miss <- check_names_teams$column[check_names_teams$miss_fraction_perc >= 30]

teams_clean <- teams_clean %>%
  select(-all_of(rem_miss)) 


# prestige is a bit weird for fifa versions 15 and 16 -> we will impute the prestige for these two years using the fifa 17
fix_prestige_1516 <- teams_clean %>%
  select(team_id, team_name, league_name, fifa_version, domestic_prestige, international_prestige) %>%
  distinct() %>%
  filter(fifa_version <= 16)



fix_prestige <- teams_clean %>%
  select(team_id, team_name, league_name, fifa_version, domestic_prestige, international_prestige) %>%
  distinct() %>%
  filter(fifa_version > 16) %>%
  group_by(team_id, team_name) %>%
  summarise(
    domestic_prestige_fix = round(mean(domestic_prestige, na.rm = TRUE)),
    international_prestige_fix = round(mean(international_prestige, na.rm = TRUE))
  ) %>%
  ungroup()


mean(fix_prestige_1516$team_id %in% fix_prestige$team_id) 

team_checkpoint <- teams_clean
#teams_clean <- team_checkpoint

teams_clean <- teams_clean %>%
  left_join(
    fix_prestige %>% 
      select(team_id, international_prestige_fix, domestic_prestige_fix), 
    by = "team_id"
  ) %>%
  mutate(
    international_prestige = case_when(
      fifa_version <= 16 & !is.na(international_prestige_fix) ~ international_prestige_fix,  # Use fix value
      fifa_version > 16 ~ international_prestige,
      .default = NA,
    ),
    domestic_prestige = case_when(
      fifa_version <= 16 & !is.na(domestic_prestige_fix) ~ domestic_prestige_fix,  # Use fix value
      fifa_version > 16 ~ domestic_prestige_fix,
      .default = NA,
    ),
    
    overall_range = case_when(
      overall <= 50 ~ "50-",
      overall >= 51 & overall <= 55 ~ "51-55",
      overall >= 56 & overall <= 60 ~ "56-60",
      overall >= 61 & overall <= 65 ~ "61-65",
      overall >= 66 & overall <= 70 ~ "66-70",
      overall >= 71 & overall <= 75 ~ "71-75",
      overall >= 76 & overall <= 80 ~ "76-80",
      overall >= 81 & overall <= 85 ~ "81-85",
      overall >= 86 & overall <= 90 ~ "86-90",
      overall > 90 ~ "90+",
      TRUE ~ NA_character_  # Handle missing or invalid overall values
    )
    
  ) %>%
  select(-international_prestige_fix, -domestic_prestige_fix) %>%
  group_by(overall_range, league_id) %>%
  mutate(
    international_prestige = ifelse(is.na(international_prestige), round(mean(international_prestige, na.rm = TRUE)), international_prestige),
    domestic_prestige = ifelse(is.na(domestic_prestige), round(mean(domestic_prestige, na.rm = TRUE)), domestic_prestige)
  ) %>% 
  ungroup %>%
  mutate(
    international_prestige = ifelse(is.na(international_prestige), min(international_prestige, na.rm = TRUE), international_prestige),
    domestic_prestige = ifelse(is.na(domestic_prestige), min(domestic_prestige, na.rm = TRUE), domestic_prestige)
    ) #this concerns only two observations


summary(teams_clean$domestic_prestige)
summary(teams_clean$international_prestige)

team_checkpoint <- teams_clean
#teams_clean <- team_checkpoint

# Impute transfer_budget_eur and club_worth_eur
# TODO beschreibe hier die Cascade runter
teams_clean <- teams_clean %>%
  mutate(
  international_prestige_range = case_when(
    international_prestige < 3 ~ "2-",
    international_prestige >= 3 & international_prestige <= 4 ~ "3-4",
    international_prestige >= 5 & international_prestige <= 6 ~ "5-6",
    international_prestige >= 7 & international_prestige <= 8 ~ "7-8",
    international_prestige >= 9 & international_prestige <= 10 ~ "9-10",
    TRUE ~ NA_character_  # Handle missing or invalid overall values
  ),
  
  domestic_prestige_range = case_when(
    domestic_prestige < 3 ~ "2-",
    domestic_prestige >= 3 & domestic_prestige <= 4 ~ "3-4",
    domestic_prestige >= 5 & domestic_prestige <= 6 ~ "5-6",
    domestic_prestige >= 7 & domestic_prestige <= 8 ~ "7-8",
    domestic_prestige >= 9 & domestic_prestige <= 10 ~ "9-10",
    TRUE ~ NA_character_  # Handle missing or invalid overall values
  )
  ) %>%
  group_by(international_prestige_range, overall_range, league_level, domestic_prestige_range) %>%  # Group by relevant columns, including age_group
  mutate(
    transfer_budget_eur = ifelse(is.na(transfer_budget_eur), mean(transfer_budget_eur, na.rm = TRUE), transfer_budget_eur),
    club_worth_eur = ifelse(is.na(club_worth_eur), mean(club_worth_eur, na.rm = TRUE), club_worth_eur)
  ) %>% 
  ungroup %>%
  group_by(international_prestige_range, overall_range, league_level) %>%
  mutate(
    transfer_budget_eur = ifelse(is.na(transfer_budget_eur), mean(transfer_budget_eur, na.rm = TRUE), transfer_budget_eur),
    club_worth_eur = ifelse(is.na(club_worth_eur), mean(club_worth_eur, na.rm = TRUE), club_worth_eur)
  ) %>%
  ungroup() %>%
  group_by(domestic_prestige_range, league_level) %>%
  mutate(
    transfer_budget_eur = ifelse(is.na(transfer_budget_eur), mean(transfer_budget_eur, na.rm = TRUE), transfer_budget_eur),
    club_worth_eur = ifelse(is.na(club_worth_eur), mean(club_worth_eur, na.rm = TRUE), club_worth_eur)
  ) %>%
  ungroup()



overview <- teams_clean %>% 
  filter(is.na(transfer_budget_eur) | is.na(club_worth_eur))



#++ Checkpoint
players_clean_checkpoint <- players_clean
#players_clean <- players_clean_checkpoint

##############################_
####### NA Handling ----

na_summary <- players_clean %>%
  summarise(across(everything(), ~ sum(is.na(.)), .names = "{col}")) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "na_count") %>%
  mutate(
    na_fraction_perc = na_count / nrow(players_clean) * 100
  ) %>%
  filter(na_count > 0)

na_summary


# players on loan do not have a club_joined_date
na_check_club_joined <- players_clean %>%
  select(key, long_name, club_name, club_joined_date, on_loan) %>%
  filter(is.na(club_joined_date))


# simplified assumption, player joined on loan in the last summer transfer window -> we will make
# the date 01.08 + year of fifa version - 1 (fifa is released in sept of the year before the versions year)
players_clean$club_joined_date <- as.Date(ifelse(
  is.na(players_clean$club_joined_date), 
  paste0("20", (players_clean$fifa_version) - 1, "-08-01"), 
  as.character(players_clean$club_joined_date)  # Convert to character for compatibility
))


# goalkeeping_speed na für alle nicht GKs -  eine Ausnahme, diese werden wir ausschließen
# all missing values except for mentality_compuser are GK
players_clean <- players_clean %>%
  mutate(
    goalkeeping_speed = ifelse((position_category != "GK" & is.na(goalkeeping_speed)), 0, goalkeeping_speed),
    goalkeeping_speed = round(goalkeeping_speed),
    pace = ifelse((position_category == "GK" & is.na(pace)), 0, pace),
    shooting = ifelse((position_category == "GK" & is.na(shooting)), 0, shooting),
    passing = ifelse((position_category == "GK" & is.na(passing)), 0, passing),
    dribbling = ifelse((position_category == "GK" & is.na(dribbling)), 0, dribbling),
    defending = ifelse((position_category == "GK" & is.na(defending)), 0, defending),
    physic = ifelse((position_category == "GK" & is.na(physic)), 0, physic)
  ) %>%
  filter(!(position_category == "GK" & is.na(goalkeeping_speed)))


na_summary <- players_clean %>%
  summarise(across(everything(), ~ sum(is.na(.)), .names = "{col}")) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "na_count") %>%
  mutate(
    na_fraction_perc = na_count / nrow(players_clean) * 100
  ) %>%
  filter(na_count > 0)

na_summary



# Impute mentality_composure NAs the way we did for value_eur
players_clean <- players_clean %>%
  group_by(position_category, age_group, overall_range) %>%  # Group by relevant columns, including age_group
  mutate(
    mentality_composure = ifelse(is.na(mentality_composure), mean(mentality_composure, na.rm = TRUE), mentality_composure),
    mentality_composure = round(mentality_composure)
    ) %>%
  ungroup() %>% # Remove grouping
  mutate(mentality_composure = ifelse(is.na(mentality_composure), min(mentality_composure, na.rm = TRUE), mentality_composure)) #this concerns only two observations



##############################_
####### League-Level Data ----

league_clean <- teams_clean %>%
  select(-rival_team, -domestic_prestige) %>%
  group_by(fifa_version, league_id, league_name, league_nationality_name, league_level) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE), .names = "{col}")
  ) %>%
  ungroup() %>%
  mutate(
    overall = round(overall),
    attack = round(attack),
    midfield = round(midfield),
    defence = round(defence),
    league_url = paste0("/league/", league_id, "/", league_name, "/", fifa_version, "002" ) # ACHTUNG this is imaginary
  ) %>%
  select(league_id, fifa_version, league_url, league_name, everything()) %>%
  mutate(
    norm_international_prestige = 1 + 9 * (international_prestige - min(international_prestige, na.rm = TRUE)) /
      (max(international_prestige, na.rm = TRUE) - min(international_prestige, na.rm = TRUE)),
    
    norm_transfer_budget = 1 + 9 * (transfer_budget_eur - min(transfer_budget_eur, na.rm = TRUE)) /
      (max(transfer_budget_eur, na.rm = TRUE) - min(transfer_budget_eur, na.rm = TRUE)),
    
    norm_club_worth = 1 + 9 * (club_worth_eur - min(club_worth_eur, na.rm = TRUE)) /
      (max(club_worth_eur, na.rm = TRUE) - min(club_worth_eur, na.rm = TRUE)),
    
    international_prestige = # we calculate a weighted prestige score from the normed transfer budget, club worth and average international prestige
      0.3 * norm_international_prestige +
      0.3 * norm_transfer_budget +
      0.6 * norm_club_worth,
    # summiert sich auf >1 auf ABER durchs runden geht das gut
    
    international_prestige = round(international_prestige)
    
  ) %>%
  select(-norm_international_prestige, -norm_transfer_budget, -norm_club_worth)




##############################_
####### Save Datasets ----


players_clean <- players_clean %>%
  mutate(
    key = paste0(player_id, "_", fifa_version),
    team_key = paste0(club_team_id, "_", fifa_version),
    league_key = paste0(league_id, "_", fifa_version),
    preferred_foot = ifelse(preferred_foot == "Right", 1, 0)
  ) %>%
  select(key,  colnames(players_clean)[1:15], team_key, colnames(players_clean)[17:19], league_key,everything())


teams_clean <- teams_clean %>%
  mutate(
    team_key = paste0(team_id, "_", fifa_version),
    league_key = paste0(league_id, "_", fifa_version),
    rival_key = paste0(rival_team, "_", fifa_version)
  ) %>%
  select(team_key, everything())

league_clean <- league_clean %>%
  mutate(
    league_key = paste0(league_id, "_", fifa_version)
  ) %>%
  select(league_key, everything())


library(arrow)
# write_parquet(players_clean, "dataset/players_clean.parquet")
# write_parquet(teams_clean, "dataset/teams_clean.parquet")
# write_parquet(league_clean,  "dataset/league_clean.parquet")


write.csv2(players_clean, file = "dataset/players_clean.csv", row.names = FALSE)
write.csv2(teams_clean, file = "dataset/teams_clean.csv", row.names = FALSE)
write.csv2(league_clean, file = "dataset/league_clean.csv", row.names = FALSE)


rm(list = ls())
getwd()
setwd("C:/mahmoud uni/TU/SS2024/KGs/Portfolio")

library(tidyverse)
library(stringr)
library(arrow)


players_clean <- read.csv2(file = "dataset/players_clean.csv")
teams_clean <- read.csv2(file = "dataset/teams_clean.csv")
league_clean <- read.csv2(file = "dataset/league_clean.csv")

#glimpse(players_clean)

####### Subsampling for Graph ----

nrow(players_clean %>% filter(fifa_version == 24))
unique(players_clean$fifa_version)
small_fifa <- c(21, 22, 23, 24)

select_player_prep <- players_clean %>%
  select(key, player_id, long_name, fifa_version) %>%
  filter(fifa_version %in% small_fifa) %>%
  group_by(player_id) %>%
  summarize(
    N = n()
  ) %>%
  filter(N == max(N))

select_player <- players_clean %>%
  select(key, player_id, long_name, fifa_version, overall_range, age_group, position_category) %>%
  filter(fifa_version %in% small_fifa) %>%
  filter(player_id %in% select_player_prep$player_id)


##### stratified sampling
set.seed(1120) 


# früher initial_sample
sampled_players_ids <- select_player %>%
  group_by(overall_range, age_group, position_category) %>%
  slice_sample(n = 20) %>% # ensures that each combi is selcted 20 times
  ungroup()

# remaining_rows <- 2000 - nrow(initial_sample)
# 
# additional_sample <- select_player %>%
#   anti_join(initial_sample, by = c("key", "player_id")) %>%  # Exclude already selected rows
#   group_by(overall_range, age_group) %>%
#   sample_frac(size = remaining_rows / nrow(select_player)) %>% 
#   ungroup()

#sampled_players_ids <- bind_rows(initial_sample, additional_sample)

players_small <- players_clean %>%
  filter(fifa_version %in% small_fifa) %>%
  filter(player_id %in% sampled_players_ids$player_id)

length(unique(players_small$player_id))

teams_small <- teams_clean %>% 
  filter(team_key %in% unique(players_small$team_key))


league_small <- league_clean %>%
  filter(league_key %in% unique(teams_small$league_key)) 


dim(players_small)
dim(teams_small)
dim(league_small)


write_parquet(players_small, "dataset/players_small.parquet")
write_parquet(teams_small, "dataset/teams_small.parquet")
write_parquet(league_small,  "dataset/league_small.parquet")

# for manual checkup
write.csv2(sampled_players_ids %>% select(key), file = "dataset/selected_players_key.csv", row.names = FALSE)
