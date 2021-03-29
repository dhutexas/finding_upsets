# obtaining and cleaning the data
library(tidyverse)
library(magrittr)
library(collegehoops)

######################################################
# Custom Functions to Calculate Summary Stats by Row #
######################################################

# function to calculate row standard deviation
RowSD <- function(x) {
  df = dim(x)[2] - 1
  return(sqrt(rowSums((x - rowMeans(x))^2)/(df)))
}

# functions to calculate row confidence intervals
RowCI_Lower <- function(x) {
  df = dim(x)[2] - 1
  n = dim(x)[2]
  mean = rowMeans(x)
  sd = sqrt(rowSums((x - rowMeans(x))^2)/(df))
  return(mean - (qt(0.975, df)*sd/sqrt(n)))
}

RowCI_Upper <- function(x) {
  df = dim(x)[2] - 1
  n = dim(x)[2]
  mean = rowMeans(x)
  sd = sqrt(rowSums((x - rowMeans(x))^2)/(df))
  return(mean + (qt(0.975, df)*sd/sqrt(n)))
}

########################################################################
# Gather, tidy, and manipulate data on each of the 68 tournament teams #
########################################################################

# tournament seed data
seeds = read.csv('Data/MNCAATourneySeeds.csv') %>%
  filter(Season == 2021) %>%
  select(-Season)

# bring in crosswalk to match team names across datasets
crosswalk = read.csv('Data/MTeams_crosswalk.csv')

# ken pomeroy data
kenpom = collegehoops::get_kenpom_data(2021) %>%
  select(team, adj_o, adj_d) %>%
  left_join(crosswalk, by = c('team' = 'team_kenpom')) %>%
  select(team_kaggle, id_kaggle, adj_o, adj_d)

# ratings on day before tournament from FiveThirtyEight - 3/18/21
elo = read.csv('Data/fivethirtyeight_elo.csv') %>%
  filter(forecast_date == '3/18/21') %>%
  select(team_name, team_rating, team_id, team_seed, team_region) %>%
  left_join(crosswalk, by = c('team_name' = 'team_538')) %>%
  select(team_kaggle, id_kaggle, team_rating, team_seed, team_region)

# read in most recent Massey ratings
massey_rat = read.csv('Data/masseyratings_21.csv') %>%
  left_join(crosswalk, by = c('team' = 'team_massey')) %>%
  select(team_kaggle, id_kaggle, rat:el)

# join the datasets for just tournament teams (note: elo only has tourney teams)
data = elo %>%
  left_join(seeds, by = c('id_kaggle' = 'TeamID')) %>%
  left_join(kenpom, by = c('team_kaggle', 'id_kaggle')) %>%
  left_join(massey_rat, by = c('team_kaggle', 'id_kaggle'))

rm(crosswalk, elo, kenpom, massey_rat, seeds)

# join slots/rounds to data
data %<>%
  janitor::clean_names() %>%
  rename(seed_int = team_seed,
         elo = team_rating,
         kenpom_adj_o = adj_o,
         kenpom_adj_d = adj_d,
         massey_rat = rat,
         massey_pwr = pwr,
         massey_off = off,
         massey_def = def) %>%
  mutate(team = as.character(team_kaggle)) %>%
  select(team, id_kaggle, seed, seed_int, team_region,
         elo, kenpom_adj_o, kenpom_adj_d,
         massey_rat, massey_pwr, massey_off, massey_def)

# create dataframe with scaled data (adjusted for whether higher/lower is 'better')
scaled_ratings = data %>%
  mutate(elo = scale(elo),
         kenpom_adj_o = scale(kenpom_adj_o),
         kenpom_adj_d = scale(kenpom_adj_d) * -1, # lower is better
         massey_rat = scale(massey_rat),
         massey_pwr = scale(massey_pwr),
         massey_off = scale(massey_off),
         massey_def = scale(massey_def)) %>%
  mutate(avg_score = rowMeans(select(., elo, kenpom_adj_o, kenpom_adj_d, massey_rat, massey_pwr, massey_off, massey_def)),
         stdv_score = RowSD(select(., elo, kenpom_adj_o, kenpom_adj_d, massey_rat, massey_pwr, massey_off, massey_def)),
         lower_ci = RowCI_Lower(select(., elo, kenpom_adj_o, kenpom_adj_d, massey_rat, massey_pwr, massey_off, massey_def)),
         upper_ci = RowCI_Upper(select(., elo, kenpom_adj_o, kenpom_adj_d, massey_rat, massey_pwr, massey_off, massey_def)))

# bring in game matchups and pair with scaled data
matchups = read.csv('Data/bracket.csv') %>%
  mutate(spot = paste0(round_slot, slot)) %>%
  select(spot, team1_name, team2_name) %>%
  pivot_longer(2:3, names_to = 'seeds', values_to = 'team') %>%
  select(-seeds) %>%
  mutate(slot = spot) %>%
  separate(spot, into = c('round', 'location'), 2) %>%
  left_join(scaled_ratings, by = 'team') %>%
  rename(`Rating: ELO` = elo,
         `Rating: Massey Power` = massey_pwr,
         `Rating: Massey` = massey_rat,
         `Offense: KenPom` = kenpom_adj_o,
         `Offense: Massey` = massey_off,
         `Defense: KenPom` = kenpom_adj_d,
         `Defense: Massey` = massey_def) %>%
  mutate(seed_clean = str_extract(seed_int,'\\d+'),
         seed_team = paste0("(",seed_clean,") ",team))
