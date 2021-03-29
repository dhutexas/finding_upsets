###########################
# Get Massey Ratings Data #
###########################

# HTML tables on masseyratings.com are dynamically loaded - 
# cannot be parsed with rvset/beautifulsoup alone
# A browser emulator, like Selenium, can request the site to obtain html tables
# Here, a Docker image of Selenium is used to run the program
# To directly copy this code, you'll need to run your own instance of Docker and
# include your own relevant server address

library(tidyverse)
library(magrittr)
library(janitor)
library(httr)
library(Rcpp)
library(magrittr)
library(rvest)
library(RSelenium)

remDr <- remoteDriver(
  remoteServerAddr = '192.168.86.27',
  port = 4445L,
  browserName = "chrome"
)
remDr$open()

# current season
url_data <- "https://masseyratings.com/cb/ncaa-d1/ratings"
remDr$navigate(url_data)
html <- remDr$getPageSource()[[1]]

# Extract table from source
DF <- read_html(html) %>% 
  html_nodes("table") %>% 
  `[[`(2) %>% 
  html_table()

# Close connection
remDr$close()

# remove extraneous data (conferences, rankings, etc.) and tidy
massey_ratings = DF %>%
  select(-c(2,3,13)) %>% 
  slice(-1) %>%
  janitor::clean_names() %>%
  mutate(team = str_replace(team,
                            paste0('West Coast|Big 12|Big 10|Pac 12|',
                            'American Athletic|Southeastern|Missouri Val|',
                            'Big East|Atlantic Coast|Conference USA|',
                            'Patriot League|Summit Lg|OH Valley|Big West|',
                            'Mid-American|Mountain West|Atlantic 10|Southland|',
                            'Big Sky|Western Athletic|Big South|Horizon|',
                            'Atlantic Sun|Sun Belt|Colonial|Metro Atlantic|',
                            'America East|Southwestern AC|Mid-Eastern AC'),'')) %>%
  # remove extra data in front of statistics (removing the rankings to get ratings only)
  mutate(rat = as.numeric(str_sub(rat, -4, -1)), # rating never goes above 10
         pwr = as.numeric(str_sub(pwr, -5, -1)),
         so_s = as.numeric(str_sub(so_s, -5, -1)),
         ssf = as.numeric(str_sub(ssf, -5, -1)),
         off = as.numeric(str_sub(off, -6, -1)),
         def = as.numeric(str_sub(def, -5, -1)),
         # remove rank in front for those ranked > 1-9 (removes first character)
         off = ifelse(off > 130, gsub('^.', '', off), off), 
         def = ifelse(def > 35, gsub('^.', '', def), def),
         pwr = ifelse(pwr > 60, gsub('^.', '', pwr), pwr),
         # remove conference that is also in real name
         team = gsub('Southern$', '', team),
         team = gsub('Northeast$', '', team)) %>%
  mutate(across(c(pwr, off, def, hfa, ew, el), ~as.numeric(.))) %>%
  drop_na()

# write to csv file in Data folder
write.csv(massey_ratings, 'Data/masseyratings_21.csv', row.names = FALSE)
