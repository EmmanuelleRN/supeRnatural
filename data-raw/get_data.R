# Load packages ---------------------------------------------------------------

# Utility packages
library(here)
library(janitor)
library(usethis)
library(lubridate)
library(stringr)
library(progress)
library(readr)

# Data wrangling packages
library(dplyr)
library(tidyr)

# Web scraping
library(rvest)
library(httr)
library(xml2)

# Extract data ----------------------------------------------------------------

# Extract data

spn_page <- "https://en.wikipedia.org/wiki/List_of_Supernatural_episodes"
read_page <- read_html(spn_page)
tbls <- html_nodes(read_page, "table")

# Extract synopsis
synopsis <- html_table(tbls[[1]], fill = TRUE) # Synopsis

# Extract list of episodes per season
# list_of_episodes <- for(season in 1:15){
#   season_tbl <- paste0("list_episodes_season_", season)
#   assign(season_tbl, html_table(tbls[[season + 1]], fill = TRUE))
# }

list_of_episodes <- lapply(1:15,
                           function(x) html_table(tbls[[x + 1]], fill = TRUE) %>%
                             mutate(season = x + 1)) %>%
  do.call(rbind, .)

# Clean films -------------------------------------------------------------

# Steps
# - Rename and clean column names
# - Remove random rows
# - Remove square brackets from all data
# - Replace TBA with NA
# - Process release date into dates

synopsis <- synopsis %>%
  janitor::clean_names() %>%
  # Remove first line as it is still part of the header
  slice(-1) %>%

  # Drop first columns as it represents the colours of the season (wikipedia colour coding)
  select(-c(season)) %>%

  # Fixing header as some columns have more than one cell
  rename("season" = season_2,
         "first_aired" = originally_aired,
         "last_aired" = originally_aired_2,
         "network" = originally_aired_3) %>%

  # Drop duplicated columns
  select(-episodes_2) %>%

  # Fixing network column
  mutate(network = c("The WB", rep("The CW", 14))) %>%

  # Remove citations for data because unneeded for our data
  mutate_all(function(x) {
    stringr::str_replace_all(x, "\\[[A-Za-z0-9 ]{1,}\\]", "")
  }) %>%

  # Replace TBA with NA for now
  mutate_all(function(x) {
    ifelse(x == "TBA", NA, x)
  }) %>%

# Clean up and format release dates
  mutate_at(vars(ends_with("aired")), function(x) {
    stringr::str_extract(x, "\\((.*?)\\)") %>%
      stringr::str_replace_all("\\(|\\)", "") %>%
      lubridate::ymd()
  })

# Clean list of episodes

list_of_episodes %>%
  janitor::clean_names() %>%
  # Remove quotes from Title
  mutate(title = stringr::str_replace_all(title, '\"', "")) %>%

  mutate(written_by = gsub("([a-z])([A-Z])", "\\1 \\2", written_by)) %>%
  tidyr::separate(col = written_by, into = c("written_by", "story_by"), sep = "Story by") %>%
  tidyr::separate(col = story_by, into = c("written_by", "teleplay_by"), sep = "Teleplay by") %>%

  # Remove citations for data because unneeded for our data
  mutate_all(function(x) {
    stringr::str_replace_all(x, "\\[[A-Za-z0-9 ]{1,}\\]", "")
  }) %>%

  # Replace TBA with NA for now
  mutate_all(function(x) {
    ifelse(x == "TBA", NA, x)
  }) %>%

  # Clean up and format air date
  mutate_at(vars(ends_with("air_date")), function(x) {
    stringr::str_extract(x, "\\((.*?)\\)") %>%
      stringr::str_replace_all("\\(|\\)", "") %>%
      lubridate::ymd()
  }) %>% head()
