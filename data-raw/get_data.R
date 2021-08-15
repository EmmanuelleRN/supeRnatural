# Load packages ---------------------------------------------------------------

# Utility packages
#library(here)
library(janitor)
library(usethis)
library(lubridate)
library(stringr)
library(progress)
library(readr)
library(data.table)

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

awards_page <- "https://en.wikipedia.org/wiki/List_of_awards_and_nominations_received_by_Supernatural"
awards_page <- read_html(awards_page)
awards_tbl <- html_nodes(awards_page, "table")

characters_page <- "https://en.wikipedia.org/wiki/List_of_Supernatural_characters"
characters_page <- read_html(characters_page)
characters_tbl <- html_nodes(characters_page, "table")

#### TO DO ####
species_page <- "https://supernatural.fandom.com/wiki/Category:Species"
species_page <- read_html(species_page)
species_list <- html_nodes(species_page, '.category-page__member-link')
#### To DO ####

# Extract synopsis
synopsis <- html_table(tbls[[1]], fill = TRUE) # Synopsis

# Extract list of episodes
list_of_episodes <- lapply(1:15,
                           function(x) html_table(tbls[[x + 1]], fill = TRUE) %>%
                             mutate(season = x)) %>%
  data.table::rbindlist(.)

# Extract awards
awards <- lapply(3:17, function(x){
  html_table(awards_tbl[[x]], fill = TRUE) %>%
    mutate(award_name = case_when(
      x == 3 ~ "Constellation Awards",
      x == 4 ~ "EWwy Awards",
      x == 5 ~ "Fangoria Chainsaw Awards",
      x == 6 ~ "GLAAD Media Awards",
      x == 7 ~ "Golden Reel Awards",
      x == 8 ~ "Leo Awards",
      x == 9 ~ "People's Choice Awards",
      x == 10 ~ "Primetime Emmy Awards",
      x == 11 ~ "Rondo Hatton Classic Horror Awards",
      x == 12 ~ "Saturn Awards",
      x == 13 ~ "SFX Awards",
      x == 14 ~ "Teen Choice Awards",
      x == 15 ~ "TV Guide Awards",
      x == 16 ~ "Young Artist Awards",
      x == 17 ~ "Hugo Awards"
    ))
}) %>%
  data.table::rbindlist(.)

# Extract characters list
characters_list <- lapply(2:4, function(x){
  html_table(characters_tbl[[x]], fill = TRUE)[1:17]
}) %>%
  data.table::rbindlist(.)

# Clean tables -------------------------------------------------------------

# Steps
# - Rename and clean column names
# - Remove random rows
# - Remove square brackets from all data
# - Replace TBA with NA
# - Process release date into dates
# - Remove duplicated and extra columns

# Clean synopsis
synopsis <- synopsis %>%

  # Clean column names
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
list_of_episodes <- list_of_episodes %>%

  # Clean column names
  janitor::clean_names(abbreviations = "U.S.") %>%

  # Rename u.s. to us_
  rename(us_viewers_millions = u.s.viewers_millions) %>%

  # Remove quotes from Title
  mutate(title = stringr::str_replace_all(title, '\"', "")) %>%

  # the episode can be written in its entirety by the same person or by a group of people that writes
  # the story and the show
  mutate(written_by = gsub("([a-z])([A-Z])", "\\1 \\2", written_by)) %>%
  tidyr::separate(col = written_by, into = c("written_by", "story_by"), sep = "Story by") %>%
  tidyr::separate(col = story_by, into = c("story_by", "teleplay_by"), sep = "Teleplay by") %>%

  # Remove "  : "
  mutate(story_by = stringr::str_sub(story_by, start = 4),
         teleplay_by = stringr::str_sub(teleplay_by, start = 4)) %>%

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
  }) %>%

  # select desired columns
  select(season, no_overall, no_inseason, title, directed_by, written_by, story_by, teleplay_by,
         original_air_date, u_s_viewers_millions)

# Clean awards
awards <- awards %>%
  # Clean column names
  janitor::clean_names()

# Clean characters list
characters_list <- characters_list %>%

  # Clean column names
  janitor::clean_names() %>%

  # Each information in a row
  tidyr::pivot_longer(-c(actor, character), names_to = 'season', values_to = 'role') %>%

  # Remove rows that have repetitive information with the title
  filter(actor != 'Actor') %>%

  # clean the season string to have just the season number
  dplyr::mutate(season = ifelse(grepl("\\d", season), stringr::str_sub(season, 9, -1), 1)) %>%
  # remove special * character
  dplyr::mutate_at(.vars = c('actor', 'character'),
                              .funs = gsub,
                              pattern = "\\*{1,}",
                              replacement = "\\1")
