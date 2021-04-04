# Load packages ---------------------------------------------------------------

# Utility packages
library(here)
library(janitor)
library(usethis)
library(lubridate)
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
for(season in 1:15){
  season_tbl <- paste0("list_episodes_season_", season)
  assign(season_tbl, html_table(tbls[[season + 1]], fill = TRUE))
}

# Clean films -------------------------------------------------------------

# Steps
# - Rename and clean column names
# - Remove random rows
# - Remove square brackets from all data
# - Replace TBA with NA
# - Process release date into dates

synopsis %>%
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
  mutate(network = c("The WB", ))

