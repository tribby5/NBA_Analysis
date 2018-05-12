library(dplyr)
library(rvest)
library(purrr)
library(stringr)

#Author: Matthew Tribby
#Date Created: 5/11/18

# The goal of this script is to gather the standings data for this project
# This data is all non-derived stats but just basic NBA standings data
# Organization:
  # 1. extractStandings_Range: main function which is the only function needed to get the standings data
  # 2. extractStandings_Year: extracts one year's worth of standings data
  # 3. extractStandings_Row: extracts one team's standings data for one year (this function depends on getting the correct html)

# This is the main function for scraping the data
#Input: the range of years to scrape from (i.e '1990:2018')
#Output: Data frame with standings for all NBA teams for all years
extractStandings_Range <- function(range){
  return(
    range %>%
      map(extractStandings_Year) %>%
      bind_rows()
  )
}

#This function builds the URL based on the year, easy to traverse the different pages
#Example use is in the function below, extractYearData
urlStandings <- function(year){return(
  paste0("https://www.basketball-reference.com/leagues/NBA_",  year ,"_standings.html")
)}

#This function scrapes data for one year from one page
#Output: All NBA teams and their standings data along with the year for easy id later
extractStandings_Year <- function(year){
  return(
    read_html(urlStandings(year)) %>%
      html_nodes(".suppress_all.sortable.stats_table") %>%
      .[1:2] %>% 
      html_nodes(".full_table") %>% 
      map(extractStandings_Row) %>%
      bind_rows() %>%
      mutate(year = year)
  )
}

#This function extracts one row (one's team data) from the table and assembles
#the row into a tibble which is one row of that year's data
extractStandings_Row <- function(html){
  teamName <- html %>%
    html_nodes(xpath = "./th/a") %>%
    html_text()
  teamInfoVec <- html %>%
    html_nodes(xpath = "./td") %>%
    html_text()
  return(
    tibble(team = teamName, wins = teamInfoVec[1], losses = teamInfoVec[2],
           WL_Percent = teamInfoVec[3], PS_G = teamInfoVec[5], PA_G = teamInfoVec[6])
  )
}


