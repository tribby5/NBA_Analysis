library(dplyr)
library(rvest)

# author: Matthew Tribby
# date: 5/12/18

# The goal of this script is to scrape game data for this project
# The main function is extractGames_Range which is the only function needed to scrape (others are helpers)
# This will deliver games data (date, visitor, home, points by both teams) for the regular season

# Input: range of years (i.e. 1991:2018) 
# Output: data.frame of all teh game data for all regular season games for that range of years
extractGames_Range <- function(range){
  return(
    range %>%
      map(extractGames_Year) %>%
      bind_rows()
  )
}

# Input: year to get all the game data from 
# Output: game data for all regular season games of that year
extractGames_Year <- function(year){
  return(
    c("october", "november", "december", "january", "february", "march", "april") %>%
      map(extractGames_Month, year = year) %>% 
      bind_rows()
  )
}

# Input: month, year of games to be scraped
# Output: the game data for that month
extractGames_Month <- function(month, year){
  monthData <- read_html(paste0("https://www.basketball-reference.com/leagues/NBA_",year,"_games-",month,".html")) %>% 
      html_nodes("table") %>% html_nodes("tbody") %>% 
      html_nodes("tr") %>% 
      map(extractRow) %>%
      bind_rows()
  if(month == "april"){
    return(monthData %>% slice(1:which(monthData$date == "Playoffs")-1))
  }
  return(monthData)
}
  
#Input: html for a row to be transformed into a tibble
#Output: a tibble containing all the wanting data in the correct format, no longer html
extractGames_Row <- function(row){
  date <- row %>% html_nodes(xpath = "./th") %>% 
    html_text()
  rowVec <- row %>% 
    html_nodes(xpath = "./td") %>% 
    html_text() 
  return(
    tibble(date = date, visitor_team = rowVec[2], visitor_score = rowVec[3], home_team = rowVec[4], 
           home_score = rowVec[5]) 
  )
}
  