library(dplyr)

# Matthew Tribby
# Date: 5/12/18

#This function returns a data.frame of a team's game for a given year with an extra combo of that team's PD for the game
getGamesPD <- function(team, year){
  return(
    data.frame(extractGames_Year(year))  %>%
      getGamesPD_games(team)
  )
}

getGamesPD_games <- function(games, team){
  return(
    games %>% 
      filter(visitor_team == team | home_team == team) %>%
      rowwise() %>%
      mutate(PD = ifelse(home_team == team, as.numeric(home_score) - as.numeric(visitor_score), 
                         as.numeric(visitor_score) - as.numeric(home_score)),
             outcome = ifelse(PD > 0, "W", "L"))
  )
}

threeOrLessGames <- function(year){
  ThreeOrLess <- data.frame(extractGames_Year(year)) %>%
   filter(abs(as.numeric(home_score)-as.numeric(visitor_score))<=3) 
  
  return(
    data.frame(team_ = union(unique(ThreeOrLess$home_team), unique(ThreeOrLess$visitor_team))) %>%
      rowwise() %>%
      mutate(W = length(ThreeOrLess %>% filter(home_team == team_ | visitor_team == team_) %>% getGamesPD_games(team = team_) %>% pull(outcome) %>% .[. == "W"]),
             L = nrow(ThreeOrLess %>% filter(home_team == team_ | visitor_team == team_)) - W, 
             WL_Percentage = W/(W+L) 
      )
  )
}

fiveOrLessGames <- function(year){
  FiveOrLess <- data.frame(extractGames_Year(year)) %>%
    filter(abs(as.numeric(home_score)-as.numeric(visitor_score))<=5) 
  
  return(
    data.frame(team_ = union(unique(FiveOrLess$home_team), unique(FiveOrLess$visitor_team))) %>%
      rowwise() %>%
      mutate(W = length(FiveOrLess %>% filter(home_team == team_ | visitor_team == team_) %>% getGamesPD_games(team = team_) %>% pull(outcome) %>% .[. == "W"]),
             L = nrow(FiveOrLess %>% filter(home_team == team_ | visitor_team == team_)) - W, 
             WL_Percentage = W/(W+L) 
      )
  )
}




