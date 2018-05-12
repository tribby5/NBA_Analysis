library(dplyr)

# Matthew Tribby
# Date Created: 5/11/18

standings <- extractData(1990:2018)

standings <- standings %>% 
  mutate(PD = as.numeric(PS_G) - as.numeric(PA_G),
         GP = as.numeric(wins) + as.numeric(losses)) %>% 
  select(team, GP, wins, losses, WL_Percent, PD, year)

#check if data is normal to qualify for linear regression
boxplot(as.numeric(standings$WL_Percent), main = "wins")
boxplot(as.numeric(standings$PD), main = "Point Differential")
#All looks good as you would expect. 

wins.lm <- lm(WL_Percent ~ PD, data = standings)
summary(wins.lm)
#Both intercept and PD are stat sig as expected 
#Intercept is .5 (50%)(make logical sense)
#.0325 estimate coefficient for Point Diff passes the eye test

#Let's look at which 
standings$res <- resid(wins.lm)
plot(standings$PD , standings$res, 
      ylab="Residuals", xlab="Point Diff", 
      main="Point Diff Residuals") 
abline(0,0)

edgeVals <- quantile(standings$res, probs = c(.1, .9))

#Represents the extreme cases
extremes <- standings %>% filter(res < edgeVals[1] | res > edgeVals[2]) %>%
  mutate(exp_win_diff = -res*GP,
         exp_wins = as.numeric(wins) + exp_win_diff)

