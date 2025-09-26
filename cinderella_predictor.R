#clear enviornment and load in packages
rm(list = ls())  
library(tidyverse)
library(readxl)
library(modelr)
library(plotly) 
library(dplyr)
#load in data
ncaa_results <- read_csv("ncaa_results.csv")
ncaa_reg<-read_csv("ncaa_reg.csv")
#general formula for pythag
ncaa_reg<-
  mutate(ncaa_reg, pythag=(TM-OPP)/(TM+OPP))
#fit to ncaa men's basketball regular season
pythag_fit <- lm(data = ncaa_reg, TW ~ pythag)
pythag_fit
#intercept=18.68, slope=97.28
rsquare(pythag_fit, ncaa_reg)
summarize(ncaa_reg, correlation=cor(pythag, TW))
#r^2=.4752, r=.689
#chart comparing pythag and total wins with line of best fit
ncaa_reg<-mutate(ncaa_reg, pythag_predicted_wins=pythag*97.28+18.68)
graph_pythag_TW<-
  ggplot(data = ncaa_reg)+
  geom_point(mapping=aes(x=pythag_predicted_wins, y=TW), col="dodgerblue")+
  geom_abline(intercept = 0 , slope = 1, col="red")+
  labs(x="Pythag Predicted Wins",
       y="Total Wins",
       title="Pythag vs Total Wins",
       subtitle="RMSE=2.4, r=.726")+
  theme_bw()
graph_pythag_TW
summarize(ncaa_reg, 
          rmse = sqrt(mean((TW - pythag_predicted_wins)^2)))
cor(ncaa_reg$TW, ncaa_reg$pythag_predicted_wins)
#format to get year of a game in ncaa_results
ncaa_results <- ncaa_results %>%
  mutate(Date2 =as.Date(Date, "%m/%d/%Y"))%>%
  mutate(year=substr(Date2, 3, 4))
#rename so vars can be used wo error
ncaa_results<- rename(ncaa_results, winning_seed=`Winning Seed`,
                      winning_score=`Winning Score`,
                      losing_score=`Losing Score`)
#use ncaa definition of cinderella team to create binary score for cinderella
ncaa_results<- 
  ncaa_results%>%
  mutate(
    cind_binary=case_when(
      Round=="Round of 32" & winning_seed >= 11~2,
      Round != "Round of 32" | winning_seed<11 ~1))
#Convert name of rounds to numerical values
ncaa_results<-
  ncaa_results%>%
  mutate(
    Round=case_when(
      Round=="Round of 64"~1,
      Round=="Round of 32"~2,
      Round=="Sweet Sixteen"~3,
      Round=="Elite Eight"~4,
      Round=="National Semifinals"~5,
      Round=="National Championship"~6))
#Combine year and team name into one column for ncaa results
ncaa_results$Winner<- paste(ncaa_results$Winner, ncaa_results$year)
ncaa_results$Loser<- paste(ncaa_results$Loser, ncaa_results$year)
#Format out "NCAA" from team names in ncaa reg
ncaa_reg<-
  ncaa_reg%>%
  mutate(school2=sub(" NCAA.*", "", School))
#show only the highest round each team lost in. Flaw is that filters out teams that didn't lose (champions)
ncaa_results_max<-
  ncaa_results%>%
  group_by(Loser)%>%
  filter(Round==max(Round))%>%
  ungroup()%>%
  #Add games won column
  mutate(games_won=Round-1)
#Combine year and team name into one column for ncaa reg similar to lines 67-68
ncaa_reg$Year <- substring(ncaa_reg$SZN, 8)
#filter out years not included in both data sets
ncaa_reg<-filter (ncaa_reg, Year<17)
ncaa_results<-filter(ncaa_results, year>01)
#?
ncaa_reg$school2<- paste(ncaa_reg$school2, ncaa_reg$Year)
#join both data sets into one called ncaa_joint
ncaa_joint<-left_join(ncaa_reg, ncaa_results_max, by=c("school2"="Loser"))
#unselect irrelevant columns
ncaa_joint<-
  ncaa_joint%>%
  select(-School, -Date, -year, -Date2, -Overtime, -losing_score, -winning_score, -Region, -SZN)
#rename certain variables so able to be used
ncaa_joint<- rename(ncaa_joint, 
                    round_lost=Round,
                    team=school2,
                    seed=`Losing Seed`,
                    lost_to=Winner,
                    WLP=`W-L%`)
#filter out teams with NA cind_binary
ncaa_joint<- filter(ncaa_joint, cind_binary!="NA")
#add column pythag_resid with residuals between actual and predicted wins for each team
ncaa_joint<-mutate(ncaa_joint, pythag_resid=TW-pythag_predicted_wins)
#add predicted wins by each seed
ncaa_joint<-
  ncaa_joint%>%
  mutate(
    seed_predicted_wins=case_when(
      seed==1~3.34,
      seed==2~2.35,
      seed==3~1.84,
      seed==4~1.53,
      seed==5~1.11,
      seed==6~1.05,
      seed==7~0.90,
      seed==8~0.72,
      seed==9~0.59,
      seed==10~0.62,
      seed==11~0.66,
      seed==12~0.52,
      seed==13~0.25,
      seed==14~0.17,
      seed==15~0.09,
      seed==16~0.007),
    #repeat line 80 for some reason (error/doesn't show if don't do both)
    mm_games_won=round_lost-1,
    #create continuous cinderella score
    cind_score=mm_games_won/seed_predicted_wins)
#find line of best fit between cind_score and predicted wins
fit_phat_wins_cind_score <- lm(data = ncaa_joint, cind_score ~ pythag_predicted_wins)
fit_phat_wins_cind_score
#graph cind_score vs predicted wins 
graph_phat_wins_cind_score<-
  ggplot(data = ncaa_joint)+
  geom_point(mapping=aes(x=pythag_predicted_wins, y=cind_score), col="dodgerblue")+
  geom_abline(intercept = .25386 , slope = .02955, col="red")+
  labs(x="Pythag Predicted Wins",
       y="Cinderella Score",
       title="Pythag Predicted Wins vs Cinderella Score")+
  theme_bw()
graph_phat_wins_cind_score
#summarize correlation between pythag predicted wins and cinderella score
summary(fit_phat_wins_cind_score)
summarize(ncaa_joint, correlation=cor(pythag_predicted_wins, cind_score))
#find line of best fit between cind_score and predicted wins residual
fit_phat_wins_resid_cind_score <- lm(data = ncaa_joint, cind_score ~ pythag_resid)
fit_phat_wins_resid_cind_score
#graph cind_score vs predicted wins residual
ncaa_joint<-mutate(ncaa_joint, pythag_resid_cind_score_hat=.8997+pythag_resid*.1519)
graph_phat_wins_resid_cind_score<-
  ggplot(data = ncaa_joint)+
  geom_point(mapping=aes(x=pythag_resid_cind_score_hat, y=cind_score), col="dodgerblue")+
  geom_abline(intercept = 0 , slope = 1, col="red")+
  labs(x="Score Predicted by Pythag Residual",
       y="Cinderella Score",
       title="Score Predicted by Pythag Residual vs Cinderella Score",
       subtitle="RMSE=1.54")+
  theme_bw()
graph_phat_wins_resid_cind_score
summarize(ncaa_joint, 
          rmse = sqrt(mean((cind_score - pythag_resid_cind_score_hat)^2)))
#summarize correlation between pythag predicted wins residual and cinderella score 
summary(fit_phat_wins_resid_cind_score)
summarize(ncaa_joint, correlation=cor(pythag_resid, cind_score))
#find line of best fit between cind_score and every relevant var
ncaa_train<-
  ncaa_joint %>% 
  filter(Rk %% 2 == 1)
pred_cind_score_fit <- lm(cind_score ~ pythag_predicted_wins + pythag_resid + WLP + seed_predicted_wins + SOS + SRS + TPP + FGP, family = gaussian(), data = ncaa_train)
pred_cind_score_fit
#graph cind_score vs predicted_cind_score
ncaa_joint<-mutate(ncaa_joint, phat_cind_score=.686543+0.238709*pythag_predicted_wins+0.393451*pythag_resid+WLP*-7.422530+seed_predicted_wins*-1.025192+SOS*0.008635+SRS*0.102137+TPP*0.951688+FGP*-1.949016)      
pred_cind_score_graph<-
  ggplot(data = ncaa_joint)+
  geom_point(mapping=aes(x=phat_cind_score, y=cind_score), col=ncaa_joint$cind_binary)+
  geom_abline(intercept = 0 , slope = 1, col="red")+
  labs(x="Predicted Cinderella Score",
       y="Cinderella Score",
       title="Predicted Cinderella Score vs Cinderella Score",
       subtitle="RMSE=1.43")
theme_bw()
ggplotly(pred_cind_score_graph)
ncaa_test<-
  ncaa_joint %>% 
  filter(Rk %% 2 == 0)
#summarize correlation between predicted cinderella score and predicted cinderella score 
summarize(ncaa_test, correlation=cor(phat_cind_score, cind_score))
summarize(ncaa_joint, 
          rmse = sqrt(mean((cind_score - phat_cind_score)^2)))
#add residual between predicted cinderella score and actual cinderella score
ncaa_joint<-mutate(ncaa_joint, cind_score_resid=cind_score-phat_cind_score)
#histogram of frequency of each cinderella residual, bins=20
graph_cind_resid_b20<- ggplot(data = ncaa_joint) + 
  geom_histogram(mapping = aes(x = cind_score_resid), bins = 20)
graph_cind_resid_b20
#add continuous histogram bins=infinite
graph_cind_resid_curve<- ggplot(data = ncaa_joint) + 
  geom_density(mapping = aes(x = cind_score_resid))
graph_cind_resid_curve

