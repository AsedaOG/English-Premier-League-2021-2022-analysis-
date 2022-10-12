---
title: "Epl 2021/2022 Exploratory Analysis"
author: "Kwame Boohene"
date: '2022-10-08'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## English Premier League 2021/2022 Exploratory Analysis

**This project is a descriptive analysis of the 2021/2022 nglish premier league in order to summarise and past information and identify general peformance that season**.

**Our data was obtained from kaggle and was verified to be open source. Available to us were  2 csv documents. The first file documented statistics from the match such as home goals, away goals. home shots, away shots etc. The second file tracked the ranking of  the teams as the league progressed. R was used for analysis and Tableau for visualization**

We begin by loading our libraries

```{r loading libraries}
library(tidyverse)
library(lubridate)
library(writexl)
library(skimr)
library(ggplot2)
library(readr)
library(janitor)
library(formattable)
library(plyr)
```

We then load in the data we are analysing


**epl_21_22 <- read_csv("Desktop/Dashboard Analysis/Epl 21:22/soccer21-22.csv")**


We get a fair summary of our data by pre viewing the data and getting a brief description of coloumns usisng **view**, **str** and **head** functions

```{r }
epl_21_22 <- read_csv("/Users/asedaog/Desktop/Projects/Epl 21:22/Data for Analysis/soccer21-22.csv")
weekly_rankings <- read_csv("Desktop/Dashboard Analysis/Epl 21:22/weeklyrank.csv")
view(epl_21_22)
view(weekly_rankings)
```

```{r }
str(epl_21_22)
str(weekly_rankings)
```

```{r }
head(epl_21_22)
head(weekly_rankings)
```

# Data Cleaning

**We check for the number of individual teams**
```{r }
n_unique(epl_21_22$HomeTeam)
n_unique(epl_21_22$AwayTeam)
```
We determine that the number of teams is *20*

**Checking for duplicates**
```{r }
sum(duplicated(epl_21_22))
sum(duplicated(weekly_rankings))
```



**Renaming Coloumns**
```{r }
epl_21_22 <- epl_21_22 %>% 
  rename(fulltime_home_goals = FTHG , fulltime_away_goals = FTAG , fulltime_results = FTR , halftime_home_goals = HTHG , halftime_away_goals = HTAG , halftime_results = HTR )
view(epl_21_22)
epl_21_22 <- epl_21_22 %>% 
  rename(hometeam_shots = HS , awayteam_shots = AS , home_shots_ot = HST , away_shots_ot = AST , home_fouls = HF , away_fouls = AF, home_corners = HC , away_corners = AC , home_yellowcards = HY , away_yellowcards = AY , home_redcards = HR , away_redcards = AR)

clean_names(epl_21_22)

```

# Analysis

```{r}
length(epl_21_22$fulltime_results)
```
Total number of matches played is 380

**Calculating the number of Home Wins, Away Wins and Draws**
```{r }
df = epl_21_22
table(df$fulltime_results)

match_summary <- as.data.frame.table(table(df$fulltime_results))
match_summary <- plyr::rename(match_summary, c("Var1" = "Result"  ))
colnames(match_summary)[2]="Frequency"

match_summaries <- c(129, 88 , 163)
labels <- c ("AwayWin", "Draw" , "HomeWin" )
```

**We then plot a pie chart to show the information**
```{r}
match_summaries <- c(129, 88 , 163)
labels <- c ("AwayWin", "Draw" , "HomeWin" )
pie(match_summaries,labels)

```

**Calculating total goals per team**
```{r }
#total home goals by teams
total_home_goals <- aggregate(epl_21_22$fulltime_home_goals ~ epl_21_22$HomeTeam , epl_21_22 , sum)
#renaming coloumns  
total_home_goals <- plyr::rename(total_home_goals, c("epl_21_22$HomeTeam"= "team" , "epl_21_22$fulltime_home_goals" = "home_goals") )

#total away goals by teams
total_away_goals <- aggregate(epl_21_22$fulltime_away_goals ~ epl_21_22$AwayTeam , epl_21_22 , sum)
#renaming coloumns 
total_away_goals <- plyr::rename(total_away_goals, c("epl_21_22$AwayTeam" = "team" , "epl_21_22$fulltime_away_goals" = "away_goals" ))

#finding the sum of goals per team
total_goals <- merge(total_home_goals,total_away_goals, by=c("team"))
total_goals <-total_goals %>% 
  mutate(total_team_goals = total_goals$home_goals+total_goals$away_goals)
```

**finding the average number of goals per match per team**
```{r }
#number of matches played per team
#home matches
df4= epl_21_22
table(epl_21_22$HomeTeam) #total amount of matches by each team is 19
#away matches
df5= epl_21_22
table(epl_21_22$AwayTeam)#total amount of matches by each team is 19
#total amount of matches played by each team is 38
total_goals <-total_goals %>% 
  mutate(average_goals_permatch = total_goals$total_team_goals/38)
```

**wins by team**
```{r}
#calculating home wins per team
winners<- epl_21_22[epl_21_22$fulltime_results == "H" , "HomeTeam"]
df1 = winners
table(df1$HomeTeam)
#change table into data frame for easy renaming
as.data.frame.table(table(df1$HomeTeam))
home_wins <- as.data.frame.table(table(df1$HomeTeam))
#renaming coloumns
home_wins <- plyr::rename(home_wins, c("Var1" = "team"  ))
colnames(home_wins)[2]="home_w"
view(home_wins)
#calculating away wins per team
aways <- epl_21_22[epl_21_22$fulltime_results == "A" , "AwayTeam"]

df2 = aways
table(df2$AwayTeam)

#change table into data frame for easy renaming
as.data.frame.table(table(df2$AwayTeam))
away_winners <- as.data.frame.table(table(df2$AwayTeam))
#renaming coloumns
away_winners <- plyr::rename(away_winners, c("Var1" = "team"  ))
colnames(away_winners)[2]= "away_w"

# merging columns to find total amount of wins per team
total_wins <- merge(home_wins,away_winners, by=c("team")) %>% 
  mutate(season_w = home_wins$home_w+away_winners$away_w)
view(total_wins)
```

**Calculating fouls per referee**
```{r}
#total fouls per referees
total_home_fouls <- aggregate(epl_21_22$home_fouls ~ epl_21_22$Referee , epl_21_22 , sum)
total_home_fouls <- plyr::rename(total_home_fouls, c("epl_21_22$Referee"= "Referee" , "epl_21_22$home_fouls" = "home_fouls") )

total_away_fouls <- aggregate(epl_21_22$away_fouls ~ epl_21_22$Referee , epl_21_22 , sum)
total_away_fouls <- plyr::rename(total_away_fouls, c("epl_21_22$Referee"= "Referee" , "epl_21_22$away_fouls" = "away_fouls") )

total_fouls_referees <- merge(total_home_fouls,total_away_fouls, by=c("Referee"))
total_fouls_referees <- total_fouls_referees %>% 
  mutate(total_fouls = away_fouls+home_fouls)

```


```{r }
#average fouls per match by referee
#first we find the number of matches officated by referees
df3= epl_21_22
table(epl_21_22$Referee)

matches_per_ref<- as.data.frame.table(table(epl_21_22$Referee)) #number of matches officiated per referee
#renaming columns
matches_per_ref <- plyr::rename(matches_per_ref, c("Var1" = "Referee"  ))
colnames(matches_per_ref)[2]="number_of_matches"

#confirming results by comparing total number of matches offciated to total number of matches played
sum(matches_per_ref$number_of_matches) #answer is 380 which matches the total amount of matches played 
#adding number to our previous data frame
total_fouls_referees <- merge(total_fouls_referees,matches_per_ref, by=c("Referee"))
#adding average fouls per referee to dataset
total_fouls_referees <-total_fouls_referees %>% 
  mutate(average_fouls_permatch = total_fouls_referees$total_fouls/total_fouls_referees$number_of_matches)
```

**fouls per team**
```{r }
#total fouls per team
total_fouls_hometeam <- aggregate(epl_21_22$home_fouls ~ epl_21_22$HomeTeam, epl_21_22, sum )
total_fouls_hometeam <- plyr::rename(total_fouls_hometeam, c("epl_21_22$HomeTeam"= "Team" , "epl_21_22$home_fouls" = "home_fouls") )

total_fouls_awayteam <- aggregate(epl_21_22$away_fouls ~ epl_21_22$AwayTeam, epl_21_22, sum )
total_fouls_awayteam <- plyr::rename(total_fouls_awayteam, c("epl_21_22$AwayTeam"= "Team" , "epl_21_22$away_fouls" = "away_fouls") )

total_fouls_team <- merge(total_fouls_hometeam,total_fouls_awayteam, by=c("Team"))
view(total_fouls_team)
```


**calculating number of cards per referee**
```{r }
#total cards per referees
ref_home_yellowcards <- aggregate(epl_21_22$home_yellowcards ~ epl_21_22$Referee , epl_21_22 , sum)
ref_home_yellowcards <- plyr::rename(ref_home_yellowcards, c("epl_21_22$Referee"= "Referee" , "epl_21_22$home_yellowcards" = "home_yellow") )

ref_home_redcards <- aggregate(epl_21_22$home_redcards ~ epl_21_22$Referee , epl_21_22 , sum)
ref_home_redcards <- plyr::rename(ref_home_redcards, c("epl_21_22$Referee"= "Referee" , "epl_21_22$home_redcards" = "home_red") )

ref_away_yellowcards <- aggregate(epl_21_22$away_yellowcards ~ epl_21_22$Referee , epl_21_22 , sum)
ref_away_yellowcards <- plyr::rename(ref_away_yellowcards, c("epl_21_22$Referee"= "Referee" , "epl_21_22$away_yellowcards" = "away_yellow") )

ref_away_redcards <- aggregate(epl_21_22$away_redcards ~ epl_21_22$Referee , epl_21_22 , sum)
ref_away_redcards <- plyr::rename(ref_away_redcards, c("epl_21_22$Referee"= "Referee" , "epl_21_22$away_redcards" = "away_red") )

total_ycards_referees <- merge(ref_home_yellowcards, ref_away_yellowcards, by=c("Referee"))
total_rcards_referees <- merge(ref_home_redcards, ref_away_redcards, by=c("Referee"))
total_card_referees <- merge(total_ycards_referees,total_rcards_referees, by=c("Referee"))

total_card_referees <- total_card_referees %>% 
  mutate(total_reds = home_red+away_red)
total_card_referees <- total_card_referees %>% 
  mutate(total_yellows = home_yellow+away_yellow)

total_card_referees <- total_card_referees %>% 
  mutate(total_cards = total_reds+total_yellows)
```

**calculating the number of cards per match**
```{r}
#yellow_cards
#calculating home and away yellow cards
total_home_cards <- aggregate(epl_21_22$home_yellowcards ~ epl_21_22$HomeTeam , epl_21_22 ,  sum)
total_home_cards <- plyr::rename(total_home_cards, c("epl_21_22$HomeTeam"= "Team" , "epl_21_22$home_yellowcards" = "home_yellow_cards") )

total_away_cards <- aggregate(epl_21_22$away_yellowcards ~ epl_21_22$AwayTeam , epl_21_22 ,  sum)
total_away_cards <- plyr::rename(total_away_cards, c("epl_21_22$AwayTeam "= "Team" , "epl_21_22$away_yellowcards" = "away_yellow_cards") )

total_yellowcards <- merge(total_home_cards,total_away_cards, by=c("Team"))

total_yellowcards <- total_yellowcards %>% 
  mutate(total_yellowcards = home_yellow_cards + away_yellow_cards)

#calculating home and away red cards
total_home_redcards <- aggregate(epl_21_22$home_redcards ~ epl_21_22$HomeTeam , epl_21_22 ,  sum)
total_home_redcards  <- plyr::rename(total_home_redcards , c("epl_21_22$HomeTeam"= "Team" , "epl_21_22$home_redcards" = "home_red_cards") )

total_away_redcards <- aggregate(epl_21_22$away_redcards ~ epl_21_22$AwayTeam, epl_21_22 ,  sum)
total_away_redcards  <- plyr::rename(total_away_redcards , c("epl_21_22$AwayTeam"= "Team" , "epl_21_22$away_redcards" = "away_red_cards") )

total_redcards <- merge(total_home_redcards,total_away_redcards, by=c("Team"))
total_redcards <- total_redcards %>% 
  mutate(total_redcards = home_red_cards+away_red_cards)

#adding red and yellow card coloumns
allcards <- merge(total_yellowcards, total_redcards, by = c("Team"))
allcards <- allcards %>% 
  mutate(total_cards = total_yellowcards + total_redcards)
          
```

**calculating shots**
```{r}
#Calculating shots
home_shots <- aggregate(epl_21_22$hometeam_shots ~ epl_21_22$HomeTeam , epl_21_22 ,  sum)
home_shots <- plyr::rename(home_shots , c("epl_21_22$HomeTeam"= "Team" , "epl_21_22$hometeam_shots" = "home_shots") )

away_shots <- aggregate(epl_21_22$awayteam_shots ~ epl_21_22$AwayTeam , epl_21_22 ,  sum)
away_shots <- plyr::rename(away_shots , c("epl_21_22$AwayTeam"= "Team" , "epl_21_22$awayteam_shots" = "away_shots") )

all_shots <- merge(home_shots,away_shots, by=c("Team"))
all_shots <- all_shots %>% 
  mutate(total_shots = home_shots+away_shots)

```

**calculating shots on target**
```{r}
#Calculating shots on target
home_shots_ot <- aggregate(epl_21_22$home_shots_ot ~ epl_21_22$HomeTeam , epl_21_22 ,  sum)
home_shots_ot  <- plyr::rename(home_shots_ot , c("epl_21_22$HomeTeam"= "Team" , "epl_21_22$home_shots_ot" = "home_shots_ot") )

away_shots_ot <- aggregate(epl_21_22$away_shots_ot ~ epl_21_22$AwayTeam , epl_21_22 ,  sum)
away_shots_ot <- plyr::rename(away_shots_ot , c("epl_21_22$AwayTeam"= "Team" , "epl_21_22$away_shots_ot" = "away_shots_ot") )

shots_ot <- merge(home_shots_ot,away_shots_ot, by=c("Team"))
shots_ot <- shots_ot %>% 
  mutate(all_shots_ot = home_shots_ot + away_shots_ot)

shots_summary <- merge(all_shots, shots_ot, by = c("Team"))

shots_summary <- shots_summary %>% 
  mutate(percentage_shots_ot = percent(all_shots_ot/total_shots ))
```

**conversion rate (shots on target to goals)**
```{r}
conversion_rate <- merge(total_goals,shots_ot, by= c("Team"))

conversion_rate <- conversion_rate %>% 
  select(Team,total_team_goals,all_shots_ot)

conversion_rate <- conversion_rate %>% 
  mutate(goal_conversion_rate = percent(total_team_goals/all_shots_ot))

```

**Exported tables as csv and converted to excel for use in Tableau**
```{r}
write_xlsx(total_goals,"Desktop\\total_goals.csv")
write_xlsx(total_wins,"Desktop\\total_wins.csv")
write_xlsx(total_fouls_referees,"Desktop\\total_fouls_ref.csv")
write_xlsx(total_fouls_team,"Desktop\\total_fouls_team.csv")
write_xlsx(allcards,"Desktop\\allcards.csv")
write_xlsx(shots_summary,"Desktop\\shots_summary.csv")
write_xlsx(conversion_rate,"Desktop\\conversion_rate.csv")
write_xlsx(epl_21_22,"Desktop\\epl.csv")
write_xlsx(total_card_referees,"Desktop\\cards_ref.csv")

```
