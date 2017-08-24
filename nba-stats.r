
library(tidyverse)
library(jsonlite)
library(plotly)
library(DT)
library(knitr)
library(stringr)

# Import json files from web

# List of json URLs

json_urls <- c("http://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2016-17&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=
",
               "http://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Scoring&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2016-17&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=
",
               "http://stats.nba.com/stats/leaguedashptdefend?College=&Conference=&Country=&DateFrom=&DateTo=&DefenseCategory=2+Pointers&Division=&DraftPick=&DraftYear=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&Season=2016-17&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=
",
               "http://stats.nba.com/stats/leaguedashptdefend?College=&Conference=&Country=&DateFrom=&DateTo=&DefenseCategory=3+Pointers&Division=&DraftPick=&DraftYear=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&Season=2016-17&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=
",
               "http://stats.nba.com/stats/leaguedashptdefend?College=&Conference=&Country=&DateFrom=&DateTo=&DefenseCategory=Overall&Division=&DraftPick=&DraftYear=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&Season=2016-17&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=
",
               "http://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2016-17&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=
","http://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2016-17&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=
",
                    "http://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2016-17&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=
",
                    "http://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Four+Factors&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2016-17&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=
")
json_names <- list("nba_advanced", "nba_assistedFGs", "nba_d2fgm", "nba_d3fgm", "nba_dfg", "nba_plusminus", "nba_teambasic", "nba_teamadvanced", "nba_fourfactors")

# Create Data Import Loop

data_list <- list(NULL)
for(i in seq_along(json_urls)){
  foo <- fromJSON(paste(readLines(json_urls[i])),
                  simplifyDataFrame = TRUE,
                  flatten = TRUE)
  flat_file <- flatten(foo$resultSets)
  foo <- data.frame(flat_file$rowSet)
  colnames(foo) <- t(data.frame(flat_file$headers))
  data_list[i] <- list(foo)
  print(i)
  }
data_list <- setNames(data_list, nm = json_names)

# import csv, edit column = name to match json files

nba_rpm <- read_csv(
  "https://raw.githubusercontent.com/robert-fields/nba-stats/master/nba_rpm_csv.csv",
  col_types = "_?___????")
nba_rpm$Name <- substr(nba_rpm$Name,
                       1,
                       nchar(nba_rpm$Name)-
                         (nchar(nba_rpm$Name)-str_locate(nba_rpm$Name,','))-1)

# match different spellings of names
nba_rpm$Name[nba_rpm$Name == "Nene Hilario"] <- "Nene"
nba_rpm$Name[nba_rpm$Name == "Sheldon Mac"] <- "Sheldon McClellan"

# Merge relevant variables from json files and csv and filter out NA from csv

player_data <-
  data_list[[1]] %>%
  left_join(data_list[[2]][,c("PLAYER_ID","PCT_AST_FGM","PCT_UAST_FGM")],
          by = "PLAYER_ID") %>%
  left_join(data_list[[3]][,c("PLAYER_NAME","FG2M")],
            by = "PLAYER_NAME") %>%
  left_join(data_list[[4]][,c("PLAYER_NAME","FG3M")],
            by = "PLAYER_NAME") %>%
  left_join(data_list[[5]][,c("PLAYER_NAME","D_FG_PCT")],
            by = "PLAYER_NAME") %>%
  left_join(data_list[[6]][,c("PLAYER_ID","PLUS_MINUS")],
            by = "PLAYER_ID") %>%
  left_join(nba_rpm, by = c("PLAYER_NAME" = "Name")) %>%
  filter(!is.na(RPM))

# Merge team data

team_data <-
  data_list[[9]] %>%
  left_join(data_list[[7]][,c("TEAM_ID","PLUS_MINUS")],
            by = "TEAM_ID") %>%
  left_join(data_list[[8]][,c("TEAM_ID","TS_PCT","PIE",
                              "NET_RATING","OFF_RATING","DEF_RATING",
                              "AST_PCT","AST_TO","AST_RATIO")],
            by = "TEAM_ID")

# clean up workspace
rm(list = setdiff(ls(), c("player_data","team_data")))

# change vectors to numerics
player_data[5:67] <- sapply(player_data[5:67],
  function(x) as.numeric(as.character(x)))

team_data[3:39] <- sapply(team_data[3:39],
                            function(x) as.numeric(as.character(x)))

# filter out unneeded variables, match team and player data, add variables
player_data <-
  player_data %>%
  select(-(GP_RANK:CFPARAMS)) %>%
  left_join(mutate(team_data,
                   TEAM_PIE = PIE,
                   TEAM_PLUS_MINUS = PLUS_MINUS,
                   TEAM_WINS = W,
                   TEAM_MIN = MIN,
                   TEAM_NET_RATING = NET_RATING,
                   TEAM_OFF_RATING = OFF_RATING,
                   TEAM_DEF_RATING = DEF_RATING)
            [,c("TEAM_ID","TEAM_PIE","TEAM_PLUS_MINUS","TEAM_WINS","TEAM_MIN",
                "TEAM_NET_RATING","TEAM_OFF_RATING","TEAM_DEF_RATING")],
       by = "TEAM_ID") %>%
       mutate(TEAM_RPM_SHARE = (RPM * (GP * MIN) / (TEAM_MIN)))

team_data <-
 team_data %>%
 left_join(
   aggregate(player_data$TEAM_RPM_SHARE ~ player_data$TEAM_ID,
     player_data, sum),
     by = c("TEAM_ID" = "player_data$TEAM_ID")) %>%
 rename("TEAM_RPM" = "player_data$TEAM_RPM_SHARE") %>%
 left_join(
           aggregate(player_data$WINS ~ player_data$TEAM_ID,player_data, sum),
           by = c("TEAM_ID" = "player_data$TEAM_ID")) %>%
 rename("TEAM_RPM_WINS" = "player_data$WINS") %>%
 select(-(GP_RANK:CFPARAMS))

# plot useful exploratory charts

# KEEP
ggplotly(
  player_data %>%
  arrange(desc(TEAM_PLUS_MINUS)) %>%
  ggplot(aes(TEAM_ABBREVIATION, RPM)) +
    geom_boxplot())

# KEEP
ggplotly(
  player_data %>%
  arrange(desc(TEAM_PLUS_MINUS)) %>%
  ggplot(aes(RPM, fill = TEAM_ABBREVIATION)) +
  geom_density(alpha = .2))

# KEEP
ggplotly(
  ggplot(team_data,aes(TEAM_RPM,NET_RATING)) +
    geom_point(aes(color = TEAM_NAME)) +
    geom_smooth(method = "glm") +
    geom_abline()
)
ggplotly(
  ggplot(team_data,aes(TEAM_RPM_WINS,W)) +
    geom_point(aes(color = TEAM_NAME)) +
    geom_smooth(method = "glm") +
    geom_abline())


