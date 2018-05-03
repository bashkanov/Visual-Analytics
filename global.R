library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

league_summary <- function(dat, league){
  
  selected_league <- dat %>%
    filter(dat$league_name == league) %>%
    select(home_team, home_team_goal, away_team, away_team_goal)
  
  by_home_team <- selected_league %>%
    group_by(home_team) %>%
    summarise(total_home = n(), total_goal_home = sum(home_team_goal))
  
  by_away_team <- selected_league %>%
    group_by(away_team) %>%
    summarise(total_away = n(), total_goal_away = sum(away_team_goal))
  
  summ_together <- full_join(by_home_team, by_away_team, by=c("home_team" = "away_team"))
  
  names(summ_together)[names(summ_together) == "home_team"] <- "team"
  drop_cols <- c("total_home", "total_goal_home", "total_away", "total_goal_away")
  
  summ_together <- summ_together %>%
    mutate(total_matches = total_home + total_away,
           total_goal = total_goal_home + total_goal_away,
           avg = round(total_goal / total_matches,digits=2)) %>%
    select(-one_of(drop_cols))
  
  return (summ_together)
}

stage_league_summary <- function(dat, league){
  selected_stage <- dat %>%
    filter(dat$league_name == league) %>%
    select(stage, home_team_goal, away_team_goal)
  
  selected_stage <- selected_stage %>%
    mutate(sum = home_team_goal + away_team_goal)%>%
    select(-one_of(c("home_team_goal", "away_team_goal")))
  
  summ_stage <- selected_stage %>%
    group_by(stage) %>%
    summarise(mean = mean(sum))

  return(summ_stage)
}

league_summary_year <- function(dat, league, year){
  selected_league_year <- dat %>%
    filter(dat$league_name == league, grepl(year, dat$date)) %>%
    select(home_team, home_team_goal, away_team, away_team_goal, date)
  
  summ_by_home_y <- selected_league_year %>%
    group_by(home_team) %>%
    summarise(mean_home = round(mean(home_team_goal),digits=2), 
              total_goal_home = sum(home_team_goal))
  
  summ_by_away_y <- selected_league_year %>%
    group_by(away_team) %>%
    summarise(mean_away = round(mean(away_team_goal),digits=2),
              total_goal_away = sum(away_team_goal))
  
  summ_together_y <- full_join(summ_by_home_y, summ_by_away_y, by=c("home_team" = "away_team"))
  summ_together_y <- summ_together_y %>%
    mutate(total_goal = total_goal_home + total_goal_away)
  
  #some cosmetics 
  names(summ_together_y)[names(summ_together_y) == "home_team"] <- "team"
  summ_together_y <- summ_together_y[c(1,6,3,5,2,4)]
  
  return(summ_together_y)
}

win_hist <- function(dat, league, year){
  
  selected_league_year <- dat %>%
    filter(dat$league_name == league, grepl(year, dat$date)) %>%
    select(home_team, home_team_goal, away_team, away_team_goal)
  
  selected_league_year <- selected_league_year %>%
    mutate(home_team_win = ifelse(selected_league_year$home_team_goal > selected_league_year$away_team_goal, 1, 0),
           away_team_win = ifelse(selected_league_year$home_team_goal < selected_league_year$away_team_goal, 1, 0))
  
  home_wins <- selected_league_year %>%
    filter(home_team_win > 0) %>%
    select(home_team, home_team_win) %>%
    group_by(home_team, home_team_win) %>%
    summarise(total_wins = sum(home_team_win)) %>%
    ungroup() %>%
    select(-one_of(c("home_team", "home_team_win"))) %>%
    mutate(type = "home")
  
  away_wins <- selected_league_year %>%
    filter(away_team_win > 0) %>%
    select(away_team, away_team_win) %>%
    group_by(away_team, away_team_win) %>%
    summarise(total_wins = sum(away_team_win)) %>%
    ungroup() %>%
    select(-one_of(c("away_team", "away_team_win")))%>%
    mutate(type = "away")
  
  return(total_wins_hist <- bind_rows(home_wins, away_wins))
}

goals_proportion <- function(dat_prop){
  dat_prop <- dat_prop %>%
    select(team, total_goal, total_goal_home, total_goal_away) %>%
    gather(cond, goals, total_goal_home:total_goal_away) %>%
    mutate(difference = total_goal - goals)
  return(dat_prop)
}

average_per_month <- function(dat, league, year){
  sel_league <- dat %>%
    filter(dat$league_name == league, grepl(year, dat$date)) %>%
    select(home_team, away_team, home_team_goal, away_team_goal, date) %>%
    mutate(date = as.Date(str_replace(date, " 00:00:00", "")))
  by_month <- sel_league %>%
    group_by(month = floor_date(date, "month")) %>%
    summarise(mean = round(sum(mean(home_team_goal),
                                          mean(away_team_goal)),
                                      digits=2)) 
  return(by_month)
}

get_all_possessions <- function(dat, league, year){
  
  selected_home <- dat %>%
    filter(!is.na(dat$home_team_possession), dat$league_name == league, grepl(year, dat$date)) %>%
    select(home_team_goal, home_team_possession)
  
  selected_away <- dat %>%
    filter(!is.na(dat$away_team_possession) & dat$league_name == league, grepl(year, dat$date)) %>%
    select(away_team_goal, away_team_possession)
  
  names(selected_home) <- c("goal", "ball_possession")
  names(selected_away) <- c("goal", "ball_possession")
  
  total_possesion <- bind_rows(selected_home, selected_away)
  
  total_possesion <- total_possesion %>%
    mutate(possession_breaks = cut(ball_possession, breaks = c(0,25,40,60,75,100), 
                                   labels = c("very low", "low", "intermediate", "high", "very high")))
  return(total_possesion)
}

availability_check <- function(dat, league, year){
  selected_leagues <- dat %>%
    filter(!is.na(dat$home_team_possession), !is.na(dat$away_team_possession), grepl(year, dat$date)) %>%
    select(league_name)

  selected_leagues <- c(unique(selected_leagues$league_name))
  return(ifelse(league %in% selected_leagues, TRUE, FALSE))
}


dat <- read_rds("Data/european_soccer.rds")
league_names <- c(unique(dat$league_name))
league_names

# -----

# -----

league_list = list()
stage_list = list()
for (league in league_names){
  league_list[[league]] <- league_summary(dat, league)
  stage_list[[league]] <- stage_league_summary(dat, league)
}
