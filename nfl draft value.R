# NFL draft class value
# as measured by Pro Football Reference's Approximate Value (AV)

library(tidyverse)
library(gt)
#require(nflfastR) for team colors

# get draft values
draft_values <- read_csv(url("https://github.com/leesharpe/nfldata/raw/master/data/draft_values.csv"))

# get AV from PFR
pfr_data_raw <- read_csv(url("https://raw.githubusercontent.com/danmorse314/draft-value/main/pfr_draft_classes.csv")) %>%
  left_join(draft_values, by = "pick")

# list of teams
teams <- pfr_data_raw %>%
  group_by(team) %>%
  summarize(seasons = n()) %>% arrange(team)

# team colors to make it pretty
team_colors <- nflfastR::teams_colors_logos %>%
  filter(team_abbr %in% pfr_data_raw$team) %>%
  mutate(
    team_color3 = ifelse(team_abbr == "SEA", team_color, team_color3),
    team_color = ifelse(team_abbr == "SEA", team_color2, team_color)) #action green gang

# 22 players with no college provided, gets in the way of some data cleaning
pfr_data_raw <- pfr_data_raw %>%
  mutate(college = ifelse(is.na(college), "no college provided", college)) %>%
  select(team, player, pos_pfr, college, everything())

# replace all NA values with 0
is.na(pfr_data_raw) <- sapply(pfr_data_raw, is.infinite)
pfr_data_raw[is.na(pfr_data_raw)] <- 0

# getting potential games played by year
# really, it's the average games
# this helps weigh recent seasons where players haven't had time
# to accumulate as much AV
pot_games <- pfr_data_raw %>%
  group_by(draft_year, player) %>%
  summarize(
    games = sum(games),
    seasons = last_season - draft_year + 1,
    .groups = "drop"
  ) %>%
  ungroup() %>%
  group_by(draft_year) %>%
  summarize(
    mean_games = mean(games, na.rm = TRUE),
    mean_seasons = mean(seasons, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup()

pfr_data <- pfr_data_raw %>%
  left_join(pot_games, by = "draft_year")

# plotting AV by pick

pfr_data %>%
  group_by(pick) %>%
  summarize(career_av = mean(career_av)) %>%
  ggplot(aes(pick, career_av)) +
  geom_point(alpha = .2) +
  theme_bw()

# relationship between OTC draft value and career av by pick

summary(lm(career_av ~ otc + mean_games, pfr_data))
# OTC r.sq:     .298

# predict each season's worth of AVs with previous 5 seasons' data
prediction <- NULL
mod_results <- NULL
for(i in 1989:2020){
  
  print(paste0("Calculating draft year ",i,"..."))
  
  # training data (previous 5 years)
  train_data <- filter(pfr_data, between(draft_year, i - 5, i - 1))
  
  # data to predict
  predict_data <- filter(pfr_data, draft_year == i)
  
  # create model
  mod1 <- lm(career_av ~ otc + mean_games, train_data)
  
  # get predictions
  prediction_i <- tibble(
    pick = predict_data$pick,
    draft_year = predict_data$draft_year,
    predict_av = round(predict.lm(mod1, predict_data, digits = 1))
  )
  
  # saving the model r^2s to see which classes had the best predictions
  mod_result_i <- tibble(
    draft_year = i,
    r.sq = round(summary(mod1)$r.sq,digits = 3)
  )
  
  mod_results <- bind_rows(mod_results, mod_result_i)
  prediction <- bind_rows(prediction, prediction_i)
  rm(train_data, predict_data, mod1, prediction_i, mod_result_i)
}

# add predicted AV to original data frame
pfr_predict <- pfr_data %>%
  left_join(prediction, by = c("pick","draft_year")) %>%
  mutate(avoe = career_av - predict_av,
         team_avoe = draft_team_av - predict_av) %>%
  filter(!is.na(predict_av))

# get data by team and draft year

chart <- pfr_predict %>%
  group_by(team, draft_year) %>%
  summarize(
    picks = n(),
    highest_pick = min(pick),
    team_av = sum(draft_team_av),
    career_av = sum(career_av),
    predict_av = sum(predict_av),
    avoe = sum(avoe),
    team_avoe = sum(team_avoe),
    johnson = sum(johnson),
    stuart = sum(stuart),
    hill = sum(hill),
    otc = sum(otc),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  arrange(-avoe) %>%
  # all time rankings
  mutate(rank_avoe = row_number()) %>%
  group_by(draft_year) %>%
  # seasonal rankings
  mutate(season_rank_avoe = row_number()) %>%
  ungroup() %>%
  select(season_rank_avoe, everything()) %>%
  left_join(team_colors, by = c("team" = "team_abbr")) %>%
  filter(!is.na(avoe))

# create plot showing value over average over time

get_team_plot <- function(tm){
  
  sub <- filter(chart, team == tm)
  
  draft_plot <- chart %>%
    ggplot(aes(draft_year, team_avoe)) +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    geom_line(aes(color = team), show.legend = FALSE, alpha = .4) +
    scale_color_manual(values = team_colors$team_color) +
    geom_point(data = sub, color = sub$team_color, size = 3) +
    geom_line(data = sub, color = sub$team_color, size = 1.2) +
    scale_x_continuous(breaks = seq(1988, 2020, 4), limits = c(1989, 2020)) +
    theme(panel.grid.minor.x = element_blank()) +
    theme_bw() +
    labs(x = NULL, y = "Draft Team AV over expected",
         caption = "data from Pro Football Reference",
         title = paste0(sub$team_nick, " approximate value over expectation by draft class"),
         subtitle = "cumulative AV by draft class through 2020 season")
  
  return(draft_plot)
}

# create table with draft value over average for a specific timespan

get_team_table <- function(tm, yr){
  
  sub <- filter(chart, team == tm & draft_year >= yr) %>%
    select(draft_year, team_logo_wikipedia, season_rank_avoe, avoe, career_av,
           predict_av, team_nick, team_color, team_color2, team_color3, team_color4)
  
  tabl <- sub %>%
    select(draft_year, season_rank_avoe, avoe, career_av, predict_av)
  
  team_table <- tabl %>%
    arrange(draft_year) %>%
    gt() %>%
    tab_header(
      subtitle = paste0(sub$team_nick[1]," drafts, ",yr, "-2020"),
      title = html(
        web_image(
          url = sub$team_logo_wikipedia[1],
          height = px(50)
        )
      )
    ) %>%
    cols_label(
      draft_year = md("**Draft<br>Year**"),
      season_rank_avoe = md("**Season<br>Rank**"),
      career_av = md("**Total<br>Career AV**"),
      avoe = md("**AV Over<br>Expectation**"),
      predict_av = md("**Predicted<br>AV**")
    ) %>%
    data_color(
      columns = vars(season_rank_avoe),
      colors = scales::col_numeric(
        palette = c("green","white","red"),
        domain = c(1,32)
      )
    ) %>%
    cols_align(
      align = "center"
    ) %>%
    tab_source_note(
      source_note = "Table: @danmorse_ | Data: Pro Football Reference"
    ) %>%
    tab_options(
      heading.subtitle.font.size = px(20)
    )
  
  return(team_table)
}
