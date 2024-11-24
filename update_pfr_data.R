library(tidyverse)
source("get_draft_class.R")

draft_values <- read_csv(
  url("https://github.com/leesharpe/nfldata/raw/master/data/draft_values.csv"),
  show_col_types = FALSE
)

pfr_data_raw <- read_csv(
  url("https://raw.githubusercontent.com/danmorse314/draft-value/main/pfr_draft_classes.csv"),
  show_col_types = FALSE
)

glimpse(pfr_data_raw)

range(pfr_data_raw$draft_year)

draft_raw <- NULL
for(i in 1996:2024){
  draft_i <- get_draft_class(i)
  
  draft_raw <- bind_rows(draft_raw, draft_i)
  
  Sys.sleep(10)
  
}

draft_raw <- bind_rows(pfr_data_raw, draft_raw) |>
  arrange(-draft_year, pick)

draft_raw |> write_csv("pfr_draft_classes.csv")
