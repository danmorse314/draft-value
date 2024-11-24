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

draft_new <- purrr::map_dfr(
  .x = 2021:2024,
  ~get_draft_class(.x)
)

draft_raw <- bind_rows(pfr_data_raw, draft_new) |>
  arrange(-draft_year, pick)

draft_new |> write_csv("pfr_draft_classes.csv")
