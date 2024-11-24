get_draft_class <- function(season = as.integer(format(Sys.Date(), "%Y"))){
  
  message(paste("\npulling draft data for",season,"season..."))
  
  # names for final df
  df_names <- c(
    "round","pick","team","player","pos_pfr","draft_age","last_season",
    "all_pro","pro_bowl","years_starter","career_av","draft_team_av",
    "games","completions","pass_attempts","pass_yards","pass_touchdowns",
    "pass_interceptions","rush_attempts","rush_yards","rush_touchdowns",
    "receptions","receiving_yards","receiving_touchdowns",
    "solo_tackles","defense_interceptions","sacks","college","draft_year"
  )
  
  # starting url
  session <- polite::bow("https://www.pro-football-reference.com/years/")
  
  # draft class url
  url <- paste0("https://www.pro-football-reference.com/years/",season,"/draft.htm")
  
  # select class
  session <- polite::nod(session, url)
  
  # gather data
  draft_raw <- polite::scrape(session)
  
  draft <- draft_raw |>
    rvest::html_table()
  
  # unlist and fix up the names
  suppressWarnings({
    draft <- draft[[1]] |>
      janitor::row_to_names(row_number = 1) |>
      janitor::clean_names() |>
      dplyr::select(1:28) |>
      dplyr::mutate(draft_year = season) |>
      dplyr::filter(rnd != "Rnd")
  })
  
  colnames(draft) <- df_names
  
  draft <- utils::type.convert(draft, as.is = "TRUE")
  
  # sacks weren't a thing before 95 I guess
  if(is.character(draft$sacks[1])){
    draft <- draft |>
      dplyr::select(-college) |>
      dplyr::rename(college = sacks)
  }
  
  return(draft)
}
