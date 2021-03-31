library(shiny)
library(DT)
library(plotly)
library(tidyverse)

# get team drafts
chart <- read_csv("https://raw.githubusercontent.com/danmorse314/draft-value/main/team_draft_data.csv",
                  col_types = cols())

# get team list
teams <- chart %>% select(team_name) %>% distinct() %>% arrange(team_name)

# get season list
drafts <- chart %>% select(draft_year) %>% distinct() %>% arrange(draft_year) %>%
  rename(Season = draft_year)

# get individual player info
pfr_predict <- read_csv("https://raw.githubusercontent.com/danmorse314/draft-value/main/player_draft_detail.csv",
                        col_types = cols())

# get draft logos
draft_logos <- read_csv(url("https://github.com/danmorse314/draft-value/raw/main/nfl_draft_logos.csv"),
                        col_types = cols())

# get nfl gms
nfl_gms <- read_csv(url("https://github.com/danmorse314/draft-value/raw/main/nfl_gms_by_year.csv"),
                    col_types =cols())

# get college data
colleges <- read_csv("https://raw.githubusercontent.com/danmorse314/draft-value/main/college_draft_data.csv",
                     col_types = cols())

ui <- fluidPage(
  tags$style(type = "text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  titlePanel("NFL team drafts by Approximate Value"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      uiOutput(outputId = "yearfilter"),
      uiOutput(outputId = "teamfilter"),
      "'AV' is Pro Football Reference's Approximate Value metric, described in detail ",
      tags$a(href = "https://pro-football-reference.com/blog/index37a8.html",
             "here"),
      br(),
      br(),
      "AV predictions based on accumulated AV at each draft position over the previous 5 years",
      br(),
      br(),
      "More discussion of how this was derived can be found ",
      tags$a(href = "https://beastpode.com/2021/01/17/john-schneider-seattle-seahawks-general-manager-nfl-draft/",
             "here"),
      br(),
      br(),
      "Code for the model and tables can be found ",
      tags$a(href = "https://github.com/danmorse314/draft-value",
             "here"),
      br(),
      br(),
      "Created by ",
      tags$a(href = "https://www.twitter.com/danmorse_",
             "Dan Morse"),
      br(),
      br(),
      "data from ",
      tags$a(href = "https://pro-football-reference.com",
             "Pro Football Reference"),
      br(),
      "team wordmarks & logos from ",
      tags$a(href = "https://www.nflfastr.com",
             "nflfastR")
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        selected = "tm",
        tabPanel(
          "By Team", value = "tm",
          uiOutput(outputId = "teamlogo"),
          uiOutput(outputId = "tabletitle"),
          uiOutput(outputId = "tablesub"),
          DT::dataTableOutput("teamtable"),
          uiOutput("message"),
          DT::dataTableOutput("bestpicks")
        ),
        tabPanel(
          "By Draft Year", value = "szn",
          uiOutput(outputId = "draftlogo"),
          uiOutput(outputId = "draftsub"),
          DT::dataTableOutput("seasontable"),
          uiOutput("message2"),
          uiOutput(outputId = "tabletitle2"),
          DT::dataTableOutput("teampicks")
        ),
        tabPanel(
          "By College", value = "school",
          DT::dataTableOutput(outputId = "collegeplot"),
          uiOutput(outputId = "message3"),
          DT::dataTableOutput(outputId = "collegetable")
        )
      )
    )
  )
)

server <- function(input, output, session){
  
  output$yearfilter <- renderUI({
    
    if(input$tabs == "tm"){
      sliderInput(
        inputId = "years",
        label = NULL,
        min = 1989, 
        max = 2020,
        value = c(2010, 2020),
        sep = ""
      )
    }
    else if(input$tabs == "szn"){
      selectInput(
        inputId = "season",
        label = "Select season",
        choices = drafts,
        selected = 2000,
        multiple = FALSE
      )
    }
    else if(input$tabs == "school"){
      sliderInput(
        inputId = "years",
        label = NULL,
        min = 1989, 
        max = 2020,
        value = c(2016, 2020),
        sep = ""
      )
    }
  })
  
  output$teamfilter <- renderUI({
    
    if(input$tabs == "tm"){
      selectInput(
        inputId = "tm",
        label = NULL,
        choices = teams,
        selected = "Seattle Seahawks",
        multiple = FALSE
      )
    }
    else if(input$tabs == "school"){
      sliderInput(
        inputId = "minpicks",
        label = "Minimum Picks:",
        min = 0, max = 189,
        value = 1
      )
    }
    else{
      NULL
    }
  })
  
  output$teamlogo <- renderUI({
    
    if(input$tabs == "tm"){
      req(input$tm)
      logo_url <- dplyr::filter(chart, team_name == input$tm) %>%
        dplyr::slice(1) %>%
        dplyr::pull(team_wordmark)
      
      HTML(paste("<center><img src=",logo_url," height = '60px'></center>"))
      
    }
    
  })
  
  output$tabletitle <- renderUI({
    
    if(input$tabs == "tm"){
      req(input$tm)
      req(input$years)
      sub_years <- seq(input$years[1], input$years[2], 1)
      HTML(paste("<center>",input$tm," drafts, ",input$years[1],"-",input$years[2],"</center>"))
      
    }
  })
  
  output$tablesub <- renderUI({
    
    if(input$tabs == "tm"){
      req(input$tm)
      req(input$years)
      sub_years <- seq(input$years[1], input$years[2], 1)
      
      meanrank <- chart %>%
        filter(draft_year  %in% sub_years) %>%
        group_by(team_name) %>%
        summarize(
          mean_rank = round(mean(season_rank_avoe),1),
          avoe = sum(avoe),
          .groups = "drop"
        ) %>%
        ungroup() %>%
        filter(team_name == input$tm)
      
      HTML(paste("<center>Average Season Rank: ",pull(meanrank,mean_rank)," | Total AVOE: ",pull(meanrank,avoe),"</center>"))
      
    }
  })
  
  output$teamtable <- DT::renderDataTable({
    
    if(input$tabs == "tm"){
      req(input$tm)
      req(input$years)
      sub_years <- seq(input$years[1], input$years[2], 1)
      
      sub <- dplyr::filter(chart, team_name == input$tm & draft_year %in% sub_years) %>%
        dplyr::arrange(draft_year) %>%
        dplyr::select(draft_year, season_rank_avoe, avoe, career_av, predict_av, picks, highest_pick)
      
      DT::datatable(
        sub,
        colnames = c("Draft Year" = "draft_year",
                     "Season Rank" = "season_rank_avoe",
                     "Total Career AV" = "career_av",
                     "AV Over Expectation" = "avoe",
                     "Predicted AV" = "predict_av",
                     "Picks" = "picks",
                     "Highest Pick" = "highest_pick"),
        #escape = FALSE,
        callback = JS("
    table.on( 'order.dt search.dt', function () {
        table.column(0, {search:'applied', order:'applied'}).nodes().each( 
            function (cell, i) {
                cell.innerHTML = i+1;
            } );
    } )"),
        selection = "single",
        options = list(
          dom = "t",
          columnDefs = list(list(
            className = "dt-center", targets = seq(1,7,1)
          )),
          #lengthMenu = list(c(10, 25, -1), c('10', '25', 'All')),
          pageLength = 32
          #)
        )
      )
    }
    
  })
  
  output$message <- renderUI({
    
    s <- input$teamtable_rows_selected
    
    if(length(s) == FALSE){
      
      tags$i("Click on a season to see all team draft picks for that year")
    }
    else{
      req(input$tm)
      
      sub_years <- seq(input$years[1], input$years[2], 1)
      
      sub <- dplyr::filter(chart, team_name == input$tm & draft_year %in% sub_years) %>%
        dplyr::arrange(draft_year) %>%
        dplyr::select(draft_year)
      
      yrs <- sub[s, , drop = FALSE] %>%
        pull(draft_year)
      
      nick <- filter(chart, team_name == input$tm) %>%
        slice(1) %>%
        pull(team_nick)
      
      logo_url <- dplyr::filter(chart, team_name == input$tm) %>%
        dplyr::slice(1) %>%
        dplyr::pull(team_logo_wikipedia)
      
      gm <- nfl_gms %>%
        filter(team_nick == nick & draft_year == yrs) %>%
        pull(gm)
      
      HTML(paste("<center><img src=",logo_url," height = '60px'><br>
                 General Manager: ",gm,"<br>
                 ",yrs,"draft class</center>"))
      
    }
  })
  
  output$bestpicks <- DT::renderDataTable({
    
    s <- input$teamtable_rows_selected
    
    if(length(s) == FALSE){
      NULL
    } else {
      req(input$years)
      req(input$tm)
      sub_years <- seq(input$years[1], input$years[2], 1)
      
      sub <- dplyr::filter(chart, team_name == input$tm & draft_year %in% sub_years) %>%
        dplyr::arrange(draft_year) %>%
        dplyr::select(draft_year)
      
      yrs <- sub[s, , drop = FALSE] %>%
        pull(draft_year)
      
      best_picks <- pfr_predict %>%
        filter(team_name == input$tm & draft_year == yrs) %>%
        arrange(-avoe) %>%
        select(draft_year,round,pick,player,pos_pfr,avoe,career_av,predict_av)
      
      DT::datatable(
        best_picks,
        colnames = c("Draft Year" = "draft_year",
                     "Round" = "round",
                     "Pick" = "pick",
                     "Player" = "player",
                     "Position" = "pos_pfr",
                     "Total Career AV" = "career_av",
                     "AV Over Expectation" = "avoe",
                     "Predicted AV" = "predict_av"),
        options = list(
          dom = "t",
          columnDefs = list(list(
            className = "dt-center", targets = seq(1,8,1)
          )),
          #lengthMenu = list(c(10, 25, -1), c('10', '25', 'All')),
          pageLength = 20
          #)
        ),
        rownames = TRUE
      )
      
    }
    
  })
  
  # by season tab
  
  output$draftlogo <- renderUI({
    
    if(input$tabs == "szn"){
      req(input$season)
      draft_logo_url <- dplyr::filter(draft_logos, draft_year == input$season) %>%
        dplyr::slice(1) %>%
        dplyr::pull(draft_logo)
      
      HTML(paste("<center><img src=",draft_logo_url," height = '90px'></center>"))
      
    }
    
  })
  
  output$draftsub <- renderUI({
    
    if(input$tabs == "szn"){
      req(input$season)
      date <- draft_logos %>%
        filter(draft_year == input$season) %>%
        slice(1) %>%
        pull(draft_date)
      
      HTML(paste("<center>",date,"</center>"))
      
    }
    
  })
  
  output$seasontable <- DT::renderDataTable({
    req(input$season)
    season.df <- chart %>%
      filter(draft_year == input$season) %>%
      mutate(Team = paste("<img src = ' ",team_logo_wikipedia,"' height = '25'></img>")) %>%
      select(Team,avoe,career_av,predict_av,picks,highest_pick)
    
    DT::datatable(
      season.df,
      colnames = c("Total Career AV" = "career_av",
                   "AV Over Expectation" = "avoe",
                   "Predicted AV" = "predict_av",
                   "Picks" = "picks",
                   "Highest Pick" = "highest_pick"),
      escape = FALSE,
      callback = JS("
    table.on( 'order.dt search.dt', function () {
        table.column(0, {search:'applied', order:'applied'}).nodes().each( 
            function (cell, i) {
                cell.innerHTML = i+1;
            } );
    } )"),
      selection = "single",
      options = list(
        dom = "t",
        columnDefs = list(list(
          className = "dt-center", targets = seq(1,6,1)
        )),
        #lengthMenu = list(c(10, 25, -1), c('10', '25', 'All')),
        pageLength = 32
        #)
      )
    )
    
  })
  
  output$message2 <- renderUI({
    
    s2 <- input$seasontable_rows_selected
    
    if(length(s2) == FALSE){
      br()
      tags$i("Click on a team to see all team draft picks for that year")
    }
    else{
      req(input$season)
      picks.df <- chart %>%
        filter(draft_year == input$season) %>%
        select(team_name)
      subteam <- picks.df[s2, , drop = FALSE] %>%
        pull(team_name)
      
      logo_url <- dplyr::filter(chart, team_name == subteam) %>%
        dplyr::slice(1) %>%
        dplyr::pull(team_wordmark)
      
      HTML(paste("<center><img src=",logo_url," height = '60px'></center>"))
    }
    
  })
  
  output$tabletitle2 <- renderUI({
    
    s2 <- input$seasontable_rows_selected
    
    if(length(s2)){
      req(input$season)
      sub_year <- input$season
      picks.df <- chart %>%
        filter(draft_year == input$season) %>%
        select(team_nick)
      sub_nick <- picks.df[s2, , drop = FALSE] %>%
        pull(team_nick)
      gm <-  nfl_gms %>%
        filter(team_nick == sub_nick & draft_year == input$season) %>%
        pull(gm)
      HTML(paste("<center>General Manager: ",gm,"<br>",sub_year,"draft class</center>"))
      
    }
  })
  
  output$teampicks <- DT::renderDataTable({
    
    s2 <- input$seasontable_rows_selected
    
    if(length(s2)){
      req(input$season)
      picks.df <- chart %>%
        filter(draft_year == input$season) %>%
        select(team_name)
      
      subteam <- picks.df[s2, , drop = FALSE] %>%
        pull(team_name)
      
      team_picks <- pfr_predict %>%
        filter(team_name == subteam & draft_year == input$season) %>%
        arrange(-avoe) %>%
        select(round,pick,player,pos_pfr,avoe,career_av,predict_av)
      
      DT::datatable(
        team_picks,
        colnames = c("Round" = "round",
                     "Pick" = "pick",
                     "Player" = "player",
                     "Position" = "pos_pfr",
                     "Total Career AV" = "career_av",
                     "AV Over Expectation" = "avoe",
                     "Predicted AV" = "predict_av"),
        options = list(
          dom = "t",
          columnDefs = list(list(
            className = "dt-center", targets = seq(1,7,1)
          )),
          #lengthMenu = list(c(10, 25, -1), c('10', '25', 'All')),
          pageLength = 20
          #)
        ),
        rownames = TRUE
      )
      
    }
    
  })
  
  # college tab
  
  # fix this in the event that input$minpicks is 'NA' or missing
  
  observeEvent(input$years, {
    
    if(input$tabs == "school"){
      
      req(input$years)
      
      sub_years <- seq(input$years[1], input$years[2], 1)
      
      max.picks <- colleges %>%
        filter(draft_year %in% sub_years) %>%
        group_by(college, color) %>%
        summarize(
          picks = sum(picks),
          .groups = "drop"
        ) %>%
        ungroup() %>%
        arrange(-picks) %>%
        slice(1) %>%
        pull(picks)
      
      updateSliderInput(
        inputId = "minpicks",
        min = 0, max = max.picks
      )
    }
  })
  
  output$collegeplot <- DT::renderDataTable({
    
    if(input$tabs == "school"){
      
      req(input$years)
      req(input$minpicks)
      
      sub_years <- seq(input$years[1], input$years[2], 1)
      
      coll.df <- colleges %>%
        filter(draft_year %in% sub_years) %>%
        mutate(team_logo = paste("<img src = ' ",logo,"' height = '25'></img>")) %>%
        group_by(college, team_logo) %>%
        summarize(
          picks = sum(picks),
          avoe = sum(avoe),
          career_av = sum(career_av),
          predict_av = sum(predict_av),
          highest_pick = min(highest_pick),
          .groups = "drop"
        ) %>%
        ungroup() %>%
        arrange(-avoe) %>%
        filter(picks >= input$minpicks)
      
      DT::datatable(
        coll.df,
        colnames = c("College" = "college",
                     " " = "team_logo",
                     "Total Career AV" = "career_av",
                     "AV Over Expectation" = "avoe",
                     "Predicted AV" = "predict_av",
                     "Picks" = "picks",
                     "Highest Pick" = "highest_pick"),
        escape = FALSE,
        callback = JS("
    table.on( 'order.dt search.dt', function () {
        table.column(0, {search:'applied', order:'applied'}).nodes().each( 
            function (cell, i) {
                cell.innerHTML = i+1;
            } );
    } )"),
        selection = "single",
        options = list(
          columnDefs = list(list(
            className = "dt-center", targets = seq(1,7,1)
          ),
          list(orderable = FALSE, targets = 2)),
          lengthMenu = list(c(25, 50, -1), c('25', '50', 'All')),
          pageLength = 25
        )
      )
      
    }
    
  })
  
  output$message3 <- renderUI({
    
    s3 <- input$collegeplot_rows_selected
    
    if(length(s3) == FALSE){
      
      tags$i("Click on a school to see all draft picks for the selected time frame")
    } 
    
    else{
      
      req(input$years)
      req(input$minpicks)
      
      sub_years <- seq(input$years[1], input$years[2], 1)
      
      picks.coll.df <- colleges %>%
        filter(draft_year %in% sub_years) %>%
        mutate(team_logo = paste("<img src = ' ",logo,"' height = '25'></img>")) %>%
        group_by(college, team_logo) %>%
        summarize(
          picks = sum(picks),
          avoe = sum(avoe),
          career_av = sum(career_av),
          predict_av = sum(predict_av),
          highest_pick = min(highest_pick),
          .groups = "drop"
        ) %>%
        ungroup() %>%
        arrange(-avoe) %>%
        filter(picks >= input$minpicks) %>%
        select(college)
      
      subcollege <- picks.coll.df[s3, , drop = FALSE] %>%
        pull(college)
      
      logo_url <- dplyr::filter(colleges, college == subcollege) %>%
        dplyr::slice(1) %>%
        dplyr::pull(logo)
      
      submascot <- filter(colleges, college == subcollege) %>%
        slice(1) %>%
        pull(mascot)
      
      if(is.na(logo_url) & !is.na(submascot)){
        HTML(paste("<center><font size = '16px'><b>",subcollege," ",submascot,"</b></font>",
                   "<br>",min(sub_years),"-",max(sub_years)," NFL drafts</center>"))
      } else if(is.na(logo_url) & is.na(submascot)){
        HTML(paste("<center><font size = '16px'><b>",subcollege,"</b></font>",
                   "<br>",min(sub_years),"-",max(sub_years)," NFL drafts</center>"))
      } else{
        HTML(paste("<center><img src=",logo_url," height = '60px'>",
                   "<br>",subcollege," ",submascot,
                   "<br>",min(sub_years),"-",max(sub_years)," NFL drafts</center>"))
      }
    }
    
  })
  
  output$collegetable <- DT::renderDataTable({
    
    s3 <- input$collegeplot_rows_selected
    
    if(length(s3)){
      
      req(input$years)
      req(input$minpicks)
      
      sub_years <- seq(input$years[1], input$years[2], 1)
      
      coll.df <- colleges %>%
        filter(draft_year %in% sub_years) %>%
        mutate(team_logo = paste("<img src = ' ",logo,"' height = '25'></img>")) %>%
        group_by(college, team_logo) %>%
        summarize(
          picks = sum(picks),
          avoe = sum(avoe),
          career_av = sum(career_av),
          predict_av = sum(predict_av),
          highest_pick = min(highest_pick),
          .groups = "drop"
        ) %>%
        ungroup() %>%
        arrange(-avoe) %>%
        filter(picks >= input$minpicks)
      
      subcollege <- coll.df[s3, , drop = FALSE] %>%
        pull(college)
      
      college_picks <- pfr_predict %>%
        filter(college == subcollege & draft_year %in% sub_years) %>%
        mutate(Team = paste("<img src = ' ",team_logo_wikipedia,"' height = '25'></img>")) %>%
        arrange(-avoe) %>%
        select(draft_year,round,pick,Team,player,pos_pfr,avoe,career_av,predict_av)
      
      DT::datatable(
        college_picks,
        colnames = c("Draft Year" = "draft_year",
                     "Round" = "round",
                     "Pick" = "pick",
                     "Draft Team" = "Team",
                     "Player" = "player",
                     "Position" = "pos_pfr",
                     "Total Career AV" = "career_av",
                     "AV Over Expectation" = "avoe",
                     "Predicted AV" = "predict_av"),
        escape = FALSE,
        callback = JS("
    table.on( 'order.dt search.dt', function () {
        table.column(0, {search:'applied', order:'applied'}).nodes().each( 
            function (cell, i) {
                cell.innerHTML = i+1;
            } );
    } )"),
        selection = "single",
        options = list(
          columnDefs = list(list(
            className = "dt-center", targets = seq(1,9,1)
          )),
          lengthMenu = list(c(10, 25, -1), c('10', '25', 'All')),
          pageLength = 10
        ),
        rownames = TRUE
      )
      
    }
    
  })
}

shinyApp(ui = ui, server = server)