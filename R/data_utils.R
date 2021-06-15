#' Clean the NBA API data
#' 
#' @param data object returned from `get_current_standings()`
clean_list_data <- function(data) {
  
  var_names <- tolower(unlist(data$headers))
  list_data <- data$rowSet
  
  # replace NULL values with NAs
  
  list_data <- lapply(list_data, function(x) {
    lapply(x, function(y) {
      ifelse(is.null(y),
             NA,
             y)
    })
  })
  
  unlist_data <- list_to_tibble(list_data)
  
  names(unlist_data) <- var_names
  
  # convert character vars to numeric
  
  num_vars <-  suppressWarnings(apply(unlist_data, 2,
                                      function(x) {
                                        tot_miss <- sum(is.na(as.numeric(x)))
                                        ifelse(tot_miss == length(x), 0, 1)
                                      }))
  
  unlist_data <- unlist_data %>%
    mutate_at(which(num_vars == 1), as.numeric)
  
  return(unlist_data)
}


#' Transform a list to a tibble
#'
#' @param l a list
#' 
#' @return a tibble

list_to_tibble <- function(l) {
  df <- do.call(rbind, lapply(l, function(x) {
    ul <- unlist(x)
    ul %>%
      matrix(byrow = T, ncol = length(ul)) %>%
      as.data.frame(stringsAsFactors = F) %>%
      dplyr::as_tibble()
  })
  )
  return(df)
}


#' Get current NBA standings
#'
#'@details scrapes the nba.com/stats website for current
#'nba standings
get_current_standings <- function() {
  
  base_url <- 'https://stats.nba.com/stats/leaguestandingsv3'
  df <- httr::GET(base_url,
                  httr::add_headers(.headers = c( 'User-Agent'= 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.193 Safari/537.36',
                                                  'Referer'= 'https://stats.nba.com/players/drives/',
                                                  'x-nba-stats-origin'= 'stats',
                                                  'x-nba-stats-token'= 'true',
                                                  "Host"='stats.nba.com',
                                                  'Origin'='https://www.nba.com',
                                                  'Referer'='https://www.nba.com/')),
                  query = list(LeagueID = '00',
                               Season = '2020-21',
                               SeasonType = 'Regular Season'))
  json_data <- httr::content(df)
  
  df <- lapply(json_data$resultSets, clean_list_data)[[1]]
  
  return(df)
  
}

#' Prepare standings data for tables
#'
#' @details scrapes data from nba.com/stats
#' creates two tibbles, one for the eastern conference
#' standings and western conference standings.
prepare_standings_data <- function(records) {
  
  
  standings <- get_current_standings()
  
  logos <- records %>%
    dplyr::filter(year_season == 2020) %>%
    dplyr::distinct(id_team,  url_team_season_logo)
  
  eastern <- standings %>%
    dplyr::filter(conference == 'East') %>%
    dplyr::rename(points_for = pointspg,
                  points_against  = opppointspg,
                  diff = diffpointspg) %>%
    dplyr::left_join(logos, by = c('teamid' = 'id_team')) %>%
    dplyr::arrange(desc(wins)) %>%
    dplyr::mutate(team = paste(teamcity, teamname)) %>%
    dplyr::select(playoffrank, teamname,
           url_team_season_logo,
           team, wins, losses, winpct,
           conferencerecord, home,
           road, l10, strcurrentstreak,
           points_for, points_against,
           diff, record) %>%
    dplyr::arrange(playoffrank)
  
  western <- standings %>%
    dplyr::filter(conference == 'West') %>%
    dplyr::rename(points_for = pointspg,
                  points_against  = opppointspg,
                  diff = diffpointspg) %>%
    dplyr::left_join(logos, by = c('teamid' = 'id_team')) %>%
    dplyr::arrange(desc(wins)) %>%
    dplyr::mutate(team = paste(teamcity, teamname)) %>%
    dplyr::select(playoffrank, teamname,
           url_team_season_logo,
           team, wins, losses, winpct,
           conferencerecord, home,
           road, l10, strcurrentstreak,
           points_for, points_against,
           diff, record)%>%
    arrange(playoffrank)
  
  return_data <- tibble(eastern = eastern,
                        western = western)
  
  return(return_data)
  
}

#' Create a reactable for the app
#'
#' @param data either the eastern or western conference
#' standings tibble returned from `prepare_standings_data()`
#' @param select an argument passed to reactable. Default
#' is NULL. Other acceptible values are "single" and "multiple"
create_standings_table <- function(data, select = NULL) {
  reactable::reactable(data,
            defaultColDef = colDef(
              sortNALast = TRUE,
              minWidth = 45,
              class = JS("function(rowInfo, colInfo, state) {
        // Highlight sorted columns
        for (var i = 0; i < state.sorted.length; i++) {
          if (state.sorted[i].id === colInfo.id) {
            return 'sorted'
          }
        }
      }"),
              headerClass = "box-score-header",
              
            ),
            columns = list(
              
              playoffrank = colDef(
                name = 'RANK',
                cell = function(value) {
                  if(nchar(value) == 1) {
                    return(paste0('0', value))
                  } else {
                    return(as.character(value))
                  }
                }
              ),
              url_team_season_logo = colDef(cell = function(value) {
                image <- img(src = value, height = "24px", alt = value)
                tagList(
                  div(style = list(display = "inline-block", width = "24px"), image)
                )
              },
              name = ""
              ),
              team = colDef(cell = function(value, index) {
                # Render as a link
                url <- sprintf("https://ca.global.nba.com/teams/#!/%s", tolower(data[index, 'teamname']))
                htmltools::tags$a(href = url, target = "_blank", as.character(value))
              },
              name = "NAME",  width = 140),
              
              wins = colDef(name = 'W', width = 30),
              losses = colDef(name = 'L', width = 30),
              winpct = colDef(name = 'PCT',
                              format = colFormat(percent = TRUE, digits = 1),
                              minWidth = 55),
              conferencerecord = colDef( name = 'CONF'),
              home = colDef(name = 'HOME', minWidth = 55),
              road = colDef(name = "ROAD"),
              l10 = colDef(name = 'LAST 10', minWidth = 55),
              strcurrentstreak = colDef(name = 'STREAK', minWidth = 55),
              points_for = colDef(name = 'PF'),
              points_against = colDef(name = 'PA'),
              diff = colDef(name = 'DIFF'),
              record = colDef(show=F),
              teamname = colDef(show=F)
            ),
            showSortIcon = FALSE,
            highlight = TRUE,
            striped = TRUE,
            class = "box-score-tbl",
            defaultPageSize = 15,
            height = 650, selection = select)
}


#' Create a reactable from matching historical records
#' @param data a records object of matching data
#'
create_historical_table <- function(data){
  
  reactable::reactable(data,
            defaultColDef = colDef(
              sortNALast = TRUE,
              minWidth = 45,
              class = JS("function(rowInfo, colInfo, state) {
        // Highlight sorted columns
        for (var i = 0; i < state.sorted.length; i++) {
          if (state.sorted[i].id === colInfo.id) {
            return 'sorted'
          }
        }
      }"),
              headerClass = "box-score-header",
              
            ),
            columns = list(
              matching_records = colDef(name = "HISTORICAL MATCHES", width = 100),
              wins = colDef(cell = function(values, index) {
                sparkline(matching_records_sum$wins_list[[index]],
                          type = "box")
              }),
              losses = colDef(cell = function(values, index) {
                sparkline(data$losses_list[[index]],
                          type = "box")
              }),
              made_playoffs = colDef(name = 'MADE PLAYOFFS',
                                     format = colFormat(percent = TRUE, digits = 1)),
              losses_list = colDef(show = F),
              wins_list = colDef(show = F),
              best = colDef(class = 'best-worst',
                            name = "BEST RECORD"),
              worst = colDef(class = 'best-worst',
                             name = "WORST RECORD")
            ),
            class = "box-score-tbl",
            defaultPageSize = 15,
            showSortIcon = FALSE,
            highlight = TRUE,
            striped = TRUE)
  
}

#' Summarize results for NBA W/L record
#' 
#'@param data nba record data
#'
#'@details For a given NBA record, summarize the outcome for 
#'end of season results. 
#'
#'@return a tibble with summary records for each record  
get_record_summary <- function(data) {
  data %>%
    dplyr::group_by(record) %>%
    dplyr::summarize(matching_records = n(),
              made_playoffs = sum(made_playoffs)/matching_records,
              wins_list = list(final_wins),
              losses_list = list(final_losses),
              best = paste(year_season[final_wins == max(final_wins)],
                           name_team[final_wins == max(final_wins)],
                           "with", max(final_wins), 'wins.'),
              worst = paste(year_season[final_wins == min(final_wins)][1],
                            name_team[final_wins == min(final_wins)][1],
                            "with", min(final_wins), 'wins.')) %>%
    dplyr::mutate(wins = NA, losses = NA) %>%
    dplyr::arrange(as.numeric(sapply(strsplit(record, '-'), '[[', 1)))
}

#' Get historical NBA matches
#' 
#' @details for a given input record (e.g. 3-4), 
#' extract all teams with a matching record in NBA history
#' 
#' @param data all historical nba results
#' @param current_records the input record to match (e.g. '5-1')
get_historical_matching_records <- function(data, current_records) {
  data <- data %>%
    dplyr::mutate(contains_record = ifelse(record %in% current_records, 1, 0)) %>%
    dplyr::group_by(name_team, year_season) %>%
    dplyr::arrange(date_game) %>%
    dplyr::mutate(keep = cumsum(contains_record)) %>%
    dplyr::filter(keep > 0) %>%
    dplyr::group_by(name_team, year_season) %>%
    dplyr::mutate(starting_record = record[row_number() == 1]) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(id = paste(name_team, year_season))
}


plot_franchise_wins_losses <- function(records, team = "Toronto Raptors") {
  
  p <-  records %>%
    dplyr::filter(name_team == team) %>%
    dplyr::mutate(made_playoffs = ifelse(made_playoffs == 1, "Made Playoffs",
                                  "Missed Playoffs")) %>%
    dplyr::group_by(year_season) %>%
    dplyr::slice(n()) %>%
    ggplot2::ggplot(aes(wins, losses, color = made_playoffs)) +
    ggplot2::geom_point() +
    ggplot2::labs(color = "Made Playoffs",
         title = "Wins vs. Losses each season",
         subtitle = paste("For the", team))
  
  plotly::ggplotly(p)
  
}

plot_diff_record <- function(records,
                             current_record,
                             diff = 3) {
  
  cc <-  records %>%
    filter(record %in% current_record) %>%
    group_by(name_team, year_season) %>%
    slice(1) %>%
    ungroup()
  p <- cc %>%
    ggplot(aes(plus_minus_pre)) +
    geom_density() +
    xlab("Total point differential") +
    geom_vline(xintercept = diff) +
    labs(title = paste0("Historical Differentials at time of ", current_record,
                        " record"))
  plotly::ggplotly(p)
  
}
