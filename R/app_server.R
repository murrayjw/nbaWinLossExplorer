#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  # current NBA standings
  standings <- prepare_standings_data(nba_season_data)
  
  # Eastern Conference Standings
  callModule(mod_standings_server, 
             "eastern_ui",
             type="eastern",
             data = standings)
  
  # Western Conference Standings
  callModule(mod_standings_server, 
             "western_ui",
             type="western",
             data = standings)
}
