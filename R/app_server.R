#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  # global data
  nba_season_data <- nbaWinLossExplorer::nba_season_data
  
  
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
  
  # the reactive data object
  get_data <- mod_sim_inputs_server("sim_inputs_ui_1",
                        data = nba_season_data)
  
  # value box for matching records summary
  mod_value_box_server("value_box_matching_records",
                       type="matching_records",
                       data = get_data,
                       color = "olive")
  
  # value box for wins summary
  mod_value_box_server("value_box_wins",
                       type="wins",
                       data = get_data,
                       color = "lime")
  
  # value box for losses summary
  mod_value_box_server("value_box_losses",
                       type="losses",
                       data = get_data,
                       color = "red")
  
  # value box for made playoffs summary
  mod_value_box_server("value_box_playoffs",
                       type="playoffs",
                       data = get_data,
                       color = "fuchsia")
  
  mod_value_box_server("value_box_best_case",
                       type="best_case",
                       data = get_data,
                       color = "teal")
  
  mod_value_box_server("value_box_worst_case",
                       type="worst_case",
                       data = get_data,
                       color = "yellow")
  
  mod_datatable_server("datatable_ui_1",
                       data = get_data)
  
  mod_plotly_display_server("plotly_display_ui_1",
                            data = get_data)
 
}
