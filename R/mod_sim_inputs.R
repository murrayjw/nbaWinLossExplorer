#' sim_inputs UI Function
#'
#' @description A shiny Module that captures numeric inputs
#' used to extract historical performance records.
#'
#' @param id Internal parameters for {shiny}
#' @param input Internal parameters for {shiny}
#' @param output Internal parameters for {shiny}
#' @param session Internal parameters for {shiny}.
#' @param status used to color the output cox
#' @noRd 
#' @importFrom shinydashboard box
#' @importFrom shiny NS tagList column
mod_sim_inputs_ui <- function(id, 
                              status="primary"){
  ns <- NS(id)
  tagList(
           box(
             title = "Inputs", 
             width = NULL,
             solidHeader = TRUE,
             status = status,
             numericInput(inputId = ns("num_wins"),
                          label = "Select Number of wins",
                          value = 0,
                          min = 0,
                          max = 82,
                          step = 1),
             numericInput(inputId = ns("num_losses"),
                          label = "Select Number of losses",
                          value = 0,
                          min = 0,
                          max = 82,
                          step = 1),
             numericInput(inputId = ns("current_pm"),
                          label = "Select Current plus minus differential",
                          value = 0,
                          min = -1000,
                          max = 100,
                          step = 1),
             numericInput(inputId = ns("accept_pm"),
                          label = "Keep records within X units of current plus minus",
                          value = 250,
                          min = 0,
                          max = 2000,
                          step = 1),
             actionButton(inputId = ns("run_check"),
                          label = "Extract Matching Records",
                          icon = icon('basketball-ball'))
           )
 
  )
}
    
#' sim_inputs Server Functions
#'
#' @noRd 
mod_sim_inputs_server <- function(id, data){
 
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    get_data <- eventReactive(input$run_check, {
      
      
      num_wins <- input$num_wins
      num_losses <- input$num_losses
      current_pm <- input$current_pm
      accept_pm <- input$accept_pm
      matching_data <- get_matching_records(data, 
                                                 num_wins,
                                                 num_losses,
                                                 current_pm - accept_pm,
                                                 current_pm + accept_pm)
      matching_data_sum <- matching_data %>%
        dplyr::select(id,
                      wins = final_wins,
                      losses = final_losses,
                      made_playoffs) %>%
        dplyr::group_by(season = id) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()
      
      return(list(matching_data = matching_data,
                  matching_data_sum = matching_data_sum))
    })
    
    return(get_data)
 
  })
}
    
## To be copied in the UI
# mod_sim_inputs_ui("sim_inputs_ui_1")
    
## To be copied in the server
# mod_sim_inputs_server("sim_inputs_ui_1")
