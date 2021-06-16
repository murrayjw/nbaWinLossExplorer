#' standings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @import reactable
#' @importFrom shiny NS tagList 
mod_standings_ui <- function(id, name){
  ns <- NS(id)
  tagList(
   
  reactable::reactableOutput(ns('standings'))        
 
  )
}
    
#' standings Server Functions
#' @import ggplot2
#' @noRd 
mod_standings_server <- function(input, output, session, data, type){
    ns <- session$ns
    
    
    if(type == "eastern") {
      
     
      output$standings <- reactable::renderReactable({
        
        create_standings_table(data$eastern, select = 'single')
        
      })
      
    } else if(type=="western"){
      
      output$standings <- reactable::renderReactable({
        
        create_standings_table(data$western, select = 'single')
        
      })
      
    }
}
    
   
    
## To be copied in the UI
# mod_standings_ui("standings_ui_1")
    
## To be copied in the server
# mod_standings_server("standings_ui_1")
