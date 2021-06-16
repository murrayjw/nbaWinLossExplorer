#' datatable UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny NS tagList 
mod_datatable_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    dataTableOutput(ns('matching_table'))
    
  )
}
    
#' datatable Server Functions
#'
#' @noRd 
mod_datatable_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$matching_table <- renderDataTable({
      df <- data()
      matching_data_sum <- df$matching_data_sum
      
      DT::datatable(matching_data_sum, options = list(scrollX=T),
                    caption = "Summary of historical team performance")
      
    })
    
    
  })
}
    
## To be copied in the UI
# mod_datatable_ui("datatable_ui_1")
    
## To be copied in the server
# mod_datatable_server("datatable_ui_1")
