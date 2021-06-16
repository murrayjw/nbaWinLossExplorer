#' plotly_display UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import ggplot2
#' @import plotly
#' 
mod_plotly_display_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    plotlyOutput(ns('plotly'))
    
  )
}
    
#' plotly_display Server Functions
#'
#' @noRd 
mod_plotly_display_server <- function(id, data){
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$plotly <- renderPlotly({
      
        df <- data()
        matching_data <- df$matching_data
        
        p1 <- matching_data %>%
          ggplot(aes(number_game_team_season, wins, color = id)) +
          geom_line() +
          labs(x = "Game number of season",
               y = "Number of wins")  +
          theme_minimal()+
          theme(legend.position='none') +
          theme(axis.text.x = element_text(colour = "#006bb8"),
                axis.text.y = element_text(colour = "#006bb8"),
                axis.title.x = element_text(colour = "#032f4f"),
                axis.title.y = element_text(colour = "#032f4f"))
        
        p1 <- plotly::ggplotly(p1)
    
        return(p1)
    })
  })
}
    
## To be copied in the UI
# mod_plotly_display_ui("plotly_display_ui_1")
    
## To be copied in the server
# mod_plotly_display_server("plotly_display_ui_1")
