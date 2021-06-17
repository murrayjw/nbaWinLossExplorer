#' value_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_value_box_ui <- function(id, width){
  ns <- NS(id)
  tagList(
    
    shinydashboard::valueBoxOutput(ns("statistic"), width = width)
    
  )
}

#' value_box Server Functions
#'
#' @noRd 
#' @import shinydashboard
mod_value_box_server <- function(id, type, data, color = "olive"){
  
  moduleServer( id, 
                function(input, output, session){
    ns <- session$ns
    
    if(type == "matching_records") {
      output$statistic <- shinydashboard::renderValueBox({
        df <- data()
        matching_data_sum <- df$matching_data_sum
        n <- dplyr::n_distinct(matching_data_sum$id)
        
        shinydashboard::valueBox(
          value = n,
          subtitle = "Matching Historical Records",
          icon = icon("thumbs-up", lib = "glyphicon"),
          color = color
        )
        
        # Add conditional colors
      })
    } else if(type == "wins") {
      
      
      output$statistic <- shinydashboard::renderValueBox({
        
        df <- data()
        matching_data_sum <- df$matching_data_sum
        
        mean_wins <- median(matching_data_sum$wins)
        q25_wins <- quantile(matching_data_sum$wins, .25)
        q75_wins <- quantile(matching_data_sum$wins, .75)
        
        win_value <- paste(mean_wins,
                           paste0("(", q25_wins, "-", q75_wins, ")"))
        shinydashboard::valueBox(
          value = win_value,
          subtitle = "Median wins in all matching seasons (IQR)",
          icon = icon("wine-bottle"),
          color = color
        )
      })
      
    } else if(type == "losses") {
      
      
      output$statistic <- shinydashboard::renderValueBox({
        
        df <- data()
        matching_data_sum <- df$matching_data_sum
        
        mean_wins <- median(matching_data_sum$losses)
        q25_wins <- quantile(matching_data_sum$losses, .25)
        q75_wins <- quantile(matching_data_sum$losses, .75)
        
        win_value <- paste(mean_wins,
                           paste0("(", q25_wins, "-", q75_wins, ")"))
        
        shinydashboard::valueBox(
          value = win_value,
          subtitle = "Median losses in all matching seasons (IQR)",
          icon = icon("window-close"),
          color = color
        )
      })
    } else if(type == "playoffs") {
      
      output$statistic <- shinydashboard::renderValueBox({
        
        df <- data()
        matching_data_sum <- df$matching_data_sum
        playoff_percentage <- sum(matching_data_sum$made_playoffs)/nrow(matching_data_sum)
        playoff_percentage <- paste0(round(100*playoff_percentage, 3), "%")
        
        
        
        shinydashboard::valueBox(
          value = playoff_percentage,
          subtitle = "Percentage of teams making the playoffs",
          icon = icon("play-circle"),
          color = color
        )
      })
      
    } else if(type == "best_case") {
      
      output$statistic <- shinydashboard::renderValueBox({
        
        df <- data()
        matching_data_sum <- df$matching_data_sum
        
        bt <- matching_data_sum %>% 
          arrange(desc(wins)) %>% 
          slice(1)
        
        txt <- glue::glue("{bt$id} with record of {bt$wins}-{bt$losses}")
        
        shinydashboard::valueBox(
          value = "Best Result",
          subtitle = txt,
          icon = icon("smile"),
          color = color
        )
      })
      
    } else if(type == "worst_case") {
      
      output$statistic <- shinydashboard::renderValueBox({
        
        df <- data()
        matching_data_sum <- df$matching_data_sum
        
        bt <- matching_data_sum %>% 
          arrange(desc(wins)) %>% 
          slice(dplyr::n())
        
        txt <- glue::glue("{bt$id} with record of {bt$wins}-{bt$losses}")
        
        print(bt)
        shinydashboard::valueBox(
          value = "Worst Result",
          subtitle = txt,
          icon = icon("sad-tear"),
          color = color
        )
      })
      
    }
    
  })
}

## To be copied in the UI
# mod_value_box_ui("value_box_ui_1")

## To be copied in the server
# mod_value_box_server("value_box_ui_1")
