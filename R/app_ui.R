#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    dashboardPage(
      
      # Header page
      dashboardHeader(title = "NBA Win/Loss Record Explorer"),
      
      # Sidebar page
      dashboardSidebar(
        sidebarMenu(
          menuItem("Current NBA record", tabName = "current", icon = icon("dashboard")),
          menuItem("Historical Record Explorer", icon = icon("th"),
                   tabName = "historical", badgeColor = "green")
        )
      ),
      
      dashboardBody(
        tabItems(
          tabItem(tabName = "current",
                  fluidRow(
                    tabBox(width = 10,
                           title = "Standings",
                           id = "tabset1",
                           tabPanel("Eastern Conference Standings",
                            mod_standings_ui("eastern_ui")
                           ),
                           tabPanel("Western Conference Standings",
                                    mod_standings_ui("western_ui")
                           )
                    )
                  )),
          
          tabItem(tabName = "historical",
                  h2("Historical Records")
          )
        )
      )
      
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'nbaWinLossExplorer'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

