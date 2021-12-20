# Current required packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyBS)
library(shinyjs)
library(shinyalert)
library(shinyWidgets)

library(readxl)
library(jsonlite)
library(readr)
library(XML)

library(data.table)
library(tidyr)
library(rhandsontable)

library(ggplot2)

library(colourpicker)

# Load other functions
source("functions.R", local = TRUE)

# Global tags

#### User Interface (UI) ####
ui <- shinyUI(
  dashboardPage(
    
    title = "FREEDOX v.2.0",
    
    header = dashboardHeader(
      title = "FREEDOX v.2.0"
    ),
    
    dashboardSidebar(
      # disable = TRUE,
      collapsed = FALSE,
      sidebarMenu( id = "tabs",
                   menuItem(
                     text="Profile",
                     tabName="freedoxProfile",
                     icon=icon("layer-group")
                   )
      )
    ),
    
    dashboardBody(
      
      shinyjs::useShinyjs(),
      
      tabItems(
        tabItem(tabName = "freedoxProfile",
                source("uiPR.R", local = TRUE)$value
                # tabPanel ( "Profile", fluid = TRUE,
                #            
                # )
        )
      ),
      
      # App Modals
      shinyBS::bsModal(
        id = "prfAddDataMdl",
        title = "Great! Let's record some data!",
        trigger = "prfAddFirst",
        p(style = 'font-size: 15px;',
          "Insert instruction here."
        ),
        fluidRow(
          column(6,
                 textInput("prfAddName", label = NULL, value = "species1")
          ),
          column(6,
                 actionButton("prfAddFinal", "Add Species", icon("wrench"), style="color: #000000; background-color: #F1BE48")
          )
        )
      )
    )# close body
  )
) 
#### End User Interface (UI) ####


##### Server ####
server <- function(input, output, session){
  
  # load Profile section server code
  source("serverPR.R", local = TRUE)$value
  
} 
#### End Server ####

# Initiate app by calling this function
shinyApp(ui=ui, server=server)
