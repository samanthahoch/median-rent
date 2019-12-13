###################################
#                                 #
#   WILDFIRE EXPOSURE MONITOR UI  #         
#                                 #
###################################

##############
#HOUSEKEEPING#
##############

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(htmltools))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(shinythemes))
suppressPackageStartupMessages(library(data.table))

# setwd("Z:/Work/Coop/Fall_2019/median-rent")

appCSS <- "
/* Initial Loading Page */
#loading-content {
position: absolute;
background: #e0e0e0;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #383737;}"

####
#UI#
####

ui <- fluidPage(
  
  theme = "spacelab",
  title = "Median Rent Viewer",
  
  # loading screen
  useShinyjs(),
  inlineCSS(appCSS),
  div(
    id = 'loading-content',
    br(), br(), br(), br(), br(), br(), br(), br(),
    h3('Please Wait'),
    tags$img(src = "spinner.gif"),
    h5('Loading data...')
  ),
  
  tags$head(tags$style(
    HTML('
         #header {
         background-color: #032f5c;
         }
         #title {
         color: white;
         font-family: calibri; 
         }
         ')
  )),
  
  # interactive content
  div(
    id = 'app-content',
    fluidRow(
      
      # TITLE PANEL
      column(12, id = "header",
             column(9, br(), h1(id = "title", "Median Rent Viewer"), offset = 2))
      # column(1, tags$img(src = "house.png")))
      
    ), # end fluid row
    br(),
    fluidRow(
      column(width = 2, 
             
             # FILTER PANEL
             wellPanel(
               h4("Filter By"), 
               radioGroupButtons(inputId = "numRooms", label = "Number of bedrooms:",
                                 choiceValues = c("rent50_1", "rent50_2", "rent50_3", "rent50_4"),
                                 choiceNames = c(" 1 ", " 2 ", " 3 ", " 4 ")),
               h5(strong("Rent Range:")), 
               sliderInput(inputId = "rentScoreRange", label = NULL, min = 1, max = 5000,
                           value = c(1,1500), ticks = FALSE), 
               actionBttn(inputId = "update", label = "Update Filters", style = "unite",
                          color = "warning", icon = icon("sync", lib = "font-awesome"), size = "sm"), br()
             ),
             
             # VIEW PANEL
             wellPanel(
               h4("View By"), 
               radioButtons(inputId = "viewChoice", label = NULL,
                            choices = c("Current Rent", "Rent Growth")), 
               hidden(actionBttn(inputId = "selectPeriod", label = "Select Period", style = "unite",
                                 color = "default", icon = icon("calendar", lib = "font-awesome"), size = "sm")),
               br(),
               hidden(textOutput(outputId = "currPeriod")), br(), 
               actionBttn(inputId = "changeView", label = "Change View", style = "unite",
                          color = "warning", icon = icon("transfer", lib = "glyphicon"), size = "sm")
             ),
             
             # SELECT STATE PANEL
             wellPanel(
               h4("Select State"),
               selectInput(inputId = "selectState", label = "State Options",
                           choices = c("AZ", "CA", "CO", "ID", "MT", "NM", "NV", "OK", "OR", "TX", "UT", "WA", "WY"),
                           selected = "CA")
             )
      ), 
      
      # MAP PANEL
      column(width = 6,
             addSpinner(leafletOutput(outputId = "mymap", height = 750), spin = "bounce", color = "#dbdbdb")),
      
      # tables for selected counties
      column(width = 4,
             
             # TABLES PANEL
             wellPanel(tabsetPanel(id = "tableTabs",
                                   type = "tabs",
                                   tabPanel(div(icon("mouse-pointer"), "Selected Counties"),
                                            br(), h4("Selected Counties"), br(),
                                            h5("Most Recent Rent"),
                                            tableOutput(outputId = "mydata"), br(), 
                                            h5("History"), br(),
                                            DTOutput(outputId = "historicalData"),
                                            value = "Selected Counties"),
                                   tabPanel(div(icon("list-ol"), "Top Counties"),
                                            br(),
                                            splitLayout(cellWidths = c(325, 150),
                                                        div(h4("Highest Median Rent within Current Filters"),
                                                            h6("Click on rows in table to select/unselect on map")),
                                                        div(br(),
                                                            switchInput(inputId = "zoomSwitch", size = "small",
                                                                        label = "Zoom to Grid", value = TRUE))),
                                            DTOutput(outputId = "topCntyData"),
                                            value = "Top Counties"))
             ), # end tab panel
             
             # SELECTION PANEL
             wellPanel(
               splitLayout(
                 cellWidths = c(100, 100, 150),
                 h4("Selection:"),
                 actionBttn(inputId = "clearSelection", label = "Clear", color = "default", size = "sm",
                            style = "unite", icon = icon("minus-circle", lib = "font-awesome")),
                 downloadBttn(outputId = "downloadData", label = "Download", size = "sm")
               ) # end split layout
             )
      ) # end column
    ) # end fluid row
  ) # end div app content
) # end ui