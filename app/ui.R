# Baltimore Census Population Selection Game
# This Shiny app allows users to draw custom polygons to select areas in Baltimore City
# to match a target population value

library(shiny)
library(sf)
library(tidycensus)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(shinydashboard)
library(shinyjs)
library(jsonlite)
library(bslib)

ui <- dashboardPage( #text

  dashboardHeader(title = "Census Population Selection Game"),
  dashboardSidebar(
    tags$head(
      tags$style(HTML("
        .shiny-input-container {
          text-align: center;
          margin-bottom: 10px;
        }
      "))
    ),
    tags$head(
      tags$style(HTML("
        .btn-primary {
          background-color: #007bff;
          border-color: #007bff;
        }
        .btn-primary:hover {
          background-color: #0056b3;
          border-color: #0056b3;
        }
      "))
    ),

    selectInput("state", "State:", 
                choices = c("Choose a state" = "", unique(tigris::fips_codes$state_name)[1:51])),
    uiOutput("countySelection"),  # This will be rendered by the server
    hr(),
      layout_column_wrap(width = 1/2,
        actionButton("rand_button", "Randomize", class = "btn-primary",
              style = "font-weight: bold; color: white;"),
        actionButton("clr_button", "Reset", class = "btn-danger",
                  style = "font-weight: bold; color: white;")),
      
      layout_column_wrap(
        actionButton("go_button", "Start Game", class = "btn-success",
                  style = "font-weight: bold; color: white;")), 
   
    hr(),
    selectInput("variable", "Census Variable:",
                choices = c("Total Population" = "B01001_001"),
                selected = "B01001_001"),
    numericInput("year", "Census Year:", 2020, min = 2010, max = 2023),
    actionButton("clearDraw", "Clear Drawing", class = "btn-warning", width = "87%"),
    hr(),
    actionButton("calculate", "Calculate Selection", class = "btn-success", width = "87%"),
     div(style="align:center;",actionButton("newGame", "New Game", class = "btn-primary", width = "87%")),
    useShinyjs()
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
      .leaflet-draw-actions {
        display: none !important;
      }
    "))
    ),
    fluidRow(
      box(
        title = "Instructions",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        htmlOutput("instructions"),
        tags$div(HTML("
          <p style='margin-top: 10px;'><strong>How to draw:</strong> Use the tools in the upper left of the map to draw a polygon. 
          When finished, click 'Calculate Selection'.</p>
        "))
      )
    ),
    fluidRow(
      box(
        title = "Selection Map",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        leafletOutput("map", height = 500)
      )
    ),
    fluidRow(
      box(
        title = "Results",
        status = "info",
        solidHeader = TRUE,
        width = 6,
        verbatimTextOutput("results")
      ),
      box(
        title = "Performance",
        status = "warning",
        solidHeader = TRUE,
        width = 6,
        verbatimTextOutput("performance")
      )
    )
  )
)