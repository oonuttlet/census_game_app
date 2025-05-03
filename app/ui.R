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
    actionButton("clearDraw", "Clear Drawing", class = "btn-warning",icon = icon("redo"), width = "87%"),
    hr(),
    actionButton("calculate", "Calculate Selection", class = "btn-success", width = "87%"),
     #div(style="align:center;",actionButton("newGame", "New Game", class = "btn-primary", width = "87%")),
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
    # fluidRow(
    #   box(
    #     #title = "Getting Started",
    #     title = tags$span("How to Play", style = "color: white; font-weight: bold;"),  
    #     status = "primary",
    #     solidHeader = TRUE,
    #     width = 12,
    #     #htmlOutput("instructions"),
    #     tags$div(HTML("
    #       <p style='margin-top: 12px;'><ol>
    #       <li>First, select a state and county from the the dropdown tabs on the left</li>
    #       <li>Click the 'Start Game' button to begin</li>
    #       <li>Draw a shape using one of the icons on the left-side of map to select an area. 
    #       Make sure the last point connects to the point to finalize your selection</li>
    #       <li>Click the 'Calculate Selection' button to see how close your selection is to the target population</li>
    #       </ol>
    #       </p>
    #     "))
    #   )
    # ),
    fluidRow(
      box(
        title = "Selection Map",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
                tags$div(HTML("
          <p style='margin-top: 12px;'><h4><strong>How to Play:</strong></h4>
            <ol>
              <li>First, select a <strong>State</strong> and <strong>County</strong> from the the dropdown tabs on the left</li>
              <li>Click the <strong>Start Game</strong> button to begin</li>
              <li>Draw a shape using one of the icons on the left-side of map to select an area. 
              Make sure the last point connects to the point to finalize your selection</li>
              <li>Click the <strong>Calculate Selection</strong> button to see how close your selection is to the target population</li>
            </ol>
          </p>
        ")),
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