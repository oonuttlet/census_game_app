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

ui <- dashboardPage(
  dashboardHeader(title = "Census Population Selection Game"),
  dashboardSidebar(
    selectInput("state", "State:", 
                choices = c("Choose a state" = "", unique(tigris::fips_codes$state_name)[1:51])),
    uiOutput("countySelection"),  # This will be rendered by the server
    hr(),
    fluidRow(
      column(1,
             actionButton("rand_button", "Randomize", class = "btn-primary")),
      column(1,
             actionButton("clr_button", "Clear", class = "btn-primary"),
             offset = 5),
    ),
    hr(),
    actionButton("go_button", "Load Data", class = "btn-primary", width = "87%"),
    hr(),
    div(style="align:center;",actionButton("newGame", "New Game", class = "btn-primary", width = "87%")),
    hr(),
    selectInput("variable", "Census Variable:",
                choices = c("Total Population" = "B01001_001"),
                selected = "B01001_001"),
    numericInput("year", "Census Year:", 2020, min = 2010, max = 2023),
    actionButton("clearDraw", "Clear Drawing", class = "btn-warning", width = "87%"),
    hr(),
    actionButton("calculate", "Calculate Selection", class = "btn-success", width = "87%"),
    useShinyjs()
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Instructions",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        htmlOutput("instructions"),
        tags$div(HTML("
          <p style='margin-top: 10px;'><strong>How to draw:</strong> Use the tools in the upper right of the map to draw a polygon. 
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

server <- function(input, output, session) {
  # Initialize state and county data
  states <- unique(tigris::fips_codes$state_name)[1:51]
  counties <- tigris::fips_codes[tigris::fips_codes$state_name %in% states, c("state_name", "county")]
  
  # Dynamic county selection based on state
  output$countySelection <- renderUI({
    req(input$state)  # Only proceed if state is selected
    
    # Filter counties based on selected state
    filtered_counties <- counties[counties$state_name == input$state, ]
    
    selectInput("county", "County:", 
                choices = c("Choose a county" = "", filtered_counties$county))
  })
  
  # Store game data
  values <- reactiveValues(
    census_data = NULL,
    target = NULL,
    drawn_shapes = NULL,
    game_active = FALSE,
    results = NULL,
    plaintext = NULL
  )
  
  # Initialize the game when data is loaded
  # This only runs after user selects state, county, and clicks "Load Data"
  observeEvent(input$go_button, {
    req(input$state, input$county) # Ensure both inputs are available
    
    # Now we'll start the game with the selected state and county
    startNewGame()
  })
  
  observeEvent(input$rand_button, {
    selected_state <- sample(states, 1)
    updateSelectInput(session, "state", selected = selected_state)
    
    # Use the sampled state instead of input$state
    filtered_counties_rand <- counties[counties$state_name == selected_state, ]
    output$countySelection <- renderUI({
      req(input$state)  # Only proceed if state is selected
      
      selectInput("county", "County:", 
                  choices = c(sample(filtered_counties_rand$county, 1)))
    }) 
    # Ensure both inputs are available
    
    # Now we'll start the game with the selected state and county
  })
  
  # Start a new game when button is pressed
  observeEvent(input$newGame, {
    # Reset state and county selections
    updateSelectInput(session, "state", selected = "")
    updateSelectInput(session, "county", selected = "")
    
    # Reset game state values
    values$drawn_shapes <- NULL
    values$results <- NULL
    values$census_data <- NULL
    values$target <- NULL
    values$game_active <- FALSE
    
    # Reset map to default view
    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -95.7129, lat = 37.0902, zoom = 4) %>%  # USA coordinates
        addControl(
          html = "<h4>Select a state and county, then click 'Load Data'</h4>",
          position = "topright"
        )
    })
  })
  
  # Function to start a new game
  startNewGame <- function() {
    # Reset values
    values$drawn_shapes <- NULL
    values$results <- NULL
    
    # Get census data
    withProgress(message = "Fetching census data...", {
      # Get plaintext description for the selected variable
      plaintext <- switch(input$variable,
                          "B01001_001" = "total population",
                          "B01002_001" = "median age",
                          "B19013_001" = "median household income")
      values$plaintext <- plaintext
      
      # Get FIPS codes for the selected state and county
      state_fips <- counties$state[counties$state_name == input$state][1]
      county_fips <- counties$county[counties$state_name == input$state & 
                                       counties$county == input$county][1]
      
      # Fetch census data
      tryCatch({
        census_dat <- get_acs(geography = "tract",
                              state = state_fips,
                              county = county_fips,
                              variables = input$variable,
                              year = input$year,
                              geometry = TRUE,
                              moe_level = 90)
        
        values$census_data <- census_dat
        
        # Calculate target based on percentile range
        pctl_min <- sum(census_dat$estimate[percent_rank(census_dat$estimate) <= 0.25])
        pctl_max <- sum(census_dat$estimate[percent_rank(census_dat$estimate) <= 0.90])
        target <- runif(1, pctl_min, pctl_max) |> round()
        values$target <- target
        
        # Set game as active
        values$game_active <- TRUE
        
        # Create a new map
        output$map <- renderLeaflet({
          createMap()
        })
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
        values$game_active <- FALSE
      })
    })
  }
  
  # Disable/enable buttons based on state
  observe({
    if (is.null(values$census_data)) {
      shinyjs::disable("newGame")
      shinyjs::disable("clearDraw")
      shinyjs::disable("calculate")
    } else {
      shinyjs::enable("newGame")
      shinyjs::enable("clearDraw")
      shinyjs::enable("calculate")
    }
  })
  
  # Instructions output
  output$instructions <- renderText({
    if (is.null(values$target) || is.null(values$plaintext)) {
      return("Select a state and county, then click 'Load Data' to begin.")
    }
    
    HTML(paste0("Draw a polygon in ", input$county, ", ", input$state, 
           ", where the ", values$plaintext, " was approximately <b>", 
           format(values$target, big.mark = ","), "</b> in ",input$year,"."))
  })
  
  # Create the map
  createMap <- function() {
    req(values$census_data)
    
    # Find the centroid of the county for the map view
    county_centroid <- st_centroid(st_union(values$census_data))
    county_coords <- st_coordinates(county_centroid)
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = county_coords[1], lat = county_coords[2], zoom = 9) %>%
      addPolygons(
        data = summarize(values$census_data),
        fillColor = "white",
        color = "#444444",
        weight = 1,
        opacity = 0.7,
        fillOpacity = 0.5,
        label = ~paste0(input$county)
      ) %>%
      addDrawToolbar(
        targetGroup = "drawn_features",
        polylineOptions = FALSE,
        markerOptions = FALSE,
        rectangleOptions = TRUE,
        circleOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(
          edit = FALSE,
          remove = TRUE
        )
      )
  }
  
  # Render the initial map
  output$map <- renderLeaflet({
    if (is.null(values$census_data)) {
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -95.7129, lat = 37.0902, zoom = 4) %>%  # USA coordinates
        addControl(
          html = "<h4>Select a state and county, then click 'Load Data'</h4>",
          position = "topright"
        )
    } else {
      createMap()
    }
  })
  
  # Store drawn shapes when they're created
  observeEvent(input$map_draw_new_feature, {
    values$drawn_shapes <- input$map_draw_new_feature
  })
  
  # Store drawn shapes when they're edited
  observeEvent(input$map_draw_edited_features, {
    values$drawn_shapes <- input$map_draw_edited_features
  })
  
  # Clear drawn shapes when they're deleted
  observeEvent(input$map_draw_deleted_features, {
    values$drawn_shapes <- NULL
    values$results <- NULL
  })
  
  # Clear drawing button handler
  observeEvent(input$clearDraw, {
    leafletProxy("map") %>%
      clearGroup("drawn_features")
    
    values$drawn_shapes <- NULL
    values$results <- NULL
  })
  
  # Create sf object from leaflet drawing output
  createSfFromDrawing <- function(drawn_feature) {
    tryCatch({
      # Extract coordinates
      coords <- drawn_feature$geometry$coordinates
      
      # For polygons, extract the coordinates and create an sf object
      if (drawn_feature$geometry$type == "Polygon") {
        # First, make sure we have proper coordinates
        polygon_coords <- lapply(coords[[1]], function(pair) {
          c(as.numeric(pair[1]), as.numeric(pair[2]))
        })
        
        # Create a matrix of coordinates
        coords_matrix <- do.call(rbind, polygon_coords)
        
        # Create an sf polygon
        polygon_sf <- st_polygon(list(coords_matrix))
        
        # Create sf object
        sf_obj <- st_sf(geometry = st_sfc(polygon_sf, crs = 4326))
        
        return(sf_obj)
      } else {
        showNotification("Only polygon drawings are supported.", type = "warning")
        return(NULL)
      }
    }, error = function(e) {
      showNotification(paste("Error processing drawing:", e$message), type = "error")
      return(NULL)
    })
  }
  
  # Calculate button handler
  observeEvent(input$calculate, {
    if (is.null(values$drawn_shapes)) {
      showNotification("Please draw a shape on the map first.", type = "warning")
      return()
    }
    
    # Create sf object from the drawn shape
    drawn_sf <- createSfFromDrawing(values$drawn_shapes)
    
    if (is.null(drawn_sf)) {
      return()  # Error already shown in createSfFromDrawing
    }
    
    # Transform to match CRS of census data
    drawn_sf <- st_transform(drawn_sf, st_crs(values$census_data)) %>%
      st_make_valid()
    
    # Perform spatial intersection with census tracts
    census_selection <- st_intersection(values$census_data, drawn_sf)
    
    if (nrow(census_selection) == 0) {
      showNotification("Your selection doesn't overlap with any census tracts.", type = "warning")
      return()
    }
    
    # Calculate the area proportion for each tract (for partial selections)
    original_areas <- values$census_data %>%
      filter(GEOID %in% census_selection$GEOID) %>%
      mutate(original_area = as.numeric(st_area(.))) %>%
      select(GEOID, original_area) %>%
      st_drop_geometry()
    
    # Calculate the intersected areas
    census_selection <- census_selection %>%
      mutate(intersected_area = as.numeric(st_area(.))) %>%
      left_join(original_areas, by = "GEOID") %>%
      mutate(
        proportion = intersected_area / original_area,
        # Adjust estimates based on proportion of area selected
        adjusted_estimate = estimate * proportion,
        adjusted_moe = moe * proportion
      )
    
    # Sum up the adjusted estimates
    selection_estimate <- sum(census_selection$adjusted_estimate, na.rm = TRUE)
    selection_moe <- sqrt(sum(census_selection$adjusted_moe^2, na.rm = TRUE))
    
    selection_max <- selection_estimate + round(selection_moe)
    selection_min <- selection_estimate - round(selection_moe)
    
    # Calculate performance metrics
    target <- values$target
    # Avoid division by zero
    if (target == 0) {
      accuracy <- ifelse(selection_estimate == 0, 100, 0)
    } else {
      accuracy <- round(100 * (1 - abs(selection_estimate - target) / target), 1)
    }
    accuracy <- max(0, accuracy)  # Ensure accuracy isn't negative
    
    # Store results
    values$results <- list(
      estimate = selection_estimate,
      min = selection_min,
      max = selection_max,
      target = target,
      accuracy = accuracy,
      num_tracts = length(unique(census_selection$GEOID))
    )
    
    if (values$results$accuracy >= 90) {
      message <- "Excellent job!"
    } else if (values$results$accuracy >= 70) {
      message <- "Good work!"
    } else if (values$results$accuracy >= 50) {
      message <- "Not bad!"
    } else {
      message <- "Keep trying!"
    }
    
    bins <- quantile(values$census_data$estimate)
    pal <- colorBin("YlOrRd", domain = values$census_data$estimate, bins = bins)
    
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = values$census_data,
                  fillColor = ~pal(estimate),
                  fillOpacity = 0.5,
                  color = "#444444",
                  weight = 1) %>%
      addPolygons(data = drawn_sf,
                  fillColor = "#bdbdbd",
                  fillOpacity = 0.3,
                  color = "#444444",
                  weight = 2)  %>%  # USA coordinates
      addControl(
        html = paste0("Target: <strong>", format(values$results$target, big.mark = ","), 
               "</strong><br>Your selection: <strong>", format(round(values$results$estimate), big.mark = ","),
               "</strong><br>Difference: <strong>", format(round(abs(values$results$estimate - values$results$target)), big.mark = ","),
               "</strong><br>Accuracy: <strong>", values$results$accuracy, "%</strong>",
               "<br>", message),
        position = "topright"
      ) %>%
      addLegend(pal = pal,
                values = values$census_data$estimate,
                opacity = 0.7,
                title = "Estimate",
                position = "bottomright")
  })
  
  # Results output
  output$results <- renderText({
    if (is.null(values$results)) {
      if (is.null(values$census_data)) {
        return("Select a state and county, then click 'Load Data'")
      } else {
        return("Draw a polygon and click 'Calculate Selection' to see results")
      }
    }
    
    paste0("Your selection's ", values$plaintext, " is ", 
           format(round(values$results$estimate), big.mark = ","), 
           ", with a margin of error of [", 
           format(round(values$results$min), big.mark = ","), ", ", 
           format(round(values$results$max), big.mark = ","), "].",
           "\n\nNumber of tracts intersected: ", values$results$num_tracts)
  })
  
  # Performance output
  output$performance <- renderText({
    if (is.null(values$results)) {
      if (is.null(values$census_data)) {
        return("Waiting for data...")
      } else {
        return("Waiting for your selection...")
      }
    }
    
    if (values$results$accuracy >= 90) {
      message <- "Excellent job!"
    } else if (values$results$accuracy >= 70) {
      message <- "Good work!"
    } else if (values$results$accuracy >= 50) {
      message <- "Not bad!"
    } else {
      message <- "Keep trying!"
    }
    
    paste0("Target: ", format(values$results$target, big.mark = ","), 
           "\nYour selection: ", format(round(values$results$estimate), big.mark = ","),
           "\nDifference: ", format(round(abs(values$results$estimate - values$results$target)), big.mark = ","),
           "\nAccuracy: ", values$results$accuracy, "%",
           "\n", message)
  })
  

}

shinyApp(ui = ui, server = server)