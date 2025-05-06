library(shiny)
library(sf)
library(tidycensus)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(shinydashboard)
library(shinyjs)
library(jsonlite)

server <- function(input, output, session) {
  # Initialize state and county data
  states <- unique(tigris::fips_codes$state_name)[1:51]
  counties <- tigris::fips_codes[tigris::fips_codes$state_name %in% states, c("state_name", "county")]
  
  # Dynamic county selection based on state
  output$countySelection <- renderUI({
    req(input$state)  # Only proceed if state is selected
    
    # Filter counties based on selected state
    filtered_counties <- counties[counties$state_name == input$state, ]
    
    selectInput("county", "County", 
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
                  choices = filtered_counties_rand$county,
                  selected = c(sample(filtered_counties_rand$county, 1)))
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
    output$map <- renderMaplibre({
      maplibre(attributionControl = FALSE,
               center = c(-95.7129, 37.0902),
               zoom = 4)
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
        output$map <- renderMaplibre({
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
      return("Select a drawing icon (square box) from the left side of the map, then draw a polygon to select an area.")
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
    county_coords <- st_coordinates(county_centroid) |> c()
    
    maplibre() |>
      fit_bounds(st_buffer(values$census_data, 10000),
                 animate = TRUE) |>
      add_fill_layer(id = "county_fill",
        source = summarize(values$census_data),
        fill_color = "white",
        fill_opacity = 0.5,
        tooltip = input$county
      ) |>
      add_line_layer(id = "county_boundary",
                     source = summarize(values$census_data),
                     line_width = 3,
                     line_cap = "round") |>
      add_draw_control(id = "drawn_features",
                       displayControlsDefault = FALSE, 
                       controls = list("polygon" = TRUE,
                                       "trash" = TRUE),
                       freehand = FALSE,
                       position = "top-left",
                       orientation = "horizontal"
      )
  }
  
  # Render the initial map
  output$map <- renderMaplibre({
    if (is.null(values$census_data)) {
      maplibre(attributionControl = FALSE,
               center = c(-95.7129, 37.0902),
               zoom = 3)
    } else {
      createMap()
    }
  })
  
  # Store drawn shapes when they're created/edited
  observeEvent(input$map_drawn_features, {
    # Convert the JSON string to an sf object
    drawn_json <- input$map_drawn_features
    
    # Parse the JSON into an sf object
    if(!is.null(drawn_json) && drawn_json != "") {
      # Convert JSON string to sf object
      drawn_sf <- sf::st_read(drawn_json, quiet = TRUE)
      
      # Store the sf object
      values$drawn_shapes <- drawn_sf
      
      # Print for debugging
      print("Converted to SF object:")
      print(values$drawn_shapes)
    }
  })
  
  # Clear drawn shapes when they're deleted
  observeEvent(input$map_drawn_features, {
    # If the JSON is empty or NULL, clear the shapes
    if(is.null(input$map_drawn_features) || input$map_drawn_features == "" ||
       input$map_drawn_features == "{\"type\":\"FeatureCollection\",\"features\":[]}") {
      values$drawn_shapes <- NULL
      values$results <- NULL
      print("Shapes cleared")
    }
  })
  
  # Create sf object from leaflet drawing output
  createSfFromDrawing <- function(drawn_feature) {
    tryCatch({
      # Extract coordinates
      shape = get_drawn_features(maplibre_proxy("map"))
      
      # For polygons, extract the coordinates and create an sf object
      if (drawn_feature$geometry$type == "Polygon") {
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
    drawn_sf <- values$drawn_shapes
    
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
    
    print(values$results)
    
    if (values$results$accuracy >= 90) {
      message <- "Excellent job!"
    } else if (values$results$accuracy >= 70) {
      message <- "Good work!"
    } else if (values$results$accuracy >= 50) {
      message <- "Not bad!"
    } else {
      message <- "Keep trying!"
    }
  })
  
  observe({
    req(values$results)  # Ensure results exist before triggering map update
    
    bins <- quantile(values$census_data$estimate, na.rm = TRUE)
    pal <- RColorBrewer::brewer.pal(5, "YlOrRd")
    
    maplibre_proxy("map") |>
      clear_controls() |>
      clear_layer("county_fill") |>
      add_fill_layer(
        id = "result_tracts",
        source = values$census_data,
        fill_color = interpolate(
          column = "estimate",
          stops = c("#fee8c8", "#7f0000"),
          values = c(min(values$census_data$estimate), max(values$census_data$estimate))
        ),
        fill_opacity = 0.5
      ) |>
      add_fill_layer(
        id = "result_drawing",
        source = values$drawn_shapes,
        fill_color = "#f5f5f5",
        fill_opacity = 0.3
      ) |>
      add_legend(
        type = "categorical",
        legend_title = stringr::str_to_sentence(values$plaintext),
        colors = pal,
        values = bins,
        position = "top-right"
      ) |>
      fit_bounds(
        st_buffer(values$census_data, 10000),
        animate = TRUE
      )
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