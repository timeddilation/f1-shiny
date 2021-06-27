server <- function(input, output, session){
  update_lap_time_race_driver <- function(circuit_name, season){
    drivers_in_race <- race_drivers(circuit_name, season)
    
    updateSelectInput(
      inputId = "lap_time_race_driver",
      choices = drivers_in_race,
      selected = drivers_in_race[1]
    )
  }
  ### Update Input Options based on circuit/season selections
  observeEvent(input$lap_time_circuit, {
    circuit_seasons <- available_circuit_seasons(input$lap_time_circuit)
    
    updateSliderTextInput(
      session = session,
      inputId = "lap_time_season",
      choices = circuit_seasons,
      selected = circuit_seasons[1]
    )
    
    update_lap_time_race_driver(input$lap_time_circuit, circuit_seasons[1])
  }, priority = 1)
  
  observeEvent(input$lap_time_season, {
    update_lap_time_race_driver(input$lap_time_circuit, input$lap_time_season)
  }, priority = 1)
  ### reactive data based on user inputs
  lap_time_circuit_races_uncut <- reactive({
    selected_circuit <- circuits[name == input$lap_time_circuit, circuitRef]
    sample_circuit_races <- lap_times_tidy[circuitRef == selected_circuit]
    return(sample_circuit_races)
  })
  
  lap_time_circuit_races <- reactive({
    sample_circuit_races <- lap_time_circuit_races_uncut()
    
    circuit_races <- suppressWarnings(split(
      sample_circuit_races,
      factor(unique(sample_circuit_races[, year]))
    ))
    
    eval_circuit <- do.call(
      rbind,
      lapply(circuit_races, remove_outlier_times)
    )
    
    eval_circuit[, year := factor(year)]
    
    return(eval_circuit)
  })
  
  lap_time_circuit_race <- reactive({
    # needs to handle when switching to a circuit, before season input updates
    # to not select a season that does not exist
    all_circuit_races <- lap_time_circuit_races()
    
    if(nrow(all_circuit_races[year == input$lap_time_season]) >= 1){
      return(all_circuit_races[year == input$lap_time_season])
    } else {
      return(all_circuit_races[year == unique(all_circuit_races[, year][1])])
    }
  })
  
  lap_time_circuit_lims <- reactive({
    buffer_time <- 3000
    all_laps <- lap_time_circuit_races()
    
    min_time <- min(all_laps[, milliseconds], na.rm = T) - buffer_time
    max_time <- max(all_laps[, milliseconds], na.rm = T) + buffer_time
    
    return(c(min_time, max_time))
  })
  ### graph outputs
  output$lap_time_circuit_seasons_violen <- renderPlot({
    ggplot(
      lap_time_circuit_races(),
      aes(
        x = milliseconds,
        y = year,
        fill = year
      )
    ) +
      geom_violin() +
      scale_fill_viridis(
        discrete = TRUE,
        alpha = 0.6,
        option = "H"
      ) +
      scale_x_continuous(
        labels = convert_ms_to_time
      ) +
      labs(
        title = "Lap Time Distribution"
      ) +
      theme_clean() +
      theme(
        plot.background = element_blank(),
        legend.position = "none",
        axis.title = element_blank()
      )
  })
  
  output$lap_time_circuit_race_density <- renderPlot({
    # throws warning when drivers have 2 or less laps
    # "Warning: Groups with fewer than two data points have been dropped."
    # this message cannot be suppressed
    ggplot(
      lap_time_circuit_race(), 
      aes(
        x = milliseconds
      )
    ) +
      geom_density(
        aes(color = "red2", fill = "red2", alpha = 0.5)
      ) +
      scale_x_continuous(
        labels = convert_ms_to_time,
        limits = lap_time_circuit_lims()
      ) +
      scale_y_continuous(
        position = "right"
      ) +
      labs(
        title = "Lap Times Density",
        subtitle = "All Drivers"
      ) +
      ylab("Density") +
      theme_clean() +
      theme(
        plot.background = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.y = element_blank()
      )
  }, bg = "transparent")
  
  output$lap_time_circtuit_race_driver_times_violen <- renderPlot({
    selected_race <- lap_time_circuit_race()
    selected_race_id <- unique(selected_race[, raceId])

    race_driver_order <- merge(
      results[raceId == selected_race_id, .(driverId, positionOrder)],
      drivers[, .(driverId, Driver)],
      by = "driverId"
    )[order(positionOrder)][, Driver]

    race_driver_order <- factor(race_driver_order, levels = race_driver_order)

    ggplot(
      selected_race[, .(Driver, milliseconds)],
      aes(
        x = milliseconds,
        y = Driver,
        fill = Driver
      )
    ) +
      geom_violin(na.rm = T) +
      scale_fill_viridis(
        limits = rev(levels(race_driver_order)), # gradiants colors based on pos
        discrete = TRUE,
        alpha = 0.6,
        option = "H"
      ) +
      scale_x_continuous(
        labels = convert_ms_to_time,
        limits = lap_time_circuit_lims()
      ) +
      scale_y_discrete(
        limits = rev(levels(race_driver_order)), # orders drivers from 1st-last
        position = "right"
      ) +
      labs(
        title = "Drivers' Lap Times Density",
        subtitle = "Ordered By Driver Finishing Position"
      ) +
      theme_clean() +
      theme(
        plot.background = element_blank(),
        legend.position = "none",
        axis.title = element_blank()
      )
  }, bg = "transparent")
  
  output$lap_time_circuit_race_drivers_best <- renderTable({
    top_times <- lap_time_circuit_race()[, .(milliseconds = min(milliseconds)), 
                                         by = "Driver"]
    top_times[, time := convert_ms_to_time(milliseconds)]
    return(top_times[order(milliseconds)][, .(Driver, Time = time)])
  })
  ### drivers tab
  output$lap_time_race_driver_times <- renderPlotly({
    selected_race <- lap_time_circuit_races_uncut()
    default_year <- lap_time_circuit_races_uncut()[, year][1]
    if(nrow(selected_race[year == input$lap_time_season]) > 0){
      selected_race <- selected_race[year == input$lap_time_season]
    } else {
      selected_race <- selected_race[year == default_year]
    }
    
    min_time <- min(selected_race[, milliseconds]) - 5000
    max_time <- max(selected_race[, milliseconds]) + 1000
    
    total_laps <- factor(1:max(selected_race[, lap]))
    # driver may update late after season input change
    default_driver <- selected_race[, Driver][1]
    if(nrow(selected_race[Driver == input$lap_time_race_driver]) > 0){
      driver_laps <- selected_race[Driver == input$lap_time_race_driver]
    } else {
      driver_laps <- selected_race[Driver == default_driver]
    }
    
    driver_laps[, lap := factor(lap, levels = total_laps)]
    
    gg <- 
      ggplot(
        driver_laps,
        aes(
          x = lap,
          y = milliseconds,
          time = time
        )
      ) +
      geom_bar(
        stat = "identity",
        fill = "#ff1801",
        color = "black"
      ) +
      scale_y_continuous(
        labels = convert_ms_to_time
      ) +
      scale_x_discrete(
        limits = rev(levels(driver_laps[, lap]))
      ) +
      ylab("Time") +
      xlab("Lap") +
      theme_clean() +
      theme(
        plot.background = element_blank()
      ) +
      coord_flip(ylim = c(min_time, max_time))
    
    return(ggplotly(gg))
  })
  ### Info boxes for Driver Stats
  lap_time_season_results <- eventReactive(input$lap_time_season, {
    race_id <- raceId_by_circuit_season(input$lap_time_circuit, input$lap_time_season)
    
    return(results[raceId == race_id])
  })
  
  lap_time_driver_results <- reactive({
    driver_id <- drivers[Driver == input$lap_time_race_driver, driverId]
    
    if (driver_id %in% lap_time_season_results()[, driverId]){
      driver_results <- lap_time_season_results()[driverId == driver_id]
    } else {
      driver_results <- lap_time_season_results()[1]
    }
    
    return(driver_results)
  })
  
  output$lap_time_driver_start_pos <- renderValueBox({
    valueBox(
      ordinal(lap_time_driver_results()[, grid]),
      "Starting Position",
      color = "teal"
    )
  })
  
  output$lap_time_driver_finish_pos <- renderValueBox({
    driver_results <- lap_time_driver_results()
    driver_stat <- ""
    
    if(driver_results[, position == "\\N"]) {
      driver_stat <- driver_stat <- " / DNF"
    }
    
    valueBox(
      paste0(
        ordinal(lap_time_driver_results()[, positionOrder]),
        driver_stat
      ),
      "Finishing Position",
      color = "green"
    )
  })
  
  output$lap_time_driver_fastest_lap_time <- renderValueBox({
    fastest_lap <- driver_fastest_lap(
      race_id = lap_time_driver_results()[, raceId],
      driver_id = lap_time_driver_results()[, driverId]
    )
    # TODO: Investigate why some scenarios throw an error here
    # test cases: Albert Park 2002; Monza 2000 (but not when seeking backwards? only when going from 1999->2000)
    valueBox(
      fastest_lap[, time],
      paste0(
        "Fastest Lap Time: #",
        fastest_lap[, lap]
      ),
      color = "purple"
    )
  })
  
  output$lap_time_drive_debug <- renderValueBox({
    valueBox(
      lap_time_driver_results()[, raceId],
      lap_time_driver_results()[, driverId],
      color = "purple"
    )
  })
  
}