server <- function(input, output, session){
  updateSelectInput(
    inputId = "lap_time_circuit",
    choices = circuits_with_times,
    selected = default_circuit
  )
  
  updateSliderInput(
    inputId = "lap_time_season",
    min = min(lap_times_tidy[name == default_circuit, year]),
    max = max(lap_times_tidy[name == default_circuit, year]),
  )
  
  observeEvent(input$lap_time_circuit, {
    updateSliderInput(
      inputId = "lap_time_season",
      min = min(lap_times_tidy[name == input$lap_time_circuit, year]),
      max = max(lap_times_tidy[name == input$lap_time_circuit, year]),
      value = min(lap_times_tidy[name == input$lap_time_circuit, year])
    )
  })
  
  lap_time_circuit_races <- reactive({
    selected_circuit <- circuits[name == input$lap_time_circuit, circuitRef]
    
    sample_circuit_races <- lap_times_tidy[circuitRef == selected_circuit]
    
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
    lap_time_circuit_races()[year == input$lap_time_season]
  })
  
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
        labels = convert_ms_to_time
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
  })
  
  output$lap_time_circtuit_race_driver_times_violen <- renderPlot({
    selected_race <- lap_time_circuit_race()
    selected_race_id <- unique(selected_race[, raceId])
    # TODO: bug here when there are 2 drivers with same name, cannot factor
    # spa 2005, for example
    race_driver_order <- merge(
      results[raceId == selected_race_id, .(driverId, positionOrder)],
      drivers[, .(driverId, surname)],
      by = "driverId"
    )[order(positionOrder)][, surname]
    
    race_driver_order <- factor(race_driver_order, levels = race_driver_order)
    
    
    ggplot(
      selected_race[, .(Driver = surname, milliseconds)],
      aes(
        x = milliseconds,
        y = Driver,
        fill = Driver
      )
    ) +
      geom_violin() +
      scale_fill_viridis(
        limits = rev(levels(race_driver_order)), # gradiants colors based on pos
        discrete = TRUE,
        alpha = 0.6,
        option = "H"
      ) +
      scale_x_continuous(
        labels = convert_ms_to_time
      ) +
      scale_y_discrete(
        limits = rev(levels(race_driver_order)), # orders drivers from 1st-last
        position = "right"
      ) +
      labs(
        title = "Drivers' Lap Times Density"
      ) +
      theme_clean() +
      theme(
        plot.background = element_blank(),
        legend.position = "none",
        axis.title = element_blank()
      )
  })
  
  output$lap_time_circuit_race_drivers_best <- renderTable({
    # TODO: Throwing a warning when switching circuit input
    top_times <- lap_time_circuit_race()[, .(milliseconds = min(milliseconds)), 
                                         by = "surname"]
    top_times[, time := convert_ms_to_time(milliseconds)]
    return(top_times[order(milliseconds)][, .(Driver = surname, Time = time)])
  })
}