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
  
  eval_circuit <- reactive({
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
  
  output$circuit_seasons_violen <- renderPlot({
    circuit_races <- eval_circuit()
    
    gg <- 
      ggplot(
        circuit_races,
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
      labs(
        title = "Lap Time Distribution"
      ) +
      scale_x_continuous(
        labels = convert_ms_to_time
      ) +
      theme_clean() +
      theme(
        plot.background = element_blank(),
        legend.position = "none",
        axis.title = element_blank()
      )

    return(gg)
  })
}