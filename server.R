server <- function(input, output, session){
  source("server/lap_times_server.R", local = T)
  
  world_poly <- ne_countries(
    scale = "medium",
    type = "countries",
    continent = c("North America", "Asia", "Africa", "Europe", "South America", "Oceania"),
    returnclass = "sf"
  )
  
  observeEvent(input$race_sum_year, {
    # update the circuits with races in selected year
    updateSelectInput(
      session = session,
      "race_sum_circuit",
      choices = circuits_by_year(input$race_sum_year)[, name],
      selected = circuits_by_year(input$race_sum_year)[1, name]
    )
    # initialize the Grand Prix option as well
    updateSelectInput(
      session = session,
      "race_sum_gp",
      choices = gps_by_year_circuit(
        input$race_sum_year,
        circuits_by_year(input$race_sum_year)[1, name]
      )[, name],
      selected = gps_by_year_circuit(
        input$race_sum_year,
        circuits_by_year(input$race_sum_year)[1, name]
      )[1, name]
    )
  }, priority = 1)
  
  observeEvent(input$race_sum_circuit, {
    updateSelectInput(
      session = session,
      "race_sum_gp",
      choices = gps_by_year_circuit(
        input$race_sum_year,
        input$race_sum_circuit
      )[, name],
      selected = gps_by_year_circuit(
        input$race_sum_year,
        input$race_sum_circuit
      )[1, name]
    )
  })
  
  output$race_sum_map <- renderPlotly({
    geo_circuit <- circuits[name == input$race_sum_circuit, .(
      Name = name,
      lat,
      lng,
      Location = paste(location, country, sep = ", ")
    )]
    
    year_circuits <- lap_times_tidy[year == input$race_sum_year, circuitId] |>
      unique() |>
      {\(x) {circuits[circuitId %in% x][name != input$race_sum_circuit]}}()
    year_circuits <- year_circuits[, .(
      Name = name,
      lat,
      lng,
      Location = paste(location, country, sep = ", ")
    )]
    
    gg <- ggplot() +
      geom_sf(data = world_poly, color = "white", fill = "#63637b") +
      geom_point(
        data = geo_circuit,
        aes(
          x = lng,
          y = lat,
          name = Name,
          loc = Location
        ),
        shape = 18,
        size = 3,
        color = "#ff1801"
      ) +
      geom_point(
        data = year_circuits,
        aes(
          x = lng,
          y = lat,
          name = Name,
          loc = Location
        ),
        shape = 18,
        size = 1,
        # color = "#db6458"
        color = "#ff1801",
        alpha = 0.6
      ) +
      theme_solid() +
      xlim(
        geo_circuit[, lng] - 40,
        geo_circuit[, lng] + 40
      ) +
      ylim(
        geo_circuit[, lat] - 10,
        geo_circuit[, lat] + 10
      ) +
      theme(
        panel.background = element_rect(fill = "#1f1f27")
      )
    
    pp <- ggplotly(gg, tooltip = c("name", "loc")) |>
      config(displayModeBar = F)
    return(pp)
  })
  
  output$race_sum_lap_pos <- renderPlotly({
    eval_raceId <- races[year == input$race_sum_year
                         ][name == input$race_sum_gp
                           ][, raceId]
    
    race_poss <- merge(
      results[raceId == eval_raceId & grid != 0, .(driverId, lap = 0, position = grid)],
      drivers[, .(driverId, Driver)],
      by = "driverId"
    ) |>
      rbind(
        lap_times_tidy[raceId == eval_raceId, .(driverId, lap, position, Driver)]
      )
    
    gg <- ggplot(race_poss, aes(x = lap, y = position, color = Driver)) +
      geom_point() +
      geom_line() +
      scale_y_continuous(
        trans = "reverse",
        breaks = race_poss[order(position)][, position] |> unique(),
      ) +
      xlab("Lap") +
      ylab("Position") +
      theme_clean() +
      theme(
        plot.background = element_rect(colour = "white"),
        legend.background = element_blank()
      )
    # TODO: Throws error when changing year and the prior selected circuit is not in that year
    return(ggplotly(gg))
  })
}