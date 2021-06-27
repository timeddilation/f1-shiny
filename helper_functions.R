remove_outlier_times <- function(race_times){
  # should only be evaluating a single race's lap times
  # used when creating violin of lap times to avoid long tails
  fourth_quartile <- boxplot.stats(race_times[, milliseconds])$stats[5]
  race_times <- race_times[milliseconds <= fourth_quartile]
  
  return(race_times)
}

convert_ms_to_time <- Vectorize(function(ms){
  # a function for labels in ggplot
  # when plotting milliseconds on x axis, convert to MM:SS.SSS format:
  # scale_x_continuous(labels = convert_ms_to_time)
  displayTime <- ""
  
  if(!is.na(ms)){
    seconds <- ms/1000
    totalMinutes <- floor(seconds / 60)
    remainingSeconds <- seconds - (totalMinutes * 60)
    
    if (remainingSeconds < 10){
      displayTime <- paste(totalMinutes, ":0", sprintf("%.3f", round(remainingSeconds,3)), sep = "")
    } else {
      displayTime <- paste(totalMinutes, ":", sprintf("%.3f", round(remainingSeconds,3)), sep = "")  
    }
  }
  return(displayTime)
})

available_circuit_seasons <- function(circuit_name){
  races[circuitId == circuits[name == circuit_name, circuitId]
        ][raceId %in% lap_times_tidy[, raceId]
          ][order(year)
            ][, year]
}

raceId_by_circuit_season <- function(circuit_name, season){
  circuit_id <- circuits[name == circuit_name, circuitId]
  return(races[circuitId == circuit_id & year == season, raceId][1])
}

race_drivers <- function(circuit_name, season, race_id = NA){
  if (is.na(race_id)) {
    race_id <- lap_times_tidy[name == circuit_name
                              ][year == season
                                ][, raceId] |>
      min()
  }
  
  return(
    lap_times_tidy[raceId == race_id, Driver] |>
      unique()
  )
}

driver_fastest_lap <- function(race_id, driver_id){
  driver_results <- lap_times_tidy[raceId == race_id & driverId == driver_id]
  
  if(nrow(driver_results) < 1){
    return(NA)
  } else {
    return(driver_results[order(milliseconds)][1])
  }
}