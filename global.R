library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(data.table)
library(stringr)
library(scales)
library(ggplot2)
library(ggthemes)
library(viridis)
library(plotly)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

source("helper_functions.R")

results <- fread("data/results.csv")
quali <- fread("data/qualifying.csv")
constructors <- fread("data/constructors.csv")

drivers <- (function(){
  drivers <- fread("data/drivers.csv")
  drivers[, Driver := gsub("_", " ", driverRef)
          ][, Driver := str_to_title(Driver)]
})()

circuits <- (function(){
  circuits <- fread("data/circuits.csv")
  # fix characters in names
  circuits[circuitId == 18, name := "Autódromo José Carlos Pace"
           ][circuitId == 20, name := "Nürburgring"]
  circuits[, name := gsub("Ã³", "ó", name)]
  return(circuits)
})()

races <- (function(){
  races <- fread("data/races.csv")
  
  race_seqs <- races[order(date)][, .(
    race_seq = seq_len(.N), raceId, round, name, date
  ), by = c("year", "circuitId")]
  
  race_seqs <- merge(
    race_seqs,
    circuits[, .(circuitId, circuit_name = name)],
    by = "circuitId"
  )
  
  race_seqs <- race_seqs[order(date)]
  race_seqs[, season_race_index := paste0(year, ".", race_seq)]
  race_seqs[, fq_name := paste0("Round ", round, ": ", name, " ", year)]
  
  return(race_seqs)
})()

lap_times_tidy <- (function(){
  lap_times <- fread("data/lap_times.csv")
  
  race_circuit <- merge(
    circuits[, .(circuitId, circuitRef, name)],
    races[, .(raceId, circuitId, year, season_race_index)],
    by = "circuitId"
  )
  
  tidy_times <- merge(
    lap_times,
    race_circuit,
    by = "raceId"
  )
  
  tidy_times <- merge(
    tidy_times,
    drivers[, .(driverId, Driver)],
    by = "driverId"
  )
  
  return(tidy_times)
})()

results_tidy <- (function(){
  result_status <- fread("data/status.csv")
  
  tidy_results <- merge(
    races[, .(raceId, name, fq_name, date, year, Round = round)],
    results[, .(raceId, driverId, points, constructorId, statusId, milliseconds,
                `Finishing Position` = positionOrder, `Starting Position` = grid)],
    by = "raceId"
  ) |>
    merge(
      drivers[, .(driverId, Driver)],
      by = "driverId"
    ) |>
    merge(
      constructors[, .(constructorId, Constructor = name)],
      by = "constructorId"
    ) |>
    merge(
      result_status[, .(statusId, Status = status)],
      by = "statusId"
    )
  tidy_results <- tidy_results[order(date)]
  tidy_results[, `Season Points` := cumsum(points), by = c("year", "driverId")]
  # handle pit lane starts starting position being 0
  tidy_results <- rbind(
    tidy_results[order(raceId, `Starting Position`)][`Starting Position` != 0],
    tidy_results[`Starting Position` == 0]
  )
  
  tidy_results[, `Starting Position` := seq_len(.N), by = "raceId"]
  return(tidy_results)
})()

circuits_with_times <- unique(lap_times_tidy[order(name)][, name])
