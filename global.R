library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2)
library(viridis)
library(ggthemes)

source("helper_functions.R")

races <- fread("data/races.csv")
drivers <- fread("data/drivers.csv")

circuits <- (function(){
  circuits <- fread("data/circuits.csv")
  # fix characters in names
  circuits[circuitId == 18, name := "Autódromo José Carlos Pace"
           ][circuitId == 20, name := "Nürburgring"]
  return(circuits)
})()

lap_times_tidy <- (function(){
  lap_times <- fread("data/lap_times.csv")
  
  race_circuit <- merge(
    circuits[, .(circuitId, circuitRef, name)],
    races[, .(raceId, circuitId, year)],
    by = "circuitId"
  )
  
  tidy_times <- merge(
    lap_times,
    race_circuit,
    by = "raceId"
  )
  
  tidy_times <- merge(
    tidy_times,
    drivers[, .(driverId, surname)],
    by = "driverId"
  )
  
  return(tidy_times)
})()

circuits_with_times <- unique(lap_times_tidy[order(name)][, name])
default_circuit <- circuits_with_times[2]
