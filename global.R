library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(data.table)
library(stringr)
library(ggplot2)
library(plotly)
library(viridis)
library(ggthemes)

source("helper_functions.R")

races <- fread("data/races.csv")
results <- fread("data/results.csv")

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
    drivers[, .(driverId, Driver)],
    by = "driverId"
  )
  
  return(tidy_times)
})()

circuits_with_times <- unique(lap_times_tidy[order(name)][, name])
