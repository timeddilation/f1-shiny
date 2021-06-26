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