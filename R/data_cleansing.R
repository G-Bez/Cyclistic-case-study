
rm.outliers <- function(data) {
  if(!("data.table" %in% class(data))) stop("dt is not a data.table object.")
  rl.upp_whisk <- robustbase::adjboxStats(data[ride_length > 0, ride_length])$stats[5]
  df <- data[-which(ride_length < 0 | ride_length > rl.upp_whisk
                    | (ride_dist == 0 & start_station_name != end_station_name)
                    | (ride_dist != 0 & start_station_name == end_station_name)
                    | ride_dist %in% data[order(-ride_dist), head(.SD, 2)]$ride_dist)]
  return(df)
}


rm.missing <- function(data) {
  if(!("data.table" %in% class(data))) stop("dt is not a data.table object.")
  df <- data[, grep("*station", names(data)) := NULL]
  df <- df[complete.cases(df)]
  return(df)
}
