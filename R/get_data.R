
add.cols <- function(dt) {
  if(!("data.table" %in% class(dt))) stop("dt is not a data.table object.")
  if(Sys.getlocale("LC_TIME") != "English_United States.1252") Sys.setlocale("LC_TIME", "English")
  year_month <- format(dt$started_at, "%b %Y")
  weekday <- lubridate::wday(dt$started_at, label = T, abbr = T, week_start = 1)
  ride_length <- as.numeric(dt$ended_at - dt$started_at)
  ride_dist = geosphere::distGeo(p1 = dt[, c("start_lng", "start_lat"), with = F], 
                                 p2 = dt[, c("end_lng", "end_lat"), with = F])
  return(cbind(dt, year_month, weekday, ride_length, ride_dist))
}


change.datatypes <- function(dt) {
  if(!("data.table" %in% class(dt))) stop("dt is not a data.table object.")
  dt[, c("rideable_type", "member_casual")] <- lapply(
    dt[, c("rideable_type", "member_casual")],
    as.factor
  )
  dt[["year_month"]] <- ordered(dt[["year_month"]], levels = unique(dt$year_month))
  return(dt)
}


get.raw_data <- function(Url, Timeout = 60) {   ## increase timeout if you get timeout error
  mm <- c("202106", "202107", "202108", "202109", "202110", "202111", "202112", "202201",
          "202202", "202203", "202204", "202205")
  url1 <- Url
  urls <- lapply(url1, paste, mm, "-divvy-tripdata.zip", sep = "")[[1]]
  
  ls <- vector(mode = "list", length = length(mm))
  names(ls) <- paste(mm, "-divvy-tripdata.csv", sep = "")
  
  options(timeout = Timeout)
  for (i in 1:length(ls)) {
    ls[[i]] <- tempfile(fileext = ".zip")
    download.file(urls[i], ls[[i]])
    ls[[i]] <- unzip(ls[[i]], names(ls)[i], exdir = tempdir()) |> 
      data.table::fread(na.strings = c("", NA))
  }
  options(timeout = 60)
  
  raw.tot_data <- data.table::rbindlist(ls) |> add.cols() |> change.datatypes() |> 
    data.table::setcolorder(c(1,2,13,3,4,16,14,15,5:12,17)) |> 
    data.table::setkey(ride_id)
  
  invisible(gc(reset = T))
  return(raw.tot_data)
}


