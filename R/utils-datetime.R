
get_timezone_offset <- function(time_zone = "") {
  if(grepl("^[+-][01][0-9]{3}$", time_zone)) {
    offset <- as.integer(time_zone)
    if( offset < -14 || offset > 14 ) {
      return(NA_character_)
    }
    return(sprintf("%05d", offset))
  }
  time <- as.POSIXlt(0, tz = time_zone)
  return(strftime(time, "%z"))
}

bids_datetime_to_nanotime <- function(string) {
  # string <- c("1867-06-15T13:45:30.0132916Z", "1867-06-15T13:45:30.0132916")

  if(inherits(string, "nanotime")) {
    return(string)
  }

  if(!length(string)) {
    return(nanotime::nanotime())
  }

  if(is.character(string)) {
    if(identical(toupper(substring(string[[1]], nchar(string[[1]]))), "Z")) {
      # UTC
      tz <- "UTC"
      string <- gsub("Z$", "", string)
    } else {
      tz <- Sys.timezone()
      if(is.na(tz)) {
        tz <- ""
      }
    }
  }
  time <- nanotime::nanotime(string, format = "%Y-%m-%dT%H:%M:%E9S", tz = tz)

  time
}

nanotime_to_bids_datetime <- function(time, milliseconds = TRUE, utc = TRUE) {
  if(!length(time)) {
    return(character(0))
  }
  if(milliseconds) {
    fmt <- "%Y-%m-%dT%H:%M:%E6S"
  } else {
    fmt <- "%Y-%m-%dT%H:%M:%S"
  }
  if( utc ) {
    re <- format(time, format = paste0(fmt, "Z"), tz = "UTC")
  } else {
    tz <- Sys.timezone()
    if(is.na(tz)) {
      tz <- ""
    }
    re <- format(time, format = fmt, tz = tz)
  }
  re[is.na(time)] <- NA_character_
  re
}
