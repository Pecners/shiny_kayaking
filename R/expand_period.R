expand_period <- function(df) {
  temp_df <- map_df(1:nrow(df), function(i) {
    t <- df[i,]
    
    if (str_detect(t$validTime, "H")) {
      p_hours <- as.numeric(str_extract(t$validTime, "\\d*(?=H)"))
    } else {
      p_hours <- 0
    }
    
    if (str_detect(t$validTime, "D")) {
      p_days <- as.numeric(str_extract(t$validTime, "\\d*(?=D)"))
    } else {
      p_days <- 0
    }
    
    p_full <- p_days * 24 + p_hours
    
    start_time <- ymd_hms(str_sub(t$validTime, start = 1, end = 25),
                          tz = "America/Chicago", quiet = TRUE) 
    
    hours <- seq.POSIXt(from = start_time, length.out = p_full, by = 60*60) %>%
      format_ISO8601(.)
    
    #cat(paste0(i, ": ", ymd_hms(hours)), "\n")
    
    return(
      tibble(
        hours = ymd_hms(hours),
        value = t$value
      )
    )
  })
}