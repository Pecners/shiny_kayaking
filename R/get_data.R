library(tidyverse)
library(curl)
library(jsonlite)
library(lubridate)

url <- "https://api.weather.gov/gridpoints/MKX/90,62"

req <- curl_fetch_memory(url)

x <- prettify(rawToChar(req$content)) %>%
  fromJSON()

waves <- x$properties$waveHeight$values %>%
  mutate(value = value * 3.281,
         start_time_local = ymd_hms(str_sub(validTime, start = 1, end = 25),
                              tz = "America/Chicago"))

temp <- x$properties$temperature$values %>%
  mutate(value = value * 1.8 + 32)

wind <- x$properties$temperature$values

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
                          tz = "America/Chicago")
    
    hours <- seq.POSIXt(from = start_time, length.out = p_full, by = 60*60)
    
    return(
      tibble(
        hours = hours,
        value = t$value
      )
    )
  })
}

waves_df <- expand_period(waves)
wind_df <- expand_period(wind)

green <- "#2dc937"
yellow <- "#efb700"
red <- "#CC3232"

waves_df %>%
  mutate(gonogo = case_when(value >= 5.0 ~ red,
                           value >= 3.0 ~ yellow,
                           TRUE ~ green)) %>%
  ggplot(aes(hours, value, fill = gonogo, color = gonogo)) +
  geom_col() +
  scale_fill_identity() +
  scale_color_identity() +
  scale_y_continuous(limits = c(0, 8), breaks = c(1:8),
                     labels = c(c(1:7), "8 ft")) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title.position = "plot") +
  labs(x = "", y = "", 
       title = "Wave Height Forecast")

wind_df %>%
  mutate(value = value / 1.609,
         gonogo = case_when(value >= 10.0 ~ red,
                            value >= 5.0 ~ yellow,
                            TRUE ~ green)) %>%
  ggplot(aes(hours, value, fill = gonogo, color = gonogo)) +
  geom_col() +
  scale_fill_identity() +
  scale_color_identity() +
  scale_y_continuous(breaks = c(1:floor(max(wind_df$value))),
                     labels = c(1:(floor(max(wind_df$value)) - 1),
                                paste0(floor(max(wind_df$value)), " mph"))) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title.position = "plot") +
  labs(x = "", y = "", 
       title = "Wind Speed Forecast")
