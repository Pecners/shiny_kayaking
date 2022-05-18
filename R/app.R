library(shiny)
library(tidyverse)
library(curl)
library(jsonlite)
library(lubridate)
library(glue)
library(showtext)
library(leaflet)
library(sf)
library(rdrop2)
source("expand_period.R")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Merienda&display=swap');
          h1 {
            margin-bottom: 30px;
          }
          body {
            font-family: 'Merienda';
          }
          .irs--shiny .irs-bar {
            border-top: 1px solid #CB4335;
            border-bottom: 1px solid #CB4335;
            background: #CB4335;
          }
          .irs--shiny .irs-single {
            background-color: #CB4335;
          }
                    ")
               )
  ),
  
  # App title ----
  titlePanel(h1("Spencer's Very Specific Kayaking Forecast", align = "center"),
             windowTitle = "Spencer Kayaks"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Width of panel out of 12, default is 4
      width = 3,
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "waves", 
                  label = "Set your wave height limit (ft):",
                  min = 1,
                  max = 10,
                  value = 5),
      sliderInput(inputId = "wind",
                  label = "Set your wind speed limit (mph):",
                  min = 1,
                  max = 20,
                  value = 10),
      tags$div(class="header", checked=NA,
               tags$p(
                 glue("Data is sourced via the US National Weather Service API (see link below). ",
                      "Location is set near South Shore Marina in Milwaukee, Wisconsin, ",
                      "where Spencer most often launches his kayak.")
                 ),
               tags$a(href="https://api.weather.gov/gridpoints/MKX/89,62", 
                      "https://api.weather.gov/gridpoints/MKX/89,62", target="_blank")
      ),
      h2("Color Coding", style='text-align:center;'),
      h3("Good conditions", style='background:#2dc937;color:white;text-align:center;padding:5px;'),
      h3("Okay conditions", style='background:#efb700;color:white;text-align:center;padding:5px;'),
      h3("Bad conditions", style='background:#CC3232;color:white;text-align:center;padding:5px;'),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("Overall",
          htmlOutput(outputId = "error"),
          plotOutput(outputId = "forecastPlot")
        ),
        tabPanel("Details",
                 plotOutput(outputId = "detailsWaves", height = "250px"),
                 plotOutput(outputId = "detailsWind", height = "250px")
                 ),
        tabPanel("About",
                 h2("Forecast Location"),
                 leafletOutput(outputId = "aboutLocation"))
      )
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  url <- "https://api.weather.gov/gridpoints/MKX/89,62"
  
  req <- curl_fetch_memory(url)
  
  
  x <- prettify(rawToChar(req$content)) %>%
    fromJSON()
  
  token <- readRDS("tokenfile.RDS")
  
  # Handle errors
  
  if (req$status_code != 200) {
    drop_download("d.rda", local_path = "d.rda", overwrite = TRUE)
    x <- read_rds("d.rda")
    resp <- prettify(rawToChar(req$content)) %>%
      fromJSON()
    
    output$error <- renderText({
      paste("Oops, there was an error! Showing old data.<br><br><strong>Error details</strong>: ",
            resp$detail)
    })
  
  # On good response, update cache
  } else {
    saveRDS(x, "d.rda")
    drop_upload("d.rda", dtoken = token)
  }
  waves <- x$properties$waveHeight$values %>%
    mutate(value = value * 3.281)
  
  temp <- x$properties$temperature$values %>%
    mutate(value = value * 1.8 + 32)
  
  wind <- x$properties$windSpeed$values %>%
    mutate(value = value / 1.609)
  
  
  green <- "#2dc937"
  yellow <- "#efb700"
  red <- "#CC3232"
  
  font_add_google("Merienda", "m")
  showtext_auto()
  
  # Tab 1
  output$forecastPlot <- renderPlot({
    waves_df <- expand_period(waves)
    wind_df <- expand_period(wind)
    wave_max <- input$waves
    wind_max <- input$wind
    
    both <- bind_rows(wind_df %>% mutate(group = "wind"), 
                      waves_df %>% mutate(group = "wave")) %>%
      pivot_wider(names_from = group, values_from = value) %>%
      mutate(wind_score = case_when(wind >= wind_max ~ 2,
                                    wind >= wind_max * .6 ~ 1,
                                    TRUE ~ 0),
             wave_score = case_when(wave >= wave_max ~ 2,
                                    wave >= wave_max * .6 ~ 1,
                                    TRUE ~ 0),
             gonogo = case_when(wind_score == 2 ~ red,
                                wave_score == 2 ~ red,
                                wind_score == 0 ~ green,
                                TRUE ~ yellow)) %>%
      filter(!is.na(wind) & !is.na(wave))
    
    both %>%
      ggplot(aes(date(hours), hour(hours), fill = gonogo)) +
      geom_tile(color = "white") +
      scale_fill_identity() +
      scale_y_reverse(breaks = seq(from = 0, to = 20, by = 4),
                      labels = c("12 am", "4 am", "8 am",
                                 "12 pm", "4 pm", "8 pm")) +
      scale_x_date(date_breaks = "days", 
                   labels = function(x) format.Date(x, "%a,\n%b %d")) +
      theme_minimal() +
      theme(text = element_text(family = "m", size = 16),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.title.position = "plot",
            plot.margin = margin(t = 20)) +
      labs(x = "", y = "", 
           title = "Wind Speed + Wave Height Forecast")
  })
  
  # Tab 2
  
  output$detailsWaves <- renderPlot({
    waves_df <- expand_period(waves)
    wave_max <- input$waves
    wave_mid <- wave_max * .6
    m <- max(waves_df$value) + 1
    
    waves_df %>%
      ggplot(aes(hours, value)) +
      annotate(geom = "rect", ymin = wave_max, ymax = m,
               xmin = min(waves_df$hours), xmax = max(waves_df$hours),
               fill = alpha(red, .75)) +
      annotate(geom = "rect", ymin = wave_mid, ymax = wave_max,
               xmin = min(waves_df$hours), xmax = max(waves_df$hours),
               fill = alpha(yellow, .75)) +
      annotate(geom = "rect", ymin = 0, ymax = wave_mid,
               xmin = min(waves_df$hours), xmax = max(waves_df$hours),
               fill = alpha(green, .75)) +
      geom_step(size = 1) +
      scale_y_continuous(limits = c(0, m)) +
      scale_x_datetime(labels = function(x) format.Date(x, "%a,\n%b %d"),
                       expand = c(0,0)) +
      theme_minimal() +
      theme(text = element_text(family = "m", size = 16),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.title.position = "plot",
            plot.margin = margin(t = 20, b = 0)) +
      labs(x = "", y = "", 
           title = "Wave Height (ft)")
  })
  
  output$detailsWind <- renderPlot({
    wind_df <- expand_period(wind)
    wind_max <- input$wind
    wind_mid <- wind_max * .6
    m <- max(wind_df$value, na.rm = TRUE) + 1
    
    wind_df %>%
      ggplot(aes(hours, value)) +
      annotate(geom = "rect", ymin = wind_max, ymax = m,
               xmin = min(wind_df$hours, na.rm = TRUE), 
               xmax = max(wind_df$hours, na.rm = TRUE),
               fill = alpha(red, .75)) +
      annotate(geom = "rect", ymin = wind_mid, ymax = wind_max,
               xmin = min(wind_df$hours), xmax = max(wind_df$hours),
               fill = alpha(yellow, .75)) +
      annotate(geom = "rect", ymin = 0, ymax = wind_mid,
               xmin = min(wind_df$hours), xmax = max(wind_df$hours),
               fill = alpha(green, .75)) +
      geom_step(size = 1) +
      scale_y_continuous(limits = c(0, m)) +
      scale_x_datetime(labels = function(x) format.Date(x, "%a,\n%b %d"),
                       expand = c(0,0)) +
      theme_minimal() +
      theme(text = element_text(family = "m", size = 16),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.title.position = "plot",
            plot.margin = margin(t = 0)) +
      labs(x = "", y = "", 
           title = "Wind Speed (mph)")
  })
  
  output$aboutLocation <- renderLeaflet({
    geo <- x$geometry$coordinates[1,,] %>%
      as.data.frame() %>%
      st_as_sf(coords = c("V1", "V2"), crs = 4326) %>%
      summarise() %>%
      st_cast(to = "POLYGON")
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = geo)

  })
  
  # Record coords of click
  observe({
    event <- input$aboutLocation_click
    if (is.null(event))
      return()
    
    isolate({
      cat("Lat: ", as.numeric(event$lat), "\n",
          "Long: ", as.numeric(event$lng), "\n")
    })
  })
    
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
