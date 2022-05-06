library(shiny)
library(tidyverse)
library(curl)
library(jsonlite)
library(lubridate)
library(glue)
library(showtext)
source("expand_period.R")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Merienda&display=swap');
          body {
            font-family: 'Merienda';
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
               tags$a(href="https://api.weather.gov/gridpoints/MKX/90,62", 
                      "https://api.weather.gov/gridpoints/MKX/90,62")
      ),
      h2("Color Coding", style='text-align:center;'),
      h3("Good conditions", style='background:#2dc937;color:white;text-align:center;padding:5px;'),
      h3("Okay conditions", style='background:#efb700;color:white;text-align:center;padding:5px;'),
      h3("Bad conditions", style='background:#CC3232;color:white;text-align:center;padding:5px;'),
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      htmlOutput(outputId = "error"),
      plotOutput(outputId = "windPlot"),
      plotOutput(outputId = "wavePlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  url <- "https://api.weather.gov/gridpoints/MKX/90,62"
  
  req <- curl_fetch_memory(url)
  
  x <- prettify(rawToChar(req$content)) %>%
    fromJSON()
  
  if (x$status != 200) {
    output$error <- renderText({
      paste("Oops, there was an error!<br><br><strong>Details</strong>: ",
             x$detail)
    })
  } else {
    waves <- x$properties$waveHeight$values %>%
      mutate(value = value * 3.281)
    
    temp <- x$properties$temperature$values %>%
      mutate(value = value * 1.8 + 32)
    
    wind <- x$properties$temperature$values %>%
      mutate(value / 1.609)
    
    green <- "#2dc937"
    yellow <- "#efb700"
    red <- "#CC3232"
    
    font_add_google("Merienda", "m")
    showtext_auto()
    
    
    output$windPlot <- renderPlot({
      wave_max <- input$waves
      waves_df <- expand_period(waves)
      
      waves_df %>%
        mutate(gonogo = case_when(value >= wave_max ~ red,
                                  value >= wave_max * .6 ~ yellow,
                                  TRUE ~ green)) %>%
        ggplot(aes(hours, value, fill = gonogo, color = gonogo)) +
        geom_col() +
        scale_fill_identity() +
        scale_color_identity() +
        scale_y_continuous(breaks = c(1:floor(max(waves_df$value))),
                           labels = c(1:(floor(max(waves_df$value)) - 1),
                                      paste0(floor(max(waves_df$value)), " ft"))) +
        scale_x_datetime(expand = c(0, 0)) +
        theme_minimal() +
        theme(text = element_text(family = "m"),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank(),
              plot.title.position = "plot") +
        labs(x = "", y = "", 
             title = "Wave Height Forecast")
    })
    
    output$wavePlot <- renderPlot({
      wind_max <- input$wind
      wind_df <- expand_period(wind)
      
      wind_df %>%
        mutate(gonogo = case_when(value >= wind_max ~ red,
                                  value >= wind_max * .6 ~ yellow,
                                  TRUE ~ green)) %>%
        ggplot(aes(hours, value, fill = gonogo, color = gonogo)) +
        geom_col() +
        scale_fill_identity() +
        scale_color_identity() +
        scale_y_continuous(breaks = c(1:floor(max(wind_df$value))),
                           labels = c(1:(floor(max(wind_df$value)) - 1),
                                      paste0(floor(max(wind_df$value)), " mph"))) +
        scale_x_datetime(expand = c(0, 0)) +
        theme_minimal() +
        theme(text = element_text(family = "m"),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank(),
              plot.title.position = "plot") +
        labs(x = "", y = "", 
             title = "Wind Speed Forecast")
    })
  }
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
