

# Global variables can go here
library(shiny)
library(rlang)
library(ggplot2)
library(patchwork)
library(dplyr)
library(stcovid)
library(datasets)
datahub <- stcovid::read_datahub()

filter_datahub <- function(state = "Maine",
                           date = as.Date("2020-04-20"),
                           x = datahub){

  ix <- x$State == state & x$date == date
  x %>%
    dplyr::filter(ix)
}


# Define the UI
ui <- bootstrapPage(
  selectInput("state", "State:",
              choices=state.name,
              selected = "Maine"),
  sliderInput("date", "Date:",
              min = min(datahub$date),
              max = max(datahub$date),
              value = max(datahub$date),
              step = 1),
  plotOutput('plot')
)


# Define the server code
server <- function(input, output) {
  output$plot <- renderPlot({
    x <- filter_datahub(input$state, input$date)
    pop <- stcovid::read_census(input$state)
    if (nrow(x) != nrow(pop)){
      msg <- sprintf("%s data mismatch: %i (datahub) %i (census)",
                     input$state, nrow(x), nrow(pop))
      warning(msg)
      return()
    }
    x <- stcovid::merge_census(x, pop)
    gg <- stcovid::draw_confirmed(x)
    print(gg[[1]] + gg[[2]])
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)

