

# Global variables can go here
library(shiny)
library(ggplot2)
library(patchwork)
library(dplyr)
library(stcovid)
library(datasets)
datahub <- stcovid::read_datahub()
outtext <- function(width = 200){
  "The maps and graphs shown are not forecasts or predictions about what to expect looking ahead. Data are updated daily from the COVID19 data hub at https://github.com/covid19datahub/COVID19. Census data are from US Census estimates via https://github.com/walkerke/tidycensus."
}

filter_datahub <- function(state = "Maine",
                           date = NULL,
                           x = datahub){

  ix <- x$State == state
  if (!is.null(date)) ix <- ix & (x$date == date)
  x %>%
    dplyr::filter(ix)
}


# Define the UI
ui <- fluidPage(
  title = "State/County level COVID-19 data",
  fluidRow(
    column(width = 4,
      selectInput("state", "State:",
                  choices=datasets::state.name,
                  selected = "Maine")
    ),
    column(width = 6, offset = 1,
      sliderInput("date", "Date:",
                  min = min(datahub$date),
                  max = max(datahub$date),
                  value = max(datahub$date),
                  step = 1)
    )
  ), # fluidRow
  hr(),
  plotOutput('plot', height = 600),
  hr(),
  textOutput("caption")
)


# Define the server code
server <- function(input, output) {
  output$plot <- renderPlot({
    x <- filter_datahub(input$state, input$date)
    pop <- stcovid::read_census(input$state)
    m <- stcovid::merge_census(x, pop)
    mm <- stcovid::draw_statemap(m)
    y <- filter_datahub(input$state)
    h  <- stcovid::draw_history(y, pop)
    print( (mm[[1]] + mm[[2]]) +  h )
  })
  output$caption <- renderText(outtext())
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)
