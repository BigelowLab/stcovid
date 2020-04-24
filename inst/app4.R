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
                           date = NULL,
                           x = datahub){

  ix <- x$State == state
  if (!is.null(date)) ix <- ix & (x$date == date)

  x %>%
    dplyr::filter(ix)
}


# Define the UI
ui <- fluidPage(
  sidebarLayout(
    selectInput("state", "State:",
                choices=datasets::state.name,
                selected = "Maine"),
    sliderInput("date", "Date:",
                min = min(datahub$date),
                max = max(datahub$date),
                value = max(datahub$date),
                step = 1)
  ),
  mainPanel(
  fluidRow(
    column(width = 4,
           plotOutput('plot1',
                      hover = hoverOpts(id = 'plot1_hover')) ),
    column(width = 8,
           plotOutput('plot2')  )
  ), #fluidRow top
  fluidRow(
    column(width = 4,
           plotOutput('plot3',
                      hover = hoverOpts(id = 'plot4_hover')) ),
    column(width = 8,
           plotOutput('plot4') )
  )
  )
)


# Define the server code
server <- function(input, output) {
  output$plot1 <- renderPlot({
    x <- filter_datahub(input$state, input$date)
    pop <- stcovid::read_census(input$state)
    x <- stcovid::merge_census(x, pop)
    gg <- stcovid::draw_confirmed(x)
    print(gg[[1]])
  })
  output$plot2 <- renderPlot({
    x <- filter_datahub(input$state)
    pop <- stcovid::read_census(input$state)
    gg <- stcovid::draw_infected(x, pop)
    print(gg[[1]])
  })

  output$plot3 <- renderPlot({
    x <- filter_datahub(input$state, input$date)
    pop <- stcovid::read_census(input$state)
    x <- stcovid::merge_census(x, pop)
    gg <- stcovid::draw_confirmed(x)
    print(gg[[2]])
  })
  output$plot4 <- renderPlot({
    x <- filter_datahub(input$state)
    pop <- stcovid::read_census(input$state)
    gg <- stcovid::draw_infected(x, pop)
    print(gg[[2]])
  })

}

# Return a Shiny app object
shinyApp(ui = ui, server = server)

