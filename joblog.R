library(shiny)
library(tidyverse)
library(lubridate)


shinyApp(
  ui = fluidPage(
    fluidRow(
      column(6,
             DT::dataTableOutput("table")),
      column(6,
             plotOutput("trend"))
    ),
    fluidRow(
      column(12,
             textOutput("message"),
             tags$head(tags$style(
               type = "text/css",
               ".test_type {font-size: 20px;
                 font-style: bold;}"))
      )
    )
  ),
  server = function(input, output) {
    d <- read_csv("joblog.csv") %>% 
      mutate(Week = isoweek(Date))
    n <- d %>% 
      group_by(Week) %>% 
      tally()
    output$table <- DT::renderDataTable(n)
    output$trend <- renderPlot({
      n %>% 
        ggplot() +
        geom_point(aes(Week, n), col = "steelblue") +
        geom_line(aes(Week, n), col = "gray50") +
        theme_classic() +
        xlab("Week of the Year") +
        ylab("Number of jobs searched")
    })
    output$message <- renderText({
      m <- n %>% 
        arrange(desc(Week)) %>% 
        filter(Week > isoweek(today) - 4) %>% 
        pull(n) %>% 
        sum()
      if (m >= 6) {
        "You searched enough jobs this week."
      } else {
        paste0("You are still missing ", 6 - m, " job searches this week.")
      }
    })
  }
)
