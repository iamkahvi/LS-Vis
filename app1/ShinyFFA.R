library(shiny)
library(tidyverse)
library(scales)
library(rsconnect)
library(readr)
library(plotly)

FFAv4_0 <- read_csv("data/FFA-Cumulative.csv", 
                    col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
                                     X7 = col_skip()))

FFAv4_0 <- gather(FFAv4_0, "Category", "Food Reclaimed", 3:5)
FFAv4_0$`Food Reclaimed` <-as.numeric(FFAv4_0$`Food Reclaimed`)

rsconnect::setAccountInfo(name='lovingspoonful', token='1A8F0568361C9C11842C34BD9EF9379B', secret='XcLPbyRXwzVrLOfoGXO8evckxFjPfpNshjX340kw')

ui <- fluidPage(
  titlePanel("Cumulative Fresh Food Access Stats", windowTitle = "FFA Stats"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "theYear", 
                  label = "Choose the Year", 
                  choices = FFAv4_0$Year,
                  selected = 2017),
      h5("General Info"),
      verbatimTextOutput("general_info"),
      h5("Selected Point"),
      verbatimTextOutput("info")
    ),

    mainPanel(
      plotOutput(outputId = "FFA", click = "plot_click"),
      plotOutput(outputId = "FFA2", click = "plot_click")
    )
  )
)
server <- function(input, output) {
  output$general_info <- renderPrint({
    table <- FFAv4_0 %>% filter(Year == input$theYear)
    cat("Average Monthly Total: ")
    cat(format(round(as.numeric(colMeans(table[5], na.rm = TRUE)), 0), big.mark=","), "lbs")
    cat("\n")
    cat("Yearly Total: ")
    cat(format(round(as.numeric(colSums(table[5], na.rm = TRUE)), 0), big.mark=","), "lbs")
  })
  
  output$info <- renderPrint({
    row <- nearPoints(FFAv4_0, input$plot_click, xvar = "Date", yvar = "Food Reclaimed", threshold = 10, maxpoints = 1)
    cat("Month: ")
    cat(format(as.Date(as.numeric(as.character(row[3])), origin="1970-01-01"), format = "%B"))
    cat("\n")
    cat("Food Reclaimed: ")
    cat(format(round(as.numeric(row[5]), 1), big.mark=","))
    cat(" lbs")
  })

  output$FFA <- renderPlot ({
    
    FFAv4_0 %>%
      filter(Year == input$theYear) %>%
      ggplot(aes(Date, `Food Reclaimed`)) +
      ylab("Food Reclaimed (lbs)") +
      xlab("Month") +
      geom_point() +
      geom_line(aes(colour = Category, group = Category)) +
      scale_x_date(labels = date_format("%b"), date_breaks = "1 month") +
      scale_y_continuous(breaks = seq(0, 20000, by = 2500))
    
  })
  
  output$FFA2 <- renderPlot ({
    
    FFAv4_0 %>%
      filter(Year == input$theYear) %>%
      filter(Category != "Robot Monthly") %>%
      ggplot(aes(Date, `Food Reclaimed`, fill = Category)) +
      ylab("Food Reclaimed (lbs)") +
      xlab("Month") +
      geom_bar(stat = "identity") +
      scale_x_date(labels = date_format("%b"), date_breaks = "1 month") +
      scale_y_continuous(breaks = seq(0, 20000, by = 2500))

  })
  
}

shinyApp(ui = ui, server= server)


