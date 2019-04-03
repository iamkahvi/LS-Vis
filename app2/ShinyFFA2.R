library(shiny)
library(tidyverse)
library(scales)
library(readxl)

GAR_2017_FINAL <- read_excel("data2/GAR 2017 FINAL.xlsx", 
                             sheet = "Grow A Row 2017 Track Sheet (OL", 
                             col_types = c("date", "text", "text", 
                                           "text", "numeric", "text", "skip", 
                                           "skip"))

ui <- fluidPage(
  titlePanel("2017 GAR Stats", windowTitle = "GAR Stats"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "theLocation", 
                  label = "Choose the Location", 
                  choices = GAR_2017_FINAL$Location,
                  selected = "DKPM"),
      sliderInput(inputId = "theMax",
                  label = "Set Range of Food Reclaimed", step = 5,
                  min = 0, max = 200, value = c(0, 200)),
      h5("General Info"),
      verbatimTextOutput("general_info"),
      h5("Selected Point"),
      verbatimTextOutput("click_info"),
      h5("Selected Points"),
      verbatimTextOutput("brush_info")
    ),
    
    mainPanel(
      plotOutput(outputId = "GAR", click = "plot_click", brush = brushOpts(id = "plot_brush"), height = "700px")

    )
    
  )
  
)

server <- function(input, output) {
  
  output$GAR <- renderPlot ({
    
    GAR_2017_FINAL %>%
      filter(Location == input$theLocation) %>%
      filter(lbs > 0) %>%
      filter(lbs < input$theMax[2]) %>%
      filter(lbs > input$theMax[1]) %>%
      ggplot(aes(DATE, lbs, col = `DONOR TYPE`)) +
      ylab("Food Reclaimed (lbs)") +
      xlab("Date") +
      geom_point(alpha = 0.3, size = 5) +
      scale_x_date(labels = date_format("%b, %d"), date_breaks = "1 week") +
      scale_y_continuous(breaks = seq(0, 20000, by = 5))
  })
  
  output$general_info <- renderPrint({
    table <- GAR_2017_FINAL %>% filter(Location == input$theLocation) %>% filter(lbs < input$theMax[2]) %>% filter(lbs > input$theMax[1])
    cat("Average Donation: ")
    cat(round(as.numeric(colMeans(table[5], na.rm = TRUE)), 1), "lbs")
    cat("\n")
    cat("Sum of Donations: ")
    cat(format(round(as.numeric(colSums(table[5], na.rm = TRUE)), 0), big.mark=","), "lbs")
    cat("\n")
    cat("Number of Donations: ")
    cat(nrow(table[5]))
  })
  
  output$click_info <- renderPrint({
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    row <- nearPoints(GAR_2017_FINAL, input$plot_click, addDist = TRUE, threshold = 10, maxpoints = 1)
    cat("Date:", format(as.Date(as.numeric(as.character(row[1])), origin="1970-01-01"), format = "%B %d, %Y"))
    cat("\n")
    cat("Donor:", as.character(row[2]))
    cat("\n")
    cat("Food:", as.character(row[4]))
    cat("\n")
    cat("Amount:", as.character(row[5]), "lbs")
    
  })
  
  output$brush_info <- renderPrint({
    table <- brushedPoints(GAR_2017_FINAL, input$plot_brush)
    cat("Sum of Selected Donations: ")
    cat(round(as.numeric(colSums(table[5], na.rm = TRUE)), 1), big.mark=" ")
    cat("\n")
  
  })
  
}

dt <- GAR_2017_FINAL

dt$DATE <- as.character(GAR_2017_FINAL$DATE)

dt$DATE <- as.Date(GAR_2017_FINAL$DATE, "%Y-%m-%d")

GAR_2017_FINAL <- dt

shinyApp(ui = ui, server= server)


