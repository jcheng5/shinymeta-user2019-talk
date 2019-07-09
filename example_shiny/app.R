library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  textInput("package", "Package name", value = "ggplot2"),
  verbatimTextOutput("summary"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  
  downloads <- reactive({
    # Retrieve a year's worth of daily download data
    cranlogs::cran_downloads(input$package, from = Sys.Date() - 365, to = Sys.Date())
  })
  
  downloads_rolling <- reactive({
    # Convert daily data to 7 day rolling average
    downloads() %>% mutate(count = zoo::rollapply(count, 7, mean, fill = "extend"))
  })
  
  output$plot <- renderPlot({
    ggplot(downloads_rolling(), aes(date, count)) + geom_line() + ggtitle("Seven day rolling average")
  })

  output$summary <- renderPrint({
    summary(downloads()$count)
  })
}

shinyApp(ui, server)
