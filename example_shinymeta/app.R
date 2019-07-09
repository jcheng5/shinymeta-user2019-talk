library(shiny)
library(ggplot2)
library(dplyr)
library(shinymeta)
library(shinyAce)
library(clipr)

ui <- fluidPage(
  textInput("package", "Package name", value = "ggplot2"),
  verbatimTextOutput("summary"),
  plotOutput("plot"),
  downloadButton("downloadReport", "Download as report")
)

server <- function(input, output, session) {
  downloads <- metaReactive({
    "# Retrieve a year's worth of daily download data"
    cranlogs::cran_downloads(!!input$package, from = Sys.Date() - 365, to = Sys.Date())
  })
  
  downloads_rolling <- metaReactive({
    "# Convert daily data to 7 day rolling average"
    !!downloads() %>% mutate(count = zoo::rollapply(count, 7, mean, fill = "extend"))
  })
  
  output$plot <- metaRender(renderPlot, {
    ggplot(!!downloads_rolling(), aes(date, count)) + geom_line() + ggtitle("Seven day rolling average")
  })
  
  output$summary <- metaRender(renderPrint, {
    summary((!!downloads())$count)
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0(input$package, ".zip")
    },
    content = function(out) {
      downloads_data <- tempfile("data", fileext = ".csv")
      on.exit(unlink(downloads_data))
      write.csv(downloads(), downloads_data, row.names = FALSE)
      
      ec <- newExpansionContext()
      ec$substituteMetaReactive(downloads, function() {
        metaExpr(readr::read_csv("data.csv", col_types = "Ddc"))
      })
      
      # Generate code
      code <- expandChain(.expansionContext = ec,
        output$summary(),
        output$plot()
      )
      
      buildRmdBundle(
        report_template = "report.Rmd",
        include_files = list("data.csv" = downloads_data),
        vars = list(pkgname = input$package, code = code),
        output_zip_path = out
      )
    }
  )
  observeEvent(input$plot_output_code, {
    # Display to user
    displayCodeModal(code)
  })
}

shinyApp(ui, server)