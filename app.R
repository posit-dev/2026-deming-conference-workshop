library(shiny)
library(bslib)

ui <- page_sidebar(
  sidebar = sidebar(),
)

server <- function(input, output, session) {}

shinyApp(ui, server)
