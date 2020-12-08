NutrienTrackeRapp <- function(...) {
  ui <- fluidPage(
    foodSearchUI("foodSearch1")
  )
  server <- function(input, output, session) {
    foodSearchServer("foodSearch1")
  }
  shinyApp(ui, server, ...)
}
