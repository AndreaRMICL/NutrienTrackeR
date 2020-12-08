NutrienTrackeRapp <- function(...) {
  ui <- fluidPage(
    titlePanel("NutrienTrackeR"),
    fluidRow(
      selectDatabaseUI("selectDatabase1"), 
      foodSearchUI("foodSearch1")
    ),
      dietInputUI("dietInput1")
  )
  server <- function(input, output, session) {
    foodDatabase <- selectDatabaseServer("selectDatabase1")
    dailyDiets <- dietInputServer("dietInputServer1")
    foodSearchServer("foodSearch1", foodDatabase)
    dietInputServer("dietInput1")
    #nutrientIntakeRequirementServer("nutrientsIntakeRequirement1", dailyDiets)
  }
  shinyApp(ui, server, ...)
}
