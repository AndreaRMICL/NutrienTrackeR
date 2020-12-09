NutrienTrackeRapp <- function(...) {
  ui <- fluidPage(
    titlePanel("NutrienTrackeR"),
    fluidRow(
      selectDatabaseUI("selectDatabase1"), 
      foodSearchUI("foodSearch1")
    ),
      dietInputUI("dietInput1"), 
    fluidRow(
      nutrientsIntakeRequirementUI("nutrientsIntakeRequirement1")
    )
  )
  server <- function(input, output, session) {
    foodDatabase <- selectDatabaseServer("selectDatabase1")
    dailyDiets <- dietInputServer("dietInput1")
    foodSearchServer("foodSearch1", foodDatabase)
    nutrientIntakeRequirementServer("nutrientsIntakeRequirement1", dailyDiets, foodDatabase)
  }
  shinyApp(ui, server, ...)
}
