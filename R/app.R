NutrienTrackeRapp <- function(...) {
  ui <- fluidPage(
    titlePanel("NutrienTrackeR"),
    sidebarLayout(
      sidebarPanel(
        personalDataUI("personalData1"),
        selectDatabaseUI("selectDatabase1")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Food searches", foodSearchUI("foodSearch1")),
          tabPanel("Diet input", dietInputUI("dietInput1")),
          tabPanel("Nutrient intake assessment", nutrientsIntakeRequirementUI("nutrientsIntakeRequirement1"))
        )
      )
    )
  )
  server <- function(input, output, session) {
    foodDatabase <- selectDatabaseServer("selectDatabase1")
    dailyDiets <- dietInputServer("dietInput1")
    personalData <- personalDataServer("personalData1")
    dietAnalysis <- reactive({
      dietBalance(dailyDiets(), food_database=foodDatabase(), age=as.numeric(personalData()[[2]]), gender=personalData()[[1]])
    })
    foodSearchServer("foodSearch1", foodDatabase)
    nutrientIntakeRequirementServer("nutrientsIntakeRequirement1", dietAnalysis, foodDatabase)
  }
  shinyApp(ui, server, ...)
}
