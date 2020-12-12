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
          tabPanel("Nutrient intake assessment", nutrientsIntakeRequirementUI("nutrientsIntakeRequirement1")),
          tabPanel("Nutrient sources assessment", nutrientSourcesUI("selectNutrient1"))
                   # fluidRow(column(3, selectNutrientUI("selectNutrient1")),
                   # column(9, nutrientSourcesUI("nutrientSources1")))
                   # )
        )
      )
    )
  )
  server <- function(input, output, session) {
    foodDatabase <- selectDatabaseServer("selectDatabase1")
    dailyDiets <- dietInputServer("dietInput1")
    personalData <- personalDataServer("personalData1")
    dietAnalysis <- reactive({
      dietBalance(dailyDiets(), food_database=foodDatabase(), 
                  age=as.numeric(personalData()[[2]]), gender=personalData()[[1]],
                  pregnant=personalData()[["pregnancyStatus"]],
                  lactation=personalData()[["lactationStatus"]])
    })
    nutrientNames <- reactive({
      getNutrientNames(foodDatabase())
    })
    nutrientSourcesServer("selectNutrient1", dietAnalysis, nutrientNames)
    foodSearchServer("foodSearch1", foodDatabase, nutrientNames)
    nutrientIntakeRequirementServer("nutrientsIntakeRequirement1", dietAnalysis, foodDatabase)
    #nutrientSourcesServer("nutrientSources1", dietAnalysis, selectedNutrient)
  }
  shinyApp(ui, server, ...)
}
