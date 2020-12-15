NutrienTrackeRapp <- function() {
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
          tabPanel("Nutrient sources assessment", nutrientSourcesUI("selectNutrient1")),
          tabPanel("One-day diet fast assessment", fluidPage(
            column(6, foodSelectionUI("foodSelection1")),
            column(6, nutrientsIntakeRequirementUI("nutrientsIntakeRequirement2"))
          ))
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
    oneDayDiet <- foodSelectionServer("foodSelection1", foodDatabase)
    dietAnalysisOneDay <- reactive({
      dietBalance(oneDayDiet(), food_database=foodDatabase(), 
                  age=as.numeric(personalData()[[2]]), gender=personalData()[[1]],
                  pregnant=personalData()[["pregnancyStatus"]],
                  lactation=personalData()[["lactationStatus"]])
    })
    foodSearchServer("foodSearch1", foodDatabase, nutrientNames)
    nutrientIntakeRequirementServer("nutrientsIntakeRequirement1", 
                                    dietAnalysis, foodDatabase)
    nutrientIntakeRequirementServer("nutrientsIntakeRequirement2", 
                                    dietAnalysisOneDay, foodDatabase, 
                                    macronutrientsOnly = TRUE)
  }
  shinyApp(ui, server)
}
