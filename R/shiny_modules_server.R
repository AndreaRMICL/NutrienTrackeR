selectDatabaseServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      input$foodSearchDatabase
    })
  })
}

personalDataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      validate(
        need(input$gender %in% c("male", "female"), "Please select a gender"),
        need(suppressWarnings(as.numeric(input$age) >= 0), "Please enter a valid age")
      )
      list(input$gender, input$age, 
           pregnancyStatus={"Pregnancy" %in% input$extraStatus},
           lactationStatus={"Lactation" %in% input$extraStatus})
    })
  })
}

foodSearchServer <- function(id, foodDatabase, nutrientNames) {
  moduleServer(id, function(input, output, session) {
    foodSearchResults <- reactive({
      if(input$foodSearchQuery == "") {
        get(foodDatabase(), food_composition_data)[,2]
      } else {
        findFoodName(keywords = trimws(unlist(strsplit(input$foodSearchQuery, split=";"))), 
                     food_database = foodDatabase(), ignore_case = TRUE)
      }
    })
    output$foodMatches <- renderTable({
      foods_search_results = foodSearchResults()
      if(input$sortByNutrient == TRUE & !is.null(input$sortingNutrient)) {
        allFoodsSorted <- subsetFoodRichIn(input$sortingNutrient, 
                                           n = nrow(get(foodDatabase(), food_composition_data)))
        sorted_food_names <- allFoodsSorted[,2]
        foods_search_results <- foods_search_results[order(match(foods_search_results, sorted_food_names))]
      }
      data.frame("Found foods" = foods_search_results)
      })
    observe({
      updateSelectizeInput(
        session,
        "sortingNutrient",
        choices = nutrientNames()
      )
    })
  })
}

dietInputServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      parsed_daily_diets <- lapply(list(input$food_day1,
                  input$food_day2,
                  input$food_day3,
                  input$food_day4,
                  input$food_day5,
                  input$food_day6,
                  input$food_day7),
             parse_input_day_diet)
      names(parsed_daily_diets) <- c("Day 1", "Day 2", "Day 3", "Day 4", "Day 5", "Day 6", "Day 7")
      parsed_daily_diets <- parsed_daily_diets[lengths(parsed_daily_diets) != 0]
      if(length(parsed_daily_diets) == 1) parsed_daily_diets <- parsed_daily_diets[[1]]
      validate(
        need(length(parsed_daily_diets) > 0, "Please enter diet for at least 1 day")
      )
      parsed_daily_diets
    })
  })
}

nutrientIntakeRequirementServer <- function(id, dietAnalysis, foodDatabase) {
  moduleServer(id, function(input, output, session) {
    output$intakePlot <- renderPlot({
      nutrientIntakePlot(dietAnalysis())
    })
  })
}

nutrientSourcesServer <- function(id, dietAnalysis, nutrientNames) {
  moduleServer(id, function(input, output, session) {
    observe({
      updateSelectizeInput(
        session,
        "targetNutrientSelection",
        choices = nutrientNames()
      )
    })
    output$sourcesPlot <- renderPlot({
      nutrientPiePlot(dietAnalysis(), input$targetNutrientSelection, as.numeric(input$maxFoods))
    })
    #reactive(input$targetNutrientSelection)
  })
}
