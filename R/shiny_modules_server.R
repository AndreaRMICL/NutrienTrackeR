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
      validate(
        need(input$foodSearchQuery != "", "Please enter one or more keywords for search, separated by semicolons (;)")
      )
      findFoodName(keywords = trimws(unlist(strsplit(input$foodSearchQuery, split=";"))), 
                   food_database = foodDatabase(), ignore_case = TRUE)
    })
    output$foodMatches <- renderTable({
      foods_search_results = foodSearchResults()
      if(input$sortByNutrient == TRUE & !is.null(input$sortingNutrient)) {
        allFoodsSorted <- subsetFoodRichIn(input$sortingNutrient, 
                                           n = nrow(get(foodDatabase(), food_composition_data)))
        sorted_food_names <- allFoodsSorted[,2]
        foods_search_results <- foods_search_results[order(match(foods_search_results, sorted_food_names))]
      }
      data.frame("Foods" = foods_search_results)
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

nutrientIntakeRequirementServer <- function(id, dietAnalysis, foodDatabase, macronutrientsOnly = FALSE) {
  moduleServer(id, function(input, output, session) {
    output$intakePlot <- renderPlot({
      validate(
        need(!inherits(tryCatch(dietAnalysis(), error=function(e) e), "error"),
             "One or more entered foods have invalid names.")
      )
      nutrientIntakePlot(dietAnalysis(), macronutrientsOnly = macronutrientsOnly)
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
      validate(
        need(!inherits(tryCatch(dietAnalysis(), error=function(e) e), "error"),
             "One or more entered foods have invalid names."),
        need(length(unique(rownames(get("food_contribution", dietAnalysis())))) >= 3, 
             "Please enter at least two different foods.")
      )
      nutrientPiePlot(dietAnalysis(), input$targetNutrientSelection, as.numeric(input$maxFoods))
    })
    #reactive(input$targetNutrientSelection)
  })
}

foodSelectionServer <- function(id, foodDatabase) {
  moduleServer(id, function(input, output, session) {
    observe({
      updateSelectizeInput(
        session,
        "eatenFood",
        choices = sort(unname(get(foodDatabase(), food_composition_data)[,2])),
        options = list(maxOptions=99999, maxItems=1, plugins = list('restore_on_backspace'))
      )
    })
    oneDayDiet <- reactiveVal(value=matrix(c("",""), ncol=2), label="oneDayDiet")
    observeEvent(input$addFood, {
      if(oneDayDiet()[1,1]=="") {
        oneDayDiet(matrix(c(input$eatenFood, as.character(as.numeric(input$eatenGrams)/100)), ncol=2))
      } else {
        oneDayDiet(rbind(oneDayDiet(), c(input$eatenFood, as.character(as.numeric(input$eatenGrams)/100))))
      }
    })
    observeEvent(input$resetFoods, {
        oneDayDiet(matrix(c("",""), ncol=2))
    })
    output$inputDiet <- renderTable({
      diet_with_colnames <- oneDayDiet()
      colnames(diet_with_colnames) <- c("Food", "Units (100 g)")
      diet_with_colnames
    })
    reactive({
      validate(
        need(oneDayDiet() != "", "Please enter diet data for a period of 24 hours")
      )
      oneDayDiet()
      })
  })
}
