selectDatabaseServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(input$foodSearchDatabase)
  })
}

personalDataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(list(input$gender, input$age))
  })
}

foodSearchServer <- function(id, foodDatabase) {
  moduleServer(id, function(input, output, session) {
    foodSearchResults <- reactive({
      if(input$foodSearchQuery == "") {
        NULL
      } else {
        findFoodName(keywords = trimws(unlist(strsplit(input$foodSearchQuery, split=";"))), 
                     food_database = foodDatabase(), ignore_case = TRUE)
      }
    })
    output$foodMatches <- renderUI({
      foodSearchResults_output <- foodSearchResults()
      if(!is.null(foodSearchResults_output)) {
        HTML(paste(c(if(foodSearchResults_output[1] != "Could not find any food with the current keywords") {
          paste(length(foodSearchResults()), " results found:")}, foodSearchResults()), 
          sep = "", collapse = '<br/>'))
        
      }
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
      parsed_daily_diets
    })
  })
}

nutrientIntakeRequirementServer <- function(id, dietAnalysis, foodDatabase) {
  moduleServer(id, function(input, output, session) {
    #diet_balance <- reactive(dietBalance(parsedDietInput(), food_database=foodDatabase(), age=27, gender="female"))
    output$intakePlot <- renderPlot({
      nutrientIntakePlot(dietAnalysis())
    })
  })
}
