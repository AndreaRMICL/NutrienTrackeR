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
                need(
                    input$gender %in% c("male", "female"),
                    "Please select a gender"
                ),
                need(
                    suppressWarnings(as.numeric(input$age) >= 0),
                    "Please enter a valid age"
                )
            )
            list(
                input$gender,
                input$age,
                pregnancyStatus = {
                    "Pregnancy" %in% input$extraStatus
                },
                lactationStatus = {
                    "Lactation" %in% input$extraStatus
                }
            )
        })
    })
}

foodSearchServer <- function(id, foodDatabase, nutrientNames) {
    moduleServer(id, function(input, output, session) {
        foodSearchResults <- reactive({
            validate(
                need(
                    input$foodSearchQuery != "",
                    "Please enter one or more keywords for search, separated by semicolons (;)"
                )
            )
            findFoodName(
                keywords = trimws(unlist(
                    strsplit(input$foodSearchQuery, split = ";")
                )),
                food_database = foodDatabase(),
                ignore_case = TRUE
            )
        })
        output$foodMatches <- renderTable({
            foods_search_results = foodSearchResults()
            if (input$sortByNutrient == TRUE &
                !is.null(input$sortingNutrient)) {
                allFoodsSorted <- subsetFoodRichIn(input$sortingNutrient,
                                                   n = nrow(get(
                                                       foodDatabase(), food_composition_data
                                                   )))
                sorted_food_names <- allFoodsSorted[, 2]
                foods_search_results <-
                    foods_search_results[order(match(foods_search_results, sorted_food_names))]
            }
            data.frame("Foods" = foods_search_results)
        })
        observe({
            updateSelectizeInput(session,
                                 "sortingNutrient",
                                 choices = nutrientNames())
        })
    })
}

dietInputServer <- function(id, foodDatabase) {
    moduleServer(id, function(input, output, session) {
        grams_conversion = setNames(c(1, 128, 15, 5),
                                    c("grams", "cups", "tablespoons", "tea spoons"))
        observe({
            updateSelectizeInput(
                session,
                "eatenFood",
                choices = sort(unname(
                    get(foodDatabase(), food_composition_data)[, 2]
                )),
                options = list(
                    maxOptions = 99999,
                    maxItems = 1,
                    plugins = list('restore_on_backspace')
                )
            )
        })
        FoodIntake <- reactiveValues(data = NULL)
        observeEvent(input$addFood, {
            validate(need(
                !is.null(input$eatenFood),
                "Please select a valid food"
            ))
            quantity_grams <-
                as.numeric(input$foodQuantity) * (grams_conversion[input$foodUnit])
            new_food_data <-
                food_composition_data[[foodDatabase()]][which(food_composition_data[[foodDatabase()]][, "food_name"] == input$eatenFood), ]
            kcals <-
                round((quantity_grams / 100) * as.numeric(new_food_data["Energy (kcal)"], 2))
            formatted_new_data <- data.frame(
                id = new_food_data[1],
                food_db = foodDatabase(),
                food_name = new_food_data[2],
                date = as.character(input$foodDate),
                meal = input$foodTime,
                quantity = input$foodQuantity,
                unit = input$foodUnit,
                quantity_grams = quantity_grams,
                kcals = kcals
            )
            FoodIntake$data <- rbind(FoodIntake$data, formatted_new_data)
            FoodIntake$data <- FoodIntake$data[order(FoodIntake$data$date), ]
        })
        observeEvent(input$resetFoods, {
            FoodIntake$data <- NULL
        })
        output$inputDietTable <- renderTable({
            FoodIntake$data
        }, options = list(pageLength = 10, autoWidth = TRUE))
        reactive({
            food_intake_splitdfs <- split(FoodIntake$data, FoodIntake$data$date)
            list_of_matrices <- vector(mode = "list", length = length(food_intake_splitdfs))
            for(i in 1:length(food_intake_splitdfs)) {
                foodIntake_singledf <- food_intake_splitdfs[[i]]
                food_names <- foodIntake_singledf$food_name
                food_units <- as.character(as.numeric(foodIntake_singledf$quantity_grams)/100)
                foodIntake_matrix <- matrix(c(food_names, food_units), ncol=2)
                colnames(foodIntake_matrix) <- c("food", "units")
                list_of_matrices[[i]] <- foodIntake_matrix
                return(list_of_matrices)
            }
        })
    })
}

nutrientIntakeRequirementServer <-
    function(id,
             dietAnalysis,
             foodDatabase,
             macronutrientsOnly = FALSE) {
        moduleServer(id, function(input, output, session) {
            output$intakePlot <- renderPlot({
                validate(need(
                    !inherits(tryCatch(
                        dietAnalysis(),
                        error = function(e)
                            e
                    ), "error"),
                    "One or more entered foods have invalid names."
                ))
                nutrientIntakePlot(dietAnalysis(), macronutrientsOnly = macronutrientsOnly)
            })
        })
    }

nutrientSourcesServer <- function(id, dietAnalysis, nutrientNames) {
    moduleServer(id, function(input, output, session) {
        observe({
            updateSelectizeInput(session,
                                 "targetNutrientSelection",
                                 choices = nutrientNames())
        })
        #output$sourcesPlot <- renderPrint(dietAnalysis())
        output$sourcesPlot <- renderPlot({
            validate(
                need(
                    !inherits(tryCatch(
                        dietAnalysis(),
                        error = function(e)
                            e
                    ), "error"),
                    "One or more entered foods have invalid names."
                ),
                need(
                    length(unique(rownames(
                        get("food_contribution", dietAnalysis())
                    ))) >= 3,
                    "Please enter at least two different foods."
                )
            )
            nutrientPiePlot(
                dietAnalysis(),
                input$targetNutrientSelection,
                as.numeric(input$maxFoods)
            )
        })
    })
}

foodSelectionServer <- function(id, foodDatabase) {
    moduleServer(id, function(input, output, session) {
        observe({
            updateSelectizeInput(
                session,
                "eatenFood",
                choices = sort(unname(
                    get(foodDatabase(), food_composition_data)[, 2]
                )),
                options = list(
                    maxOptions = 99999,
                    maxItems = 1,
                    plugins = list('restore_on_backspace')
                )
            )
        })
        oneDayDiet <-
            reactiveVal(value = matrix(c("", ""), ncol = 2), label = "oneDayDiet")
        observeEvent(input$addFood, {
            if (oneDayDiet()[1, 1] == "") {
                oneDayDiet(matrix(c(
                    input$eatenFood,
                    as.character(as.numeric(input$eatenGrams) / 100)
                ), ncol = 2))
            } else {
                oneDayDiet(rbind(
                    oneDayDiet(),
                    c(
                        input$eatenFood,
                        as.character(as.numeric(input$eatenGrams) / 100)
                    )
                ))
            }
        })
        observeEvent(input$resetFoods, {
            oneDayDiet(matrix(c("", ""), ncol = 2))
        })
        output$inputDiet <- renderTable({
            diet_with_colnames <- oneDayDiet()
            colnames(diet_with_colnames) <-
                c("Food", "Units (100 g)")
            diet_with_colnames
        })
        reactive({
            validate(need(
                oneDayDiet() != "",
                "Please enter diet data for a period of 24 hours"
            ))
            oneDayDiet()
        })
    })
}

foodListsServer <- function(id, foodDatabase, nutrientNames) {
    moduleServer(id, function(input, output, session) {
        foodTable <- reactive({
            data.frame(food_composition_data[foodDatabase()])
        })
        output$foodTable <- renderDataTable({
            food_table <- foodTable()
            colnames(food_table) <-
                c("FoodID",
                  "FoodName",
                  "FoodGroup",
                  "FoodDB",
                  nutrientNames())
            food_table
        }, options = list(pageLength = 10, autoWidth = TRUE))
    })
}
