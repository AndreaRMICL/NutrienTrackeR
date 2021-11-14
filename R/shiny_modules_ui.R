selectDatabaseUI <- function(id) {
    selectInput(
        NS(id, "foodSearchDatabase"),
        "Food database to be used:",
        c("USDA", "CIQUAL", "BEDCA", "CNF"),
        selected = "USDA",
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
    )
}

genders <- c("male", "female")
names(genders) <- c("Male", "Female")

personalDataUI <- function(id) {
  tagList(      
    selectInput(
      NS(id, "gender"),
      "Gender:",
      genders,
      selected = "Male"
    ),
    textInput(
      NS(id, "age"),
      "Age:",
      "30"
    ),
    conditionalPanel(
      condition="input.gender == 'female'",
      selectInput(
        NS(id, "extraStatus"),
        "Pregnancy/lactation status:",
        c("Pregnancy", "Lactation"),
        multiple = TRUE
      ),
      ns=NS(id))
  )
}

foodSearchUI <- function(id) {
  fluidRow(
    column(3, 
           fluidRow(textInput(
             NS(id, "foodSearchQuery"), 
             "Search for foods:", 
             "")),
           checkboxInput(
             NS(id, "sortByNutrient"), 
             "Sort foods by nutrient"),
           conditionalPanel(
             condition="input.sortByNutrient == 1",
             selectInput(
               NS(id, "sortingNutrient"), 
               "Nutrient to sort by:",
               getNutrientNames("USDA")),
             ns = NS(id))),
    column(9,
           tableOutput(
             NS(id, "foodMatches")),
             style = "height:50vh;overflow-y:auto"))
}

dietInputUI <- function(id) {
#   list(fluidRow(
#     column(4, textAreaInput(NS(id, "food_day1"), "Diet for day 1", NULL)),
#     column(4, textAreaInput(NS(id, "food_day2"), "Diet for day 2", NULL)),
#     column(4, textAreaInput(NS(id, "food_day3"), "Diet for day 3", NULL))),
#     fluidRow(
#       column(4, textAreaInput(NS(id, "food_day4"), "Diet for day 4", NULL)),
#       column(4, textAreaInput(NS(id, "food_day5"), "Diet for day 5", NULL)),
#       column(4, textAreaInput(NS(id, "food_day6"), "Diet for day 6", NULL))),
#     fluidRow(
#       column(4, textAreaInput(NS(id, "food_day7"), "Diet for day 7", NULL)))
#   )
        tagList(
            fluidRow(
                selectizeInput(
                    NS(id, "eatenFood"),
                    "Select food:",
                    sort(food_composition_data$USDA[,2]),
                    multiple = TRUE,
                    options = list(maxOptions=99999, maxItems=1, plugins = list('restore_on_backspace'))
                ),
                dateInput(
                    NS(id, "foodDate"), 
                    "Meal Date"),
                selectizeInput(
                    NS(id, "foodTime"),
                    "Meal",
                    choices = c("Breakfast", "Lunch", "Supper", "Snacks", "Other/Misc."),
                    multiple = F, 
                    selected = "breakfast"
                ), 
                textInput(
                    NS(id ,"foodQuantity"), 
                    "Quantity", 
                    value = "100"
                ),
                selectInput(NS(id, "foodUnit"), 
                            "Unit", 
                            choices = c("grams", "cups", "tablespoons", "tea spoons"), selected = "grams")
            ),
            fluidRow(
                actionButton(
                    NS(id, "addFood"),
                    "Add food to my diet",
                    style="color: #fff; background-color: #337ab7; border-color: #211a23"
                ),
                actionButton(
                    NS(id, "resetFoods"),
                    "Reset entered diet",
                    style="color: #fff; background-color: #337ab7; border-color: #211a23")
            ),
            fluidRow(
                tableOutput(
                    NS(id, "inputDietTable")),
                style = "height:50vh;overflow-y:auto")
        )
}


nutrientsIntakeRequirementUI <- function(id) {
  plotOutput(NS(id, "intakePlot"))
}

nutrientSourcesUI <- function(id) {
  fluidRow(
    column(3, 
           fluidRow(
             selectInput(
               NS(id, "targetNutrientSelection"),
               "Select nutrient:",
               getNutrientNames("USDA")
             ),
             textInput(
               NS(id, "maxFoods"),
               "Maximum number of foods:",
               value = "5"
             )
           )
    ), 
    column(9,
           #verbatimTextOutput(NS(id, "sourcesPlot"))))
           plotOutput(NS(id, "sourcesPlot"))))
}

foodSelectionUI <- function(id) {
  tagList(
    fluidRow(
      selectizeInput(
        NS(id, "eatenFood"),
        "Select food:",
        sort(food_composition_data$USDA[,2]),
        multiple = TRUE,
        options = list(maxOptions=99999, maxItems=1, plugins = list('restore_on_backspace'))
      ),
      textInput(
        NS(id, "eatenGrams"),
        "Enter amount in grams:"
      )
    ),
    fluidRow(
      actionButton(
        NS(id, "addFood"),
        "Add food to my diet"
      ),
      actionButton(
        NS(id, "resetFoods"),
        "Reset entered diet")
    ),
    fluidRow(
      tableOutput(
        NS(id, "inputDiet")),
      style = "height:50vh;overflow-y:auto")
  )
}

foodListsUI <- function(id) {
    dataTableOutput(
        NS(id, "foodTable"))
}

