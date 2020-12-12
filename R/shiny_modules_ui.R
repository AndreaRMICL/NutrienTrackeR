selectDatabaseUI <- function(id) {
  selectInput(
    NS(id, "foodSearchDatabase"),
    "Food database to be used:",
    c("USDA", "CIQUAL", "BEDCA"),
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
  list(fluidRow(
    column(4, textAreaInput(NS(id, "food_day1"), "Diet for day 1", NULL)),
    column(4, textAreaInput(NS(id, "food_day2"), "Diet for day 2", NULL)),
    column(4, textAreaInput(NS(id, "food_day3"), "Diet for day 3", NULL))),
    fluidRow(
      column(4, textAreaInput(NS(id, "food_day4"), "Diet for day 4", NULL)),
      column(4, textAreaInput(NS(id, "food_day5"), "Diet for day 5", NULL)),
      column(4, textAreaInput(NS(id, "food_day6"), "Diet for day 6", NULL))),
    fluidRow(
      column(4, textAreaInput(NS(id, "food_day7"), "Diet for day 7", NULL)))
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
           plotOutput(NS(id, "sourcesPlot"))))
}
