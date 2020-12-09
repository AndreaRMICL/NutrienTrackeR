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
      selected = "Male",
      multiple = FALSE,
      selectize = TRUE,
      width = NULL,
      size = NULL
    ),
    textInput(
      NS(id, "age"),
      "Age:",
      "",
      width = NULL
    )
  )
}

foodSearchUI <- function(id) {
  fluidRow(column(3, textInput(NS(id, "foodSearchQuery"), "Search for foods:", "")),
       column(9, htmlOutput(NS(id, "foodMatches"), style = "height:30vh;overflow-y:auto")))
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
 column(8, plotOutput(NS(id, "intakePlot")))
}
