selectDatabaseUI <- function(id) {
  column(3,       
         selectInput(
           NS(id, "foodSearchDatabase"),
           "Food database to be used:",
           c("USDA", "CIQUAL", "BEDCA"),
           selected = "USDA",
           multiple = FALSE,
           selectize = TRUE,
           width = NULL,
           size = NULL
         ))
}

foodSearchUI <- function(id) {
  list(column(3, textInput(NS(id, "foodSearchQuery"), "Search for foods:", "")),
       column(6, htmlOutput(NS(id, "foodMatches"), style = "height:15vh;overflow-y:auto;overflow-x:auto")))
}

dietInputUI <- function(id) {
  list(fluidRow(
    column(3, textAreaInput(NS(id, "food_day1"), "Diet for day 1", NULL)),
    column(3, textAreaInput(NS(id, "food_day2"), "Diet for day 2", NULL)),
    column(3, textAreaInput(NS(id, "food_day3"), "Diet for day 3", NULL)),
    column(3, textAreaInput(NS(id, "food_day4"), "Diet for day 4", NULL))), 
    fluidRow(
      column(3, textAreaInput(NS(id, "food_day5"), "Diet for day 5", NULL)),
      column(3, textAreaInput(NS(id, "food_day6"), "Diet for day 6", NULL)),
      column(3, textAreaInput(NS(id, "food_day7"), "Diet for day 7", NULL))),
    fluidRow(
      column(12, verbatimTextOutput(NS(id, "dailyDiets")))
    ))
}

# nutrientsIntakeRequirementUI <- function(id) {
#   column(8, plotOutput(NS(id, "intakePlot")))
# }

debugUI <- function(id) {
       column(12, textOutput(NS(id, "dailyDiets")))
}