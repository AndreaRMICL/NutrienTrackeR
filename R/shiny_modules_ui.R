# shinyUI(fluidPage(
#   headerPanel("NutrienTrackeR"),
#   sidebarPanel(
#     textInput("foodSearchQuery", "Search for foods:", ""),
#     selectInput(
#       "foodSearchDatabase",
#       "Database to search for foods:",
#       c("USDA", "CIQUAL", "BEDCA"),
#       selected = "USDA",
#       multiple = FALSE,
#       selectize = TRUE,
#       width = NULL,
#       size = NULL
#     )
#   ),
#   mainPanel(
#     htmlOutput("foodMatches")
#   )
# ))

foodSearchUI <- function(id) { 
  tagList(
    headerPanel("NutrienTrackeR"),
    sidebarPanel(
      textInput(NS(id, "foodSearchQuery"), "Search for foods:", ""),
      selectInput(
        NS(id, "foodSearchDatabase"),
        "Database to search for foods:",
        c("USDA", "CIQUAL", "BEDCA"),
        selected = "USDA",
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      )
    ),
    mainPanel(
      htmlOutput(NS(id, "foodMatches"))
    )
  )}