foodSearchServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    foodSearchResults <- reactive({
      if(input$foodSearchQuery == "") {
        NULL
      } else {
        findFoodName(keywords = input$foodSearchQuery, food_database = input$foodSearchDatabase, 
                     ignore_case = TRUE)
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
# 
# # Define server logic required to generate and plot a random distribution
# shinyServer(function(input, output) {
#   
#   # Expression that generates a plot of the distribution. The expression
#   # is wrapped in a call to renderPlot to indicate that:
#   #
#   #  1) It is "reactive" and therefore should be automatically 
#   #     re-executed when inputs change
#   #  2) Its output type is a plot 
#   #
#   foodSearchResults <- reactive({
#     if(input$foodSearchQuery == "") {
#       NULL
#     } else {
#       findFoodName(keywords = input$foodSearchQuery, food_database = input$foodSearchDatabase, 
#                    ignore_case = TRUE)
#     }
#   })
#   output$foodMatches <- renderUI({
#     foodSearchResults_output <- foodSearchResults()
#     if(!is.null(foodSearchResults_output)) {
#         HTML(paste(c(if(foodSearchResults_output[1] != "Could not find any food with the current keywords") {
#           paste(length(foodSearchResults()), " results found:")}, foodSearchResults()), 
#                    sep = "", collapse = '<br/>'))
#       
#     }
#   })
# })