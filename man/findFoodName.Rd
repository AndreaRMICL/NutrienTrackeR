\name{findFoodName}
\alias{findFoodName}
\title{Find food names based on keywords}
\description{
This function allows finding food names based on query keywords.
}
\usage{
findFoodName(keywords, food_database = "USDA", food_group = NULL, ignore_case = TRUE)
}
\arguments{
  \item{keywords}{character vector of containing one or several keywords.
  For example, "peppers, "green" and "raw" would be good keywords for
  the food: "Peppers, sweet, green, raw".}
  \item{food_database}{character vector indicating the food database to be used.
  Possible values are: "USDA", "CIQUAL", "BEDCA", "CNF".}
  \item{food_group}{character vector indicating the food groups that are likely to
  contain the food of interest. NULL indicates that the search is done using all
  food groups. }
  \item{ignore_case}{logical constant indicating whether the search is case sensitive.}
}
\value{
A vector of matched food names.}

\examples{
## Load data
data(food_composition_data)

findFoodName(keywords = c("Rice", "brown", "raw"), food_database = "USDA")
findFoodName(keywords = c("Rice", "brown", "raw"), food_database = "CIQUAL")
findFoodName(keywords = c("Rice", "brown", "raw"), food_database = "BEDCA")
findFoodName(keywords = c("rice", "brown"), food_database = "CNF")
}
