\name{getNutrientNames}
\alias{getNutrientNames}
\title{Get the names of nutrients included in a given database}
\description{
This function returns the names of all nutrients included in a given database.
}
\usage{
getNutrientNames(food_database = "USDA")
}
\arguments{
  \item{food_database}{character vector indicating the food database to be used. 
  Possible values are: "USDA", "CIQUAL", "BEDCA", "CNF".}
}
\value{
A vector of nutrient names. 
}
\examples{
## Load data
data(food_composition_data)

## Get nutrient names from USDA
getNutrientNames("USDA")

## Get nutrient names from CIQUAL
getNutrientNames("CIQUAL")

## Get nutrient names from BEDCA
getNutrientNames("BEDCA")

## Get nutrient names from CNF
getNutrientNames("CNF")
}
