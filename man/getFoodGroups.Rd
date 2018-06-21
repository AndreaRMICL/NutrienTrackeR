\name{getFoodGroups}
\alias{getFoodGroups}
\title{Get the names of the food groups included in a given database}
\description{
This function returns the names of the different food groups included in a given 
database.
}
\usage{
getFoodGroups(food_database = "USDA")
}
\arguments{
  \item{food_database}{character vector indicating the food database to be used. 
  Possible values are: "USDA", "CIQUAL", "BEDCA".}
}
\value{
A vector of food groups. 
}
\examples{
## Load data
data(food_composition_data)

## Get food groups from USDA
getFoodGroups("USDA")

## Get food groups from CIQUAL
getFoodGroups("CIQUAL")

## Get food groups from BEDCA
getFoodGroups("BEDCA")
}
