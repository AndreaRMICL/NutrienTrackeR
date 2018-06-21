\name{subsetFoodRichIn}
\alias{subsetFoodRichIn}
\title{Find nutrient-rich foods}
\description{
This function selects the foods with the highest amount of a given nutrient from 
a food composition database. 
}
\usage{
subsetFoodRichIn(nutrient_name, food_database = "USDA", food_group = NULL, n = 10)
}
\arguments{
  \item{nutrient_name}{character vector indicating the name of the nutrient of interest.}
  \item{food_database}{character vector indicating the food database to be used. 
  Possible values are: "USDA", "CIQUAL", "BEDCA".}
  \item{food_group}{character vector indicating the food group(s) of interest. 
  NULL indicates that all food groups are considered. }
  \item{n}{numeric value indicating the number of foods to be selected.}
}
\value{
A subset from the food composition database containing the foods with the highest amount
of the nutrient of interest.
}
\examples{
## Load data
data(food_composition_data)

## Get foods rich in niacin
subsetFoodRichIn(nutrient_name = "Niacin (mg)", food_database = "USDA", n = 5)
subsetFoodRichIn(nutrient_name = "Niacin (mg)", food_database = "CIQUAL", n = 5)
subsetFoodRichIn(nutrient_name = "Niacin (mg)", food_database = "BEDCA", n = 5)

## Get foods rich in niacin from CIQUAL within the group "diary products and deserts"
subsetFoodRichIn(nutrient_name = "Niacin (mg)", food_database = "CIQUAL", n = 5, 
                 food_group = "dairy products and deserts")
}
