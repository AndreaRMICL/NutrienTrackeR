\name{nutrientIntakePlot}
\alias{nutrientIntakePlot}
\title{Visualize nutrient intake levels}
\description{
This function generates a barplot of nutrient intake levels, based on the output
generated with the function dietBalance().
}
\usage{
nutrientIntakePlot(daily_intake, color_scale = c("salmon", "cornflowerblue",
                   "palegreen3"))
}
\arguments{
  \item{daily_intake}{list generated with the function dietBalance().}
  \item{color_scale}{character vector indicating the colors used to fill the bars,
  according to nutrient groups: macronutrient (first color), mineral (second color),
  vitamin (third color).}
}
\value{
A barplot of nutrient intake levels, expressed as a percentage of RDA.
}
\examples{
## Load data
data(food_composition_data)
data(NHI_nutrient_recommendations)
data(nutrient_group)
data(sample_diet_USDA) ## contains an example of a one-week diet

## Get daily intake
balanceF <- dietBalance(my_daily_food = sample_diet_USDA,
                       food_database = "USDA", age = 27, gender = "female")

## Generate plot
nutrientIntakePlot(daily_intake = balanceF)
}
