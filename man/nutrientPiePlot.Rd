\name{nutrientPiePlot}
\alias{nutrientPiePlot}
\title{Visualize the main foods contributing to the intake levels of a nutrient}
\description{
This function generates a pie-chart of the main foods contributing to the intake
levels of a nutrient, based on the output from the function dietBalance().
}
\usage{
nutrientPiePlot(daily_intake, nutrient_name = "Vitamin B-12 (ug)", n = 10)
}
\arguments{
  \item{daily_intake}{list generated with the function dietBalance().}
  \item{nutrient_name}{character vector indicating the name of the nutrient of interest
  (e.g. "Vitamin B-12 (ug)".}
  \item{n}{maximum number of foods to be displayed.}
}
\value{
A pie-chart showing the contribution (as percentage) of each food to the intake level
of a given nutrient.}

\examples{
## Load data
data(food_composition_data)
data(NHI_nutrient_recommendations)
data(nutrient_group)
data(sample_diet_USDA) ## contains an example of a one-week diet

## Get daily intake
balanceF <- dietBalance(my_daily_food = sample_diet_USDA,
                        food_database = "USDA", age = 27, gender = "female")

## Generate plots
nutrientPiePlot(daily_intake = balanceF, nutrient_name = "Fiber, total dietary (g)")
nutrientPiePlot(daily_intake = balanceF, nutrient_name = "Magnesium, Mg (mg)")
nutrientPiePlot(daily_intake = balanceF, nutrient_name = "Calcium, Ca (mg)")
nutrientPiePlot(daily_intake = balanceF, nutrient_name = "Niacin (mg)")
}
