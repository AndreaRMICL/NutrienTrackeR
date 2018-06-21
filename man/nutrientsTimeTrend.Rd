\name{nutrientsTimeTrend}
\alias{nutrientsTimeTrend}
\title{Visualize time trends of nutrient intake levels}
\description{
This function allows visualizing time trends of intake levels of one or several
nutrients.
}
\usage{
nutrientsTimeTrend(my_daily_food, food_database = "USDA", nutrients = NULL,
                   age = 27, gender = "female", pregnant = FALSE, lactation = FALSE)
}
\arguments{
  \item{my_daily_food}{matrix or list of matrices, where each matrix reports a
  daily intake. The matrix must have two columns: 1)"food" (reporting food names) and
  2) "units" (reporting the number of units relative to 100 grams, e.g. 125 g -> 1.25).}
  \item{food_database}{character vector indicating the food database to be used.
  Possible values are: "USDA", "CIQUAL", "BEDCA".}
  \item{nutrients}{character vector indicating the subset of nutrients that will be
  displayed. NULL indicates that all nutrients will be displayed. }
  \item{age}{numeric vector indicating age.}
  \item{gender}{character vector indicating gender (i.e. "female" or "male").}
  \item{pregnant}{logical constant indicating pregnancy status.}
  \item{lactation}{logical constant indicating lactation status.}
}
\value{
A timeseries plot displaying nutrient intake levels against time.}
\examples{
## Load data
data(food_composition_data)
data(NIH_nutrient_recommendations)
data(nutrient_group)
data(sample_diet_USDA) ## contains an example of a one-week diet

## Generate plots
nutrientsTimeTrend(my_daily_food = sample_diet_USDA, food_database = "USDA",
                   age = 27, gender = "female")

nutrientsTimeTrend(my_daily_food = sample_diet_USDA, food_database = "USDA",
                   nutrients = c("Calcium, Ca (mg)", "Iron, Fe (mg)"), age = 27,
                   gender = "female")
}
