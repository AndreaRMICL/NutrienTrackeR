\name{dietBalance}
\alias{dietBalance}
\title{Nutrition calculator}
\description{
This function calculates the daily nutrient intake of an individual and
compares it with the NIH nutrient recommendations (recommended dietary
allowances (RDA) and tolerable upper intake levels (TUIL)).
}
\usage{
dietBalance(my_daily_food, food_database = "USDA", age = 27, gender = "female",
            pregnant = FALSE, lactation = FALSE, summary_report = TRUE)
}
\arguments{
  \item{my_daily_food}{matrix or a list of matrices, where each matrix reports all
  the foods eaten in a given day. The matrix must have two columns: 1)"food" (reporting food names) and
  2) "units" (reporting the number of units relative to 100 grams, e.g. 125 g -> 1.25).
  For more details, see the dataset "sample_diet_USDA".}
  \item{food_database}{character vector indicating the food database to be used.
  Possible values are: "USDA", "CIQUAL", "BEDCA".}
  \item{age}{numeric vector indicating age.}
  \item{gender}{character vector indicating gender (i.e. "female" or "male").}
  \item{pregnant}{logical constant indicating pregnancy status.}
  \item{lactation}{logical constant indicating lactation status.}
  \item{summary_report}{logical constant indicating whether a summary of results
  (e.g.nutrients whose daily intake level is below RDA or above TUIL) will be reported. }
}
\value{
A list, where the first element indicates daily nutrient intake; the second element
indicates the contribution of each food to the total intake level of each nutrient (as percentage);
and the second element reports the total intake level of each nutrient relative to the RDA (as percentage).
When my_daily_food is a list, the results correspond to an average daily intake.
}
\references{
https://ndb.nal.usda.gov/ndb/

http://www.bedca.net/

https://ciqual.anses.fr/
}
\examples{
## Load data
data(food_composition_data)
data(NIH_nutrient_recommendations)
data(nutrient_group)
data(sample_diet_USDA) ## contains an example of a one-week diet
day1 = sample_diet_USDA[[1]]

weekly_balanceF <- dietBalance(my_daily_food = sample_diet_USDA,
                              food_database = "USDA", age = 27, gender = "female")

day1_balanceF <- dietBalance(my_daily_food = day1,
                            food_database = "USDA", age = 27, gender = "female")

day1_balanceM <- dietBalance(my_daily_food = day1,
                            food_database = "USDA", age = 27, gender = "male")

day1_balanceF <- dietBalance(my_daily_food = day1,
                            food_database = "USDA", age = 27, gender = "male")
}
