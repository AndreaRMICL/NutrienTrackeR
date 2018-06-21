######################### TEST NutrienTrackeR ###################################
library(RUnit)

## Load data
data(food_composition_data)
data(NHI_nutrient_recommendations)
data(nutrient_group)
data(sample_diet_USDA)

#### Test dietBalance ####

balance1 = dietBalance(my_daily_food = sample_diet_USDA, 
                       food_database = "USDA", age = 27, gender = "female")
balance2 = dietBalance(my_daily_food = sample_diet_USDA, 
                       food_database = "USDA", age = 27, gender = "female", 
                       pregnant = TRUE)
balance3 = dietBalance(my_daily_food = sample_diet_USDA, 
                       food_database = "USDA", age = 27, gender = "female", 
                       lactation = TRUE)
balance4 = dietBalance(my_daily_food = sample_diet_USDA, 
                       food_database = "USDA", age = 27, gender = "male")
balance5 = dietBalance(my_daily_food = sample_diet_USDA, 
                       food_database = "USDA", age = 7, gender = "male")
balance6 = dietBalance(my_daily_food = sample_diet_USDA, 
                       food_database = "USDA", age = 7, gender = "female")
    
## Tests
test.dietBalance <- function() {

    checkTrue(is.list(balance1))
    checkTrue(is.list(balance2))
    checkTrue(is.list(balance3))
    checkTrue(is.list(balance4))
    checkTrue(is.list(balance5))
    checkTrue(is.list(balance6))
    checkTrue(identical(balance6$diff_intake, balance5$diff_intake))

}
test.dietBalance()

################################################################################

#### getNutrientNames ####

## Tests
test.getNutrientNames <- function() {
    checkTrue(is.vector(getNutrientNames(food_database = "USDA")))
    checkTrue(is.vector(getNutrientNames(food_database = "CIQUAL")))
}
test.getNutrientNames()

################################################################################

#### subsetFoodRichIn ####
nutrients_USDA = getNutrientNames(food_database = "USDA")
nutrients_CIQUAL = getNutrientNames(food_database = "CIQUAL")

## Tests
test.subsetFoodRichIn <- function() {
    checkTrue(is.matrix(subsetFoodRichIn(nutrient_name = nutrients_USDA[1], food_database = "USDA")))
    checkTrue(is.matrix(subsetFoodRichIn(nutrient_name = nutrients_CIQUAL[1], food_database = "CIQUAL")))
}
test.subsetFoodRichIn()

################################################################################

#### getFoodGroups ####

## Tests
test.getFoodGroups <- function() {
    checkTrue(is.vector(getFoodGroups(food_database = "USDA")))
    checkTrue(is.vector(getFoodGroups(food_database = "CIQUAL")))
}
test.getFoodGroups()
