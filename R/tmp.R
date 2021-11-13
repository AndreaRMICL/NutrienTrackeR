head(food_composition_data$USDA)

cnf_foodName <- read.csv("./data/csvs/FOOD NAME.csv", header = TRUE)
cnf_FoodGroup <- read.csv("./data/csvs/FOOD GROUP.csv", header=TRUE)
cnf_nutrientAmount <- read.csv("./data/csvs/NUTRIENT AMOUNT.csv", header=TRUE)
cnf_nutrientName <- read.csv("./data/csvs/NUTRIENT NAME.csv", header=TRUE)
cnf_nutrientName[, "NutrientUnit"] <- gsub("\xb5g", "ug", cnf_nutrientName[, "NutrientUnit"])
cnf_nutrientName[, "NutrientName"] <- str_to_title(cnf_nutrientName[, "NutrientName"])
nutrientsWithUnits <- paste(cnf_nutrientName[, "NutrientName"], " (", cnf_nutrientName[, "NutrientUnit"], ")", sep="")

nutrientsWithUnits_replaced <- nutrientsWithUnits
nutrientsWithUnits_replaced[3] <- nutrient_group[2,1]
nutrientsWithUnits_replaced[22] <- nutrient_group[3,1]
nutrientsWithUnits_replaced[23] <- nutrient_group[5,1]
nutrientsWithUnits_replaced[24] <- nutrient_group[10,1]
nutrientsWithUnits_replaced[25] <- nutrient_group[11,1]
nutrientsWithUnits_replaced[26] <- nutrient_group[14,1]
nutrientsWithUnits_replaced[27] <- nutrient_group[17,1]
nutrientsWithUnits_replaced[28] <- nutrient_group[18,1]
nutrientsWithUnits_replaced[29] <- nutrient_group[16,1]
nutrientsWithUnits_replaced[30] <- nutrient_group[7,1]
nutrientsWithUnits_replaced[31] <- nutrient_group[12,1]
nutrientsWithUnits_replaced[32] <- nutrient_group[15,1]
nutrientsWithUnits_replaced[34] <- nutrient_group[21,1]
nutrientsWithUnits_replaced[37] <- nutrient_group[24,1]
nutrientsWithUnits_replaced[40] <- nutrient_group[23,1]
nutrientsWithUnits_replaced[47] <- nutrient_group[22,1]
nutrientsWithUnits_replaced[50] <- nutrient_group[30,1]
nutrientsWithUnits_replaced[52] <- nutrient_group[32,1]
nutrientsWithUnits_replaced[57] <- nutrient_group[33,1]
nutrientsWithUnits_replaced[58] <- nutrient_group[25]
nutrientsWithUnits_replaced[61] <- nutrient_group[31]

nutrientsWithUnits_replaced[c(3, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 34,
                              37, 40, 47, 50, 52, 57, 58, 61)]

nutrientsWithUnits[c(3, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 34,
                     37, 40, 47, 50, 52, 57, 58, 61)]

cnf_nutrientName_original <- read.csv("./data/csvs/NUTRIENT NAME.csv", header=TRUE)
nutrientsOriginal_noWaterNoEnergy <- cnf_nutrientName_original[, "NutrientName"][-c(5, 14, 19)]
nutrientsWithUnits_replaced_noWaterNoEnergy <- nutrientsWithUnits_replaced[-c(5, 14, 19)]


CNF <- data.frame(matrix(ncol = 6 + length(nutrientsWithUnits_replaced_noWaterNoEnergy), nrow = nrow(cnf_foodName)))
colnames(CNF) <- c("food_id", "food_name", "food_group", "food_db", "Water (g)", 
                   "Energy (kcal)", nutrientsWithUnits_replaced_noWaterNoEnergy)
rownames(CNF) <- cnf_foodName[, "FoodID"]

for(i in 1:nrow(CNF)) {
    food_ID <- cnf_foodName[i, "FoodID"]
    food_Name <- cnf_foodName[i, "FoodDescription"]
    foodGroup_ID <- cnf_foodName[i, "FoodGroupID"]
    food_Group <- cnf_FoodGroup[which(cnf_FoodGroup[, "FoodGroupID"] == foodGroup_ID), "FoodGroupName"]
    foodWater <- cnf_nutrientAmount[which(cnf_nutrientAmount[, "FoodID"] == food_ID &
                                              cnf_nutrientAmount[, "NutrientID"] == 255), "NutrientValue"]
    foodEnergyKcal <- cnf_nutrientAmount[which(cnf_nutrientAmount[, "FoodID"] == food_ID &
                                                   cnf_nutrientAmount[, "NutrientID"] == 208), "NutrientValue"]
    nutrientsValues <- numeric(length(nutrientsWithUnits_replaced_noWaterNoEnergy))
    for(j in 1:length(nutrientsWithUnits_replaced_noWaterNoEnergy)) {
        nutrientID <- cnf_nutrientName_original[which(cnf_nutrientName_original[,"NutrientName"] == nutrientsOriginal_noWaterNoEnergy[j]),
                                                "NutrientID"]
        if (sum(cnf_nutrientAmount[,"FoodID"] == food_ID & cnf_nutrientAmount[, "NutrientID"] == nutrientID) > 0) {
            nutrientsValues[j] <- cnf_nutrientAmount[cnf_nutrientAmount[,"FoodID"] == food_ID & cnf_nutrientAmount[, "NutrientID"] == nutrientID, 
                                                     "NutrientValue"]
        } else {
            nutrientsValues[j] <- 0
        }
    }
    CNF[i, 1] <- food_ID
    CNF[i, 2] <- food_Name
    CNF[i, 3] <- food_Group
    CNF[i, 4] <- "CNF"
    CNF[i, 5] <- as.numeric(foodWater)
    CNF[i, 6] <- as.numeric(foodEnergyKcal)
    for(column in 1:length(nutrientsValues)) {
        CNF[i, column+6] <- nutrientsValues[column]
    }
}



