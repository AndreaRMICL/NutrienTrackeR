## getNutrientNames ##
getNutrientNames = function(food_database = "USDA") {

    ## Check food_database
    if (!(food_database %in% names(food_composition_data))) {
        stop ("Invalid food_database")
    }

    ## Select data
    database_food_composition = food_composition_data[names(food_composition_data) == food_database][[1]]
    idx = grep("[(]", colnames(database_food_composition))
    return(colnames(database_food_composition[, idx = grep("[(]", colnames(database_food_composition))]))

}
################################################################################

## getFoodGroups ##
getFoodGroups = function(food_database = "USDA") {

    ## Check food_database
    if (!(food_database %in% names(food_composition_data))) {
        stop ("Invalid food_database")
    }

    ## Select data
    database_food_composition = food_composition_data[names(food_composition_data) == food_database][[1]]
    return(unique(database_food_composition[, "food_group"]))
}
################################################################################

## findFooodName ##
findFoodName = function(keywords, food_database = "USDA", food_group = NULL,
                        ignore_case = FALSE) {

    ## Check food_database
    if (!(food_database %in% names(food_composition_data))) {
        stop ("Invalid food_database")
    }

    ## Select data
    database_food_composition = food_composition_data[names(food_composition_data) == food_database][[1]]

    if (!is.null(food_group)) {
        food_group = intersect(food_group, database_food_composition[, "food_group"])

        if (length(food_group) == 0) {
            stop ("Invalid food_group")
        }
        idx = unique(unlist(sapply(food_group,find_idx, database_food_composition[, "food_group"])))
        search_space = database_food_composition[idx, ]
    } else {
        search_space = database_food_composition
    }

    idx_food = grep(paste(keywords, collapse = ".*"), search_space[, "food_name"],
                    ignore.case = ignore_case)

    if (length(idx_food) == 0) {
        return("Could not find any food with the current keywords")
    }

    food_candidates = search_space[idx_food, "food_name"]

    return(food_candidates)
}
################################################################################

## subsetFoodRichIn ##
subsetFoodRichIn = function(nutrient_name, food_database = "USDA", food_group = NULL,
                            n = 10) {

    ## Check food_database
    if (!(food_database %in% names(food_composition_data))) {
        stop ("Invalid food_database")
    }

    ## Select data
    database_food_composition = food_composition_data[names(food_composition_data) == food_database][[1]]
    idx_na = which(is.na(database_food_composition))
    if (length(idx_na) > 0) {
        database_food_composition[idx_na] = 0
    }

    nutrient_name = nutrient_name[1] # in case the user eneters more tthan one

    if(!(nutrient_name %in% colnames(database_food_composition))) {
        stop ("Invalid nutrient name")
    }

    if (!is.null(food_group)) {
        food_group = intersect(food_group, database_food_composition[, "food_group"])

        if (length(food_group) == 0) {
            stop ("Invalid food_group")
        }
        idx = unique(unlist(sapply(food_group,find_idx, database_food_composition[, "food_group"])))
        nutrient_data = database_food_composition[idx, ]
    } else {
        nutrient_data = database_food_composition
    }
    nutrient_data = nutrient_data[order(as.numeric(nutrient_data[, nutrient_name]),
                                        decreasing = TRUE), ][1:n, ]
    return(nutrient_data)
}
