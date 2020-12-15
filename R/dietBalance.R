globalVariables(c("food_composition_data", "NIH_nutrient_recommendations", "nutrient_group"))

## dietBalance ##
dietBalance = function(my_daily_food, food_database = "USDA",
                       age = 27, gender = "female", pregnant = FALSE,
                       lactation = FALSE, summary_report = TRUE) {

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
    nutrient_requirements = NIH_nutrient_recommendations$RDA
    nutrient_upperlevels =  NIH_nutrient_recommendations$TUIL

    ## Get common columns from nutrient_requirements and database_food_composition (from environment)
    common_nutrients = intersect(colnames(nutrient_requirements), colnames(database_food_composition))
    nutrient_requirements = nutrient_requirements[, common_nutrients]
    food_composition = apply(database_food_composition[, -c(1:4)], 2, as.numeric) # select numeric columns
    rownames(food_composition) = database_food_composition[, "food_name"]

    ## Get TUIL as % of requirements
    common_nutrients2 = intersect(colnames(nutrient_requirements), colnames(nutrient_upperlevels))
    nutrient_upperlevels = (nutrient_upperlevels[, common_nutrients2]/nutrient_requirements[, common_nutrients2])*100

    ## Daily_food must be a list or matrix, but a character vector of length 2 
    ## will also be accepted and converted to a 1-row matrix
    if (is.character(my_daily_food) &
        is.vector(my_daily_food) &
        length(my_daily_food) == 2) {
        my_daily_food = matrix(my_daily_food, ncol=2)
        colnames(my_daily_food) = c("food", "units")
    } 
    if (!is.list(my_daily_food) & !is.matrix(my_daily_food)) {
        stop("daily_food must be a list or a matrix")
    }
    if (is.matrix(my_daily_food)) {
        my_daily_food = list(my_daily_food)
    }

    daily_food = do.call(rbind, my_daily_food)
    dup_idx = duplicated(daily_food[, 1])

    if (sum(dup_idx) > 1) {# there are duplicates
        dup_names = unique(daily_food[dup_idx, 1])
        added_units = cbind(dup_names, sapply(dup_names, sum_duplicates, daily_food))
        rownames(daily_food) = daily_food[, 1]
        daily_food = daily_food[setdiff(daily_food[, 1], dup_names), ]
        daily_food = rbind(added_units, daily_food)
        rownames(daily_food) = NULL
    }
    colnames(daily_food) = c("food", "units")

    ## Get average daily food (if length list > 1)
    daily_food[, 2] = as.numeric(daily_food[, 2]) / length(my_daily_food)

    ## Get persons group
    group = getAgeGroup(age = age, gender = gender, pregnant = pregnant,
                        lactation = lactation)

    ## Get all food eaten and units
    food_eaten = daily_food[, "food"]
    units_eaten = as.numeric(daily_food[, "units"])

    ## Check that all items in food_eaten are included in food_composition
    if(length(setdiff(food_eaten, rownames(food_composition)) > 0)) {
        stop ("One or more of the foods from my_daily_food are not valid")
    }

    ## Get daily intake level of nutrients
    my_food = food_composition[food_eaten, ]*units_eaten
    total_intake = if(is.matrix(my_food)) round(colSums(my_food), 3) else round(my_food, 3) 
#    total_intake = round(colSums(my_food), 3)
    my_food = rbind(my_food, "Total intake" = total_intake)
    food_contribution = t(t(my_food) / total_intake) * 100
    my_requirements = nutrient_requirements[group, ]

    diff_intake = data.frame(nutrient = names(my_requirements),
                             proportion = as.numeric((total_intake[common_nutrients]/my_requirements) *100),
                             group = nutrient_group[(names(my_requirements)), "group"])
    colnames(diff_intake)[2] = "proportion(%RDA)"

    diff_intake = diff_intake[order(diff_intake[, "group"], diff_intake[, "proportion(%RDA)"]), ]

    res = list(total_intake, food_contribution, diff_intake)

    names(res) = c("nutrient_intake", "food_contribution", "diff_intake")

    if (length(my_daily_food) > 1) {
        message(paste("The results correspond to an average of", length(my_daily_food),
                      "days", sep = " "))
        #message()
    }

    ## Report relevant nutrient values
    if (summary_report == FALSE) {
        return(res)
    }

    ## Total energy
    total_energy = total_intake["Energy (kcal)"]
    message(paste("Total energy intake (kcal):", round(sum(total_energy), 0)))
    #message()

    ## Deficiencies
    def_idx = which(diff_intake[, "proportion(%RDA)"] < 100)

    if (length(def_idx) > 0) {
        message("The intake level of the following nutrients is below the RDA:")
        to_print = diff_intake[def_idx, ]
        rownames(to_print) = NULL
        print(to_print)
    } else {
        message()
        message ("The intake level of all nutrients is above the RDA")
    }

    ## Intake levels above upper limits
    my_upperlevels = nutrient_upperlevels[group, intersect(rownames(diff_intake),
                                                           colnames(nutrient_upperlevels))]
    TUIL_diff = diff_intake[names(my_upperlevels), "proportion(%RDA)"] - my_upperlevels
    exc_idx = which(TUIL_diff > 0)

    if (length(exc_idx) > 0) {
        #message()
        message("The intake level of the following nutrients is above the TUIL:")
        to_print = diff_intake[names(TUIL_diff[exc_idx]), ]
        rownames(to_print) = NULL
        print(to_print)
    } else {
        message()
        message ("The intake level of all nutrients is below the TUIL")
    }

    return(res)
}

