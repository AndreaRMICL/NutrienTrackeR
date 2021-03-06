## getAgeGroup ##
getAgeGroup = function(age, gender, pregnant = FALSE, lactation = FALSE) {
# nutrient requirements from environment
    nutrient_requirements = NIH_nutrient_recommendations$RDA

    possible_groups = rownames(nutrient_requirements)
    possible_groups = gsub(">70", "71_100", possible_groups)
    possible_groups = gsub(":", "_", possible_groups)
    gender_age_m = do.call(rbind, strsplit(possible_groups, "_"))
    age_m = apply(gender_age_m[, 2:3], 2, as.numeric)

    if (age < 9) {
        group = rownames(nutrient_requirements)[which(age_m[, 2] >= age &
                    age_m[, 1] <= age)]
        return(group)
    }
    if (gender == "male") {
        group = rownames(nutrient_requirements)[which(age_m[, 2] >= age &
                    age_m[, 1] <= age & gender_age_m[, 1] == "Male")]
        return(group)
    }
    if (gender == "female") {
        group = rownames(nutrient_requirements)[which(age_m[, 2] >= age & age_m[, 1] <= age &
                    gender_age_m[, 1] == "Female")]
        return(group)
    }
    if (pregnant == TRUE) {
        group = rownames(nutrient_requirements)[which(age_m[, 2] >= age & age_m[, 1] <= age &
                    gender_age_m[, 1] == "Pregnancy")]
        return(group)
    }
    if (lactation == TRUE) {
        group = rownames(nutrient_requirements)[which(age_m[, 2] >= age & age_m[, 1] <= age &
                    gender_age_m[, 1] == "Lactation")]
        return(group)
    }
}

## sum_duplicates ##
sum_duplicates = function(dup_name, daily_food) {
    dup_units = as.numeric(daily_food[which(daily_food[, 1] == dup_name), 2])
    return(sum(dup_units))
}

## find_idx ##
find_idx = function(name, all_names) {
    return(which(all_names == name))
}

## bind_day ##
bind_day = function(data, day) {
  return(cbind(data$diff_intake, day = day))
}

## parse_input_day_diet ##
parse_input_day_diet = function(data) {
  foods_split <- unlist(strsplit(data, split="\n"))
  foods_amounts_split <- strsplit(unlist(strsplit(data, "\n")), split=";")
  food_names <- trimws(sapply(foods_amounts_split, `[[`, 1, simplify=TRUE))
  food_amounts <- as.character(as.numeric(trimws(sapply(foods_amounts_split, `[[`, 2, simplify=TRUE)))/100)
  daily_diet_matrix <- matrix(c(food_names, food_amounts), ncol=2)
  colnames(daily_diet_matrix) <- c("Food", "Units (100 g)")
  return(daily_diet_matrix)
}
