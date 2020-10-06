globalVariables(c("nutrient", "proportion", "group", "food", "day"))

## nutrientPiePlot ##
nutrientPiePlot = function(daily_intake,
                           nutrient_name = "Vitamin B-12 (ug)", n = 10) {

    if ((nutrient_name %in% colnames(daily_intake$food_contribution)) == FALSE) {
        stop("Invalid nutrient_name")
    }

    nutrient_values = daily_intake$food_contribution[, nutrient_name]
    nutrient_values = nutrient_values[which(nutrient_values > 0 &
                                              nutrient_values < 100)]
    nutrient_values = sort(nutrient_values, decreasing = TRUE)

    if (length(nutrient_values) > n) {
        nutrient_values = nutrient_values[1:n]
        names_nutri = names(nutrient_values)
        nutrient_values = c(nutrient_values, 100 - sum(nutrient_values))
        names(nutrient_values) = c(names_nutri, "Others")
    }

    nutrient_df = data.frame(food = names(nutrient_values),
                             proportion = as.numeric(nutrient_values))
    nutrient_df$food = factor(nutrient_df$food, levels = rev(as.character(nutrient_df$food)))

    bp =  ggplot(nutrient_df, aes(x = "", y = proportion, fill = food)) +
      geom_bar(width = 1, stat = "identity") + coord_polar(theta = "y") +
      labs(x = nutrient_name, y = "Proportion (%)") +
      guides(fill = guide_legend(reverse = TRUE))

    return(bp) # Save landscape 7x7
}
################################################################################

## nutrientIntakePlot
nutrientIntakePlot = function(daily_intake, color_scale = c("salmon", "cornflowerblue",
                                                            "palegreen3")) {
    ## Color scale
    names(color_scale) = c("macronutrient", "mineral", "vitamin")
    color_scale = color_scale[intersect(unique(daily_intake$diff_intake[, "group"]), names(color_scale))]
    colnames(daily_intake$diff_intake)[[2]] = "proportion"

    plot = ggplot(data=daily_intake$diff_intake, aes(x=nutrient, y=proportion, fill = group)) +
           geom_bar(stat="identity") +
           scale_x_discrete(limits = daily_intake$diff_intake[, "nutrient"]) +
           labs(x="Nutrient", y = "Intake/Requirement (%)") +
           scale_fill_manual(values = color_scale) +
           theme_bw() +
           geom_hline(yintercept=100, linetype="dashed", color = "red", size=0.5) +
           theme(#axis.title.x=element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 axis.text = element_text(size = 10),
                 axis.title = element_text(size = 10, vjust = 0),
                 axis.text.x = element_text(angle=90, hjust = 1))

    return(plot)
}
################################################################################

## nutrientTimeTrend ##
nutrientsTimeTrend = function(my_daily_food, food_database = "USDA", nutrients = NULL,
                              age = 27, gender = "female", pregnant = FALSE,
                              lactation = FALSE) {

    ## Get nutrient requirements
    nutrient_requirements = NIH_nutrient_recommendations$RDA

    ## Check input data
    if(!is.list(my_daily_food)) {
        stop("my_daily_food must be a list")
    }
    if(length(my_daily_food) < 2) {
        stop("my_daily_food must contain information of at least 2 days")
    }
    if (!is.null(nutrients)) {
        nutrients = intersect(nutrients, colnames(nutrient_requirements))
        if(length(nutrients) == 0) {
            stop("Invalid nutrients")
        }
    }

    time_data = lapply(my_daily_food, dietBalance, food_database = food_database,
                       age = age, gender = gender,pregnant = pregnant,
                       lactation = lactation, summary_report = FALSE)

    time_dataM = do.call(rbind, mapply(bind_day, time_data, 1:length(time_data), SIMPLIFY = FALSE))

    if (!is.null(nutrients)) {
        nutrients = intersect(nutrients, time_dataM[, 1])
        if (length(nutrients) == 0) {
            stop ("Invalid nutrients")
        }
        idx = sapply(nutrients, find_idx, time_dataM[, 1])
        time_dataM = time_dataM[idx, ]
    }

    time_dataDF = data.frame(nutrient = as.character(time_dataM[, "nutrient"]),
                             proportion = as.numeric(time_dataM[, "proportion(%RDA)"]),
                             day = as.numeric(time_dataM[, "day"]))
    p = ggplot(data = time_dataDF, aes(x = day, y =proportion, colour = nutrient)) +
        geom_line() + geom_point() +
        theme_bw() +
        geom_hline(yintercept=100, linetype="dashed", color = "red", size=0.5) +
        labs(x="Day", y = "Intake/Requirement (%)") +
        theme(#axis.title.x=element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text = element_text(size = 10),
              axis.title = element_text(size = 10, vjust = 0),
              axis.text.x = element_text(angle=0, hjust = 1))

    return(p)
}
