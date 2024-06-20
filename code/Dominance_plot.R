dominance_plot <- function(summer, winter) {

    library(dplyr)
    library(ggplot2)
    library(tidyr)
    library(gridExtra)
    library(grid)

    #  calculate medal weightings
    medal_weight <- function(medal_ratio) {
        medal_ratio %>%
            mutate(Medal_Score = case_when(
                Medal == "Gold" ~ 4,
                Medal == "Silver" ~ 2,
                Medal == "Bronze" ~ 1,
                TRUE ~ 0
            )) %>%
            group_by(Year, Country) %>%
            summarise(Total_Score = sum(Medal_Score, na.rm = TRUE)) %>%
            ungroup()
    }

    # Calculate medal tally for summer and winter olympics
    summer_scores <- medal_weight(summer)
    winter_scores <- medal_weight(winter)

    # top 5 countries in Summer Olympics
    top_summer_countries <- summer_scores %>%
        group_by(Country) %>%
        summarise(Total_Score = sum(Total_Score, na.rm = TRUE)) %>%
        top_n(5, Total_Score) %>%
        pull(Country)

    # top 5 countries in Winter Olympics
    top_winter_countries <- winter_scores %>%
        group_by(Country) %>%
        summarise(Total_Score = sum(Total_Score, na.rm = TRUE)) %>%
        top_n(5, Total_Score) %>%
        pull(Country)

    # Filter scores for these top countries
    summer_top_scores <- summer_scores %>%
        filter(Country %in% top_summer_countries)

    winter_top_scores <- winter_scores %>%
        filter(Country %in% top_winter_countries)

    # Combine summer and winter tallys
    combined_scores <- bind_rows(summer_top_scores, winter_top_scores)


    # plot
    plot_summer_dominance <- ggplot(summer_top_scores, aes(x = Year, y = Country, fill = Total_Score)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "orange2") +
        labs(title = "Summer Olympics Dominance",
             x = "Year",
             y = "Country",
             fill = "Total Score (Medals)") +
        theme_ipsum()


    plot_winter_dominance <- ggplot(winter_top_scores, aes(x = Year, y = Country, fill = Total_Score)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "deepskyblue2") +
        labs(title = "Winter Olympic Dominance",
             x = "Year",
             y = "Country",
             fill = "Total Score (Medals)") +
        theme_ipsum()

  combined_dominance_plot <- grid.arrange(plot_summer_dominance, plot_winter_dominance, ncol = 1)
  combined_dominance_plot

}

