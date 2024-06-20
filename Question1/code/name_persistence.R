name_persistence <- function(Baby_Names, top_n_count = 25, lag = 1:3) {

    library(tidyverse)
    library(dplyr)
    library(stats)
    library(ggplot2)

    # Extract top N names by year and gender
    top_25_names <- Baby_Names %>%
        group_by(Year, Gender) %>%
        top_n(top_n_count, Count) %>%
        ungroup()

    # Function to calculate serial correlation
    calculate_serial_correlation <- function(Baby_Names, start_year, gender, lag = 1:3) {
        correlation_results <- data.frame(Lag = integer(), Correlation = numeric())

        for (l in lag) {
            current_year_names <- Baby_Names %>%
                filter(Year == start_year, Gender == gender) %>%
                select(Name)

            next_year_names <- Baby_Names %>%
                filter(Year == (start_year + l), Gender == gender) %>%
                select(Name)

            common_names <- intersect(current_year_names$Name, next_year_names$Name)
            correlation <- length(common_names) / top_n_count

            correlation_results <- rbind(correlation_results, data.frame(Lag = l, Correlation = correlation))
        }

        return(correlation_results)
    }

    # Apply the function to all years and genders
    all_years <- unique(top_25_names$Year)
    all_genders <- unique(top_25_names$Gender)
    results <- data.frame(Year = integer(), Gender = character(), Lag = integer(), Correlation = numeric())

    for (year in all_years) {
        for (gender in all_genders) {
            correlation_results <- calculate_serial_correlation(top_25_names, year, gender)
            correlation_results$Year <- year
            correlation_results$Gender <- gender
            results <- rbind(results, correlation_results)
        }
    }

    # Function to calculate average correlation by decade
    average_correlation_by_decade <- function(results) {
        results %>%
            mutate(Decade = floor(Year / 10) * 10) %>%
            group_by(Decade, Gender, Lag) %>%
            summarise(Average_Correlation = mean(Correlation)) %>%
            ungroup()
    }

    # Calculate the average correlation by decade
    decade_correlation <- average_correlation_by_decade(results)

    # Plot the results to visualize the trends
    corr_plot <- decade_correlation %>%
        ggplot() +
        geom_line(aes(x = Decade, y = Average_Correlation, color = as.factor(Lag))) +
        facet_wrap(~ Gender) +
        labs(
            title = "Average Correlation of Popular Names by Decade",
            x = "Decade",
            y = "Average Correlation",
            color = "Subsequent Year"
        )

    corr_plot
}

