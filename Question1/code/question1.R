library(tidyverse)
Baby_Names <- readRDS("./data/US_Baby_names/Baby_Names_By_US_State.rds")
charts <- readRDS("./data/US_Baby_names/charts.rds")
HBO_Titles <- readRDS("./data/US_Baby_names/HBO_titles.rds")
HBO_Credits <- readRDS("./data/US_Baby_names/HBO_credits.rds")


library(dplyr)

# Filter the top 25 boys and girls names for each year
top_25_names_per_year <- Baby_Names %>%
    group_by(Year, Gender) %>%
    top_n(25, Count) %>%
    ungroup()

# Function to calculate serial correlation
# lag = subsequent years populairty of names
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
        correlation <- length(common_names) / 25

        correlation_results <- rbind(correlation_results, data.frame(Lag = l, Correlation = correlation))
    }

    return(correlation_results)
}

# Apply the function to all years and genders
all_years <- unique(top_25_names_per_year$Year)
all_genders <- unique(top_25_names_per_year$Gender)
results <- data.frame(Year = integer(), Gender = character(), Lag = integer(), Correlation = numeric())

for (year in all_years) {
    for (gender in all_genders) {
        correlation_results <- calculate_serial_correlation(top_25_names_per_year, year, gender)
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
library(ggplot2)

serial_correlation_plot = decade_correlation %>%
    ggplot() +
    geom_line(aes(x = Decade, y = Average_Correlation, color = as.factor(Lag))) +
    facet_wrap(~ Gender) +
    labs(title = "Average Serial Correlation of Popular Names by Decade",
         x = "Decade",
         y = "Average Serial Correlation",
         color = "Subsequent Year")

