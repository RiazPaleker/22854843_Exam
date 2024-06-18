library(tidyverse)
Baby_Names <- readRDS("./data/US_Baby_names/Baby_Names_By_US_State.rds")
charts <- readRDS("./data/US_Baby_names/charts.rds")
HBO_Titles <- readRDS("./data/US_Baby_names/HBO_titles.rds")
HBO_Credits <- readRDS("./data/US_Baby_names/HBO_credits.rds")



library(dplyr)
library(stats)

# PERSISTENCE IN POPULAR NAMES

top_25_names <- Baby_Names %>%
    group_by(Year, Gender) %>%
    top_n(25, Count) %>%
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
        correlation <- length(common_names) / 25

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
library(ggplot2)

corr_plot = decade_correlation %>% ggplot() +
    geom_line(aes(x = Decade, y = Average_Correlation, color = as.factor(Lag)))+
    facet_wrap(~ Gender) + labs(title = "Average Correlation of Popular Names by Decade",
                                              x = "Decade",
                                              y = "Average Correlation",
                                              color = "Subsequent Year")


# CHARTS

subset_baby_names <- function(Baby_Names) {
    # Filter the data for the year 2014 and state New York
    filter_names <- Baby_Names %>%
        filter(Year>=2010 & Year<=2014, State == "NY", Count>=1000 )%>%
        arrange(desc(Count))

    return(filter_names[,-c(1,4,5)])
}

new_names =subset_baby_names(Baby_Names)

subset_charts  <- function(charts){

    filter_charts = charts %>%
        filter(date>="2010-01-01" & date<="2014-12-31", charts$`peak-rank`==1) %>%
        mutate(year = year(date)) %>%
        select(-date) %>%
        rename(date = year)


    return(filter_charts[,-c(1,2,4,5,6)])


}
charts_new=subset_charts(charts)
colnames(charts_new) =c("Name", "Year")

# Load required package for fuzzy matching
install.packages("stringdist")
library(stringdist)

# Step 1: Find approximate matches between new_names$Name and dataset2$Artist
matching_names <- stringdist::amatch(new_names$Name, charts_new$Name, maxDist = 5)

# Step 2: Filter Dataset 1 based on matching indices
names_to_plot <- new_names$Name[!is.na(matching_names)]

# Step 3: Aggregate counts across years for names in names_to_plot
agg_data <- new_names %>%
    filter(Name %in% names_to_plot) %>%
    group_by(Name, Year) %>%
    summarise(TotalCount = sum(Count))


# Step 4: Create the bubble plot using ggplot2
library(ggplot2)

charts_names_plot = agg_data %>% ggplot() +
    geom_point(aes(x = Name, y = Year, size = TotalCount), shape = 21, fill="blue", alpha = 0.6, ) +
    guides(size = F)+
    labs(title = "Popularity of Names in Hot 100 Charts",
         x = "Names",
         y = "Total Count",
         size = "Total Count")

charts_names_plot
