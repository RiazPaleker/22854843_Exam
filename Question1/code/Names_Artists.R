
process_baby_names_and_charts <- function(Baby_Names, charts) {

    library(hrbrthemes)
    library(tidyverse)
    library(dplyr)
    library(stats)
    library(ggplot2)

    # Subset function for Baby_Names
    subset_baby_names <- function(Baby_Names) {
        new_names <- Baby_Names %>%
            filter(Year >= 2010 & Year <= 2014, State == "NY", Count >= 1000) %>%
            arrange(desc(Count)) %>%
            select(-c(1, 4, 5))

        return(new_names)
    }

    # Subset function for charts
    subset_charts <- function(charts) {
        charts_new <- charts %>%
            filter(date >= "2010-01-01" & date <= "2014-12-31", `peak-rank` == 1) %>%
            mutate(Year = year(date)) %>%
            select(artist, Year) %>%  # Adjust column selection based on your dataset
            rename(Name = artist) # Adjust column names as per your dataset structure

        return(charts_new)
    }

    # Match and aggregate data function
    match_agg_data <- function(new_names, charts_new, max_dist = 5) {
        # Step 1: Find approximate matches between new_names$Name and charts_new$Name
        matching_names <- stringdist::amatch(new_names$Name, charts_new$Name, maxDist = max_dist)

        # Step 2: Filter new_names based on matching indices
        names_to_plot <- new_names$Name[!is.na(matching_names)]

        # Step 3: Aggregate counts across years for names in names_to_plot
        agg_data <- new_names %>%
            filter(Name %in% names_to_plot) %>%
            group_by(Name, Year) %>%
            summarise(TotalCount = sum(Count)) %>%
            ungroup()

        return(agg_data)
    }

    # Call subset functions
    new_names <- subset_baby_names(Baby_Names)
    charts_new <- subset_charts(charts)

    # Match and aggregate data
    agg_data <- match_agg_data(new_names, charts_new, max_dist = 5)

    # Create the bubble plot using ggplot2
    charts_names_plot <- agg_data %>%
        ggplot() +
        geom_point(aes(x = Name, y = Year, size = TotalCount), shape = 21, fill = "blue", alpha = 0.6) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Name Inspiration from Artists",
             x = "Names",
             y = "Total Count")+
        guides(size = FALSE)

    charts_names_plot
}

