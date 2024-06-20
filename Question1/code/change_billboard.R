process_baby_names_and_charts_new <- function(Baby_Names, charts) {

    library(tidyverse)
    library(hrbrthemes)
    library(stringdist)
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

    # Subset and process charts
    subset_charts_change <- function(charts) {
        charts_change <- charts %>%
            filter(date >= "2000-01-01" & date <= "2014-12-31", `peak-rank` == 1) %>%
            mutate(Year = year(date)) %>%
            select(artist, Year) %>%  # Adjust column selection based on your dataset
            rename(Name = artist)  # Adjust column renaming based on your dataset structure

        return(charts_change)
    }

    # Match and aggregate data function
    match_and_aggregate <- function(new_names, charts_change, max_dist = 5) {
        # Step 1: Find approximate matches between new_names$Name and charts_change$Name
        matching_names <- stringdist::amatch(new_names$Name, charts_change$Name, maxDist = max_dist)

        # Step 2: Filter new_names based on matching indices
        names_to_plot <- new_names$Name[!is.na(matching_names)]

        # Step 3: Aggregate counts across years for names in names_to_plot
        agg_data <- new_names %>%
            filter(Name %in% names_to_plot) %>%
            group_by(Name, Year) %>%
            summarise(TotalCount = sum(Count), .groups = 'drop')

        return(agg_data)
    }

    # Call subset functions
    new_names <- subset_baby_names(Baby_Names)
    charts_change <- subset_charts_change(charts)

    # Match and aggregate data
    plotdata <- match_and_aggregate(new_names, charts_change, max_dist = 5)

    # Create the bubble plot using ggplot2
    charts_plot <- plotdata %>%
        ggplot() +
        geom_point(aes(x = Name, y = Year, size = TotalCount), shape = 21, fill = "blue", alpha = 0.6) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Name Inspiration from Artists",
             x = "Names",
             y = "Years") +
        guides(size = FALSE)

    charts_plot
}
