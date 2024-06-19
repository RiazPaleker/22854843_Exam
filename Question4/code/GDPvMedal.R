GDP_per_capita_medal <- function(GDP,summer) {

    library(dplyr)
    library(ggplot2)
    library(hrbrthemes)
    GDP_sort <- GDP %>%
        arrange(desc(`GDP per Capita`))

    GDP <- GDP_sort

    GDP_sub <- GDP[,-c(1,3)]
    colnames(GDP_sub) <- c("Country", "GDP per Capita")


    country_codes <- c("IND", "PAK", "UKR","RSA","BRA",
                       "TUR","MEX","ARG","COL","GHA",
                       "KEN","CGO","NGR")

    GDP_sub <- GDP_sub %>%
        filter(Country %in% country_codes)

    summer_sub <- summer %>%
        filter(Country %in% country_codes) %>%
        left_join(GDP_sub, by = "Country")

     # Define team sizes for team sports
     team_sizes <- list(
         "Hockey" = 11,
         "Football" = 11,
         #main well-known sports team sizes
         "Basketball" = 5,
         "Volleyball" = 6)

     # Adjust medal counts for team sizes
     summer_sub <- summer_sub %>%
         rowwise() %>%
         mutate(AdjustedMedalCount = ifelse(Sport %in% names(team_sizes), 1 / team_sizes[[Sport]], 1))

     # Calculate total medals per country
     medal_count <- summer_sub %>%
         group_by(Country, `GDP per Capita`) %>%
         summarise(TotalMedals = sum(AdjustedMedalCount), .groups = 'drop')

     # Calculate log of GDP per Capita
     medal_count <- medal_count %>%
         mutate(LogGDP = log(`GDP per Capita`))

     # Plot using ggplot2
     point_olympic_plot <- ggplot(medal_count, aes(x = LogGDP, y = TotalMedals)) +
         geom_point() + geom_text(aes(label = Country), vjust = -1, hjust = 0.5, size=2.9)+
         scale_color_viridis_d()+
         labs(title = "Medal Count vs GDP per Capita",
              x = "Log GDP per Capita",
              y = "Total Medal Count")+
         theme_ipsum()

     point_olympic_plot
}

