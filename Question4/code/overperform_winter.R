over_perform_winter <- function(GDP,winter){

    library(dplyr)
    library(ggplot2)
    library(hrbrthemes)

    GDP_sub <- GDP %>%
        arrange(desc(`GDP per Capita`))

    GDP_sub <- GDP[,-c(1,3)]
    colnames(GDP_sub) <- c("Country", "GDP per Capita")


    winter_sub <- winter %>%
        left_join(GDP_sub, by = "Country") %>%
        filter(!is.na(`GDP per Capita`))

    # Define team sizes for team sports
    team_sizes <- list(
        "Hockey" = 11,
        "Football" = 11,
        #main well-known sports team sizes
        "Basketball" = 5,
        "Volleyball" = 6)

    # Adjust medal counts for team sizes
    winter_sub <- winter_sub %>%
        rowwise() %>%
        mutate(AdjustedMedalCount = ifelse(Sport %in% names(team_sizes), 1 / team_sizes[[Sport]], 1))

    # Calculate total medals per country
    medal_count <- winter_sub %>%
        group_by(Country, `GDP per Capita`) %>%
        summarise(TotalMedals = sum(AdjustedMedalCount), .groups = 'drop')

    # Calculate log of GDP per Capita
    medal_count <- medal_count %>%
        mutate(LogGDP = log(`GDP per Capita`))

    quartiles <- quantile(medal_count$LogGDP, probs = c(0.25, 0.5, 0.75))


    # Plot using ggplot2
    gdp_medal_winter_plot <- ggplot(medal_count, aes(x = LogGDP, y = TotalMedals)) +
        geom_point(alpha=0.8, size =2) + geom_text(aes(label = Country), vjust = -1, hjust = 0.5, size=2.9)+
        scale_color_viridis_d()+
        labs(title = "Medal Count vs GDP per Capita (Winter Olympics)",
             x = "Log GDP per Capita",
             y = "Total Medal Count")+
        ylim(5, 750)+ xlim(6,12)+
        geom_vline(xintercept = quartiles[1], linetype = "dashed", color = "blue",) +
        geom_vline(xintercept = quartiles[2], linetype = "dashed", color = "blue") +
        geom_vline(xintercept = quartiles[3], linetype = "dashed", color = "blue")+
        annotate("text", x = quartiles[1], y = 750, label = "Q1", vjust = -1, color = "black") +
        annotate("text", x = quartiles[2], y = 750, label = "Q2", vjust = -1, color = "black") +
        annotate("text", x = quartiles[3], y = 750, label = "Q3", vjust = -1, color = "black")+
        theme_ipsum()


    gdp_medal_winter_plot

}
