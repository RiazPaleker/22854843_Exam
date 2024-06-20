    GDP_per_capita <- function(GDP,summer) {

        library(dplyr)
        library(hrbrthemes)
        GDP_sort <- GDP %>%
            arrange(desc(`GDP per Capita`))

        GDP <- GDP_sort

        GDP_sub <- GDP[c(125:142),2]
        #select any countries with similar economy size to India
        # & any emerging economies
        # & any South American Countries



        country_codes <- c("IND", "PAK", "UKR","RSA","BRA",
                           "TUR","MEX","ARG","COL","GHA",
                           "KEN","CGO","NGR")

        summer_sub <- summer %>%
            filter(Country %in% country_codes)

        # Process data to count medals per country, ensuring team sports count as one medal
        medal_counts <- summer_sub %>%
            distinct(Year, Country, Sport, Medal) %>%
            group_by(Country, Medal) %>%
            summarise(count = n(), .groups = 'drop')

        # Pivot data to wide format
        medal_counts_wide <- medal_counts %>%
            pivot_wider(names_from = Medal, values_from = count, values_fill = 0)

        # Ensure the dataframe has columns for all medal types
        medal_counts_wide <- medal_counts_wide %>%
            mutate(Gold = ifelse(is.na(Gold), 0, Gold),
                   Silver = ifelse(is.na(Silver), 0, Silver),
                   Bronze = ifelse(is.na(Bronze), 0, Bronze))

        # Gather data back to long format for ggplot
        medal_counts_long <- medal_counts_wide %>%
            pivot_longer(cols = c(Gold, Silver, Bronze), names_to = "Medal", values_to = "Count")

        # Create combined bar plot
        GDP_plot <- ggplot(medal_counts_long, aes(x = Country, y = Count, fill = Medal)) +
            geom_bar(stat = "identity", position = position_dodge()) +
            labs(title = "Countries Medal Count",
                 x = "Country",
                 y = "Count of Medals") +
            scale_fill_manual(values = c("Gold" = "gold", "Silver" = "grey", "Bronze" = "brown")) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme_ipsum()

        GDP_plot
    }



