combine_plots<- function(alloc, commit){
    EU_alloc_c <- function(alloc) {
        library(dplyr)
        library(tidyr)
        library(ggplot2)
        alloc_EU <- alloc %>%
            filter(EU.member == 1) %>%
            select(-EU.member) %>%
            gather(key = "Variable", value = "Value", -Country) %>%
            filter(Variable == "Humanitarian.allocations...billion.")

    } # Define the second function to get the commitments data
    Aid_commit_c <- function(commit) {
        commit_EU <- commit %>%
            filter(EU.member == 1) %>%
            select(-EU.member) %>%
            gather(key = "Variable", value, -Country) %>%
            filter(Variable == "Humanitarian.commitments...billion.")

    }
    alloc_EU <- EU_alloc_c(alloc)
    commit_EU <- Aid_commit_c(commit)

    # Rename the value column to distinguish between the two datasets
    alloc_EU <- alloc_EU %>% rename(Humanitarian_Allocation = Value)
    commit_EU <- commit_EU %>% rename(Humanitarian_Commitment = value)

    # Combine the datasets
    combined_data <- full_join(alloc_EU, commit_EU, by = c("Country", "Variable"))

    # Reshape to long format
    combined_data_long <- combined_data %>%
        gather(key = "Type", value = "Value", Humanitarian_Allocation, Humanitarian_Commitment)

    # Plot the combined data
    plot_combined_data <- combined_data_long %>% ggplot() +
        geom_bar(aes(x = Country, y = Value, fill = Type), stat = "identity", position = "dodge") +
        theme_minimal() +
        labs(title = "Humanitarian Allocations and Commitments by EU Country",
             x = "Country",
             y = "Amount (billion)",
             fill = "Type") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

}

