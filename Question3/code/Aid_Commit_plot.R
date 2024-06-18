Aid_commit <- function(commit) {
    library(dplyr)
    library(tidyr)
    # turn data long, filter to onyl EU members and remove EU from variable coloumn
    commit_EU <- commit %>%
        filter(EU.member == 1) %>%
        select(-EU.member) %>%
        gather(Variable, value, -Country)
    # Plot the bar graph
    humanitarian_commit_data <- commit_EU %>%
        filter(Variable == "Humanitarian.commitments...billion.")

    library(ggplot2)
    aid_commit_plot <- humanitarian_commit_data %>%  ggplot()+
        geom_bar(aes(x = Country, y = value), stat = "identity") +
        theme_minimal() +
        labs(title = "Humanitarian Commitment by EU Country",
             x = "Country",
             y = "Humanitarian Allocations (billion)") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

}
