Aid_alloc <- function(alloc) {
    library(dplyr)
    library(tidyr)
    # turn data long, filter to onyl EU members and remove EU from variable coloumn
    alloc_EU <- alloc %>%
        filter(EU.member == 1) %>%
        select(-EU.member) %>%
        gather(Variable, value, -Country)
    # Plot the bar graph
    humanitarian_data <- alloc_EU %>%
        filter(Variable == "Humanitarian.allocations...billion.")

    library(ggplot2)
    aid_alloc_plot <- humanitarian_data %>%  ggplot()+
        geom_bar(aes(x = Country, y = value), stat = "identity") +
        labs(title = "Humanitarian Allocations by EU Country",
             x = "Country",
             y = "Humanitarian Allocations (billion)") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

}


