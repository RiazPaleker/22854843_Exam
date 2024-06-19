popular_album_coldplay <- function(coldplay){

    library(dplyr)
    library(tidyr)
    library(ggplot2)

    # Average popularity per album
    popular_coldplay <- coldplay %>%
        group_by(album_name) %>%
        summarise(popular_coldplay = mean(popularity, na.rm = T))

    # create plot

    popular_coldplay_plot <- popular_coldplay %>%
        ggplot() +
        geom_bar(aes(x = reorder(album_name, popular_coldplay), y = popular_coldplay), stat = "identity",
                 fill = "steelblue")+
        labs(title = "Average Popularity of Each Coldplay Album",
             x = "Album",
             y = "Average Popularity")+
        coord_flip()+
        theme_classic()

    popular_coldplay_plot

}
