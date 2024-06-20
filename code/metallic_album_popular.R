popular_album_metallica <- function(metallica){

    library(dplyr)
    library(tidyr)
    library(ggplot2)

    # Filter out unwanted albums
    filtered_metallica <- metallica %>%
        filter(!grepl(("Deluxe Remaster|Live|Remastered Deluxe Box Set|Remastered"),
                      album, ignore.case=T))

    # Average popularity per album
    popular_metallic <- metallica %>%
        group_by(album) %>%
        summarise(popular_metallic = mean(popularity, na.rm = T))

    # create plot

    popular_metallic_plot <- popular_metallic %>%
        ggplot() +
        geom_bar(aes(x = reorder(album, popular_metallic), y = popular_metallic), stat = "identity",
                 fill = "steelblue")+
        labs(title = "Average Popularity of Each Metallica Album",
             x = "Album",
             y = "Average Popularity")+
        coord_flip()+
    theme_classic()

    popular_metallic_plot

}
