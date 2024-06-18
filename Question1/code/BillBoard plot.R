# CHARTS: MUSIC

subset_baby_names <- function(Baby_Names) {
    # Filter the data for the year 2014 and state New York
    new_names <- Baby_Names %>%
        filter(Year>=2010 & Year<=2014, State == "NY", Count>=1000 )%>%
        arrange(desc(Count))

    return(new_names[,-c(1,4,5)])
}

new_names =subset_baby_names(Baby_Names)

subset_charts  <- function(charts){

    charts_new = charts %>%
        filter(date>="2010-01-01" & date<="2014-12-31", charts$`peak-rank`==1) %>%
        mutate(year = year(date)) %>%
        select(-date) %>%
        rename(date = year)


    return(filter_charts[,-c(1,2,4,5,6)])


}
charts_new=subset_charts(charts)
colnames(charts_new) =c("Name", "Year")

# Load required package for fuzzy matching
install.packages("stringdist")
library(stringdist)

match_agg_data = function(new_names, charts_new, max_dist = 5){
    # Step 1: Find approximate matches between new_names$Name and dataset2$Artist
    matching_names <- stringdist::amatch(new_names$Name, charts_new$Name, maxDist = 5)

    # Step 2: Filter Dataset 1 based on matching indices
    names_to_plot <- new_names$Name[!is.na(matching_names)]

    # Step 3: Aggregate counts across years for names in names_to_plot
    agg_data <- new_names %>%
        filter(Name %in% names_to_plot) %>%
        group_by(Name, Year) %>%
        summarise(TotalCount = sum(Count))

    return(agg_data)
}

agg_data = match_agg_data(new_names, charts_new, max_dist = 5)
# Step 4: Create the bubble plot using ggplot2
library(ggplot2)

charts_names_plot = agg_data %>% ggplot() +
    geom_point(aes(x = Name, y = Year, size = TotalCount), shape = 21, fill="blue", alpha = 0.6, ) +
    guides(size = F)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    labs(title = "Popularity of Names in Hot 100 Charts",
         x = "Names",
         y = "Total Count")

charts_names_plot



# CHANGE BILLBOARD YEARS!!!

new_names

subset_charts_change  <- function(charts){

    charts_change = charts %>%
        filter(date>="2000-01-01" & date<="2014-12-31", charts$`peak-rank`==1) %>%
        mutate(year = year(date)) %>%
        select(-date) %>%
        rename(date = year)


    return(charts_change[,-c(1,2,4,5,6)])

}
charts_change= subset_charts_change(charts)
colnames(charts_change) =c("Name", "Year")

match_and_aggregate <- function(new_names, charts_change, max_dist = 5) {
    # Step 1: Find approximate matches
    matching_names_new <- stringdist::amatch(new_names$Name, charts_change$Name, maxDist = max_dist)

    # Step 2: Filter Dataset 1 based on matching indices
    names_to_plot_new <- new_names$Name[!is.na(matching_names_new)]

    # Step 3: Aggregate counts across years for names in names_to_plot
    plotdata <- new_names %>%
        filter(Name %in% names_to_plot_new) %>%
        group_by(Name, Year) %>%
        summarise(TotalCount = sum(Count), .groups = 'drop')

    return(plotdata)
}

plotdata <- match_and_aggregate(new_names, charts_change, max_dist = 5)

# Step 4: Create the bubble plot using ggplot2
library(ggplot2)

charts_names_plot_new = plotdata %>% ggplot() +
    geom_point(aes(x = Name, y = Year, size = TotalCount), shape = 21, fill="blue", alpha = 0.6, ) +
    guides(size = F)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    labs(title = "Popularity of Names in Hot 100 Charts",
         x = "Names",
         y = "Years")

charts_names_plot_new


