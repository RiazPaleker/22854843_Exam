regress_metallica <- function(metallica){
    library(regress)
    library(patchwork)

    metallica_reg = metallica %>%
        group_by(album)

    model <- # So we estimate a model for each fund
    lm(popularity ~ duration_ms + danceability + valence +tempo +energy, data = metallica_reg )


# Create the plots using ggplot2
plot_duration <- ggplot(metallica_reg, aes(x = duration_ms, y = popularity)) +
    geom_point(size = 0.5, alpha = 0.2) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    geom_rug(col="steelblue",alpha=0.1, linewidth=1)+
    labs(
        title = "Regression Analysis of Metallica",
        x = "Duration (ms)",
        y = "Popularity"
    ) +
    theme_minimal()

plot_danceability <- ggplot(metallica_reg, aes(x = danceability, y = popularity)) +
    geom_point(size = 0.5, alpha = 0.2) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    geom_rug(col="steelblue",alpha=0.1, linewidth=1)+
    labs(

        x = "Danceability",
        y = "Popularity"
    ) +
    theme_minimal()

plot_valence <- ggplot(metallica_reg, aes(x = valence, y = popularity)) +
    geom_point(size = 0.5, alpha = 0.2) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    geom_rug(col="steelblue",alpha=0.1, linewidth=1)+
    labs(

        x = "Valence",
        y = "Popularity"
    ) +
    theme_minimal()

plot_tempo <- ggplot(metallica_reg, aes(x = tempo, y = popularity)) +
    geom_point(size = 0.5, alpha = 0.2) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    geom_rug(col="steelblue",alpha=0.1, linewidth=1)+
    labs(

        x = "Tempo",
        y = "Popularity"
    ) +
    theme_minimal()

plot_energy <- ggplot(metallica_reg, aes(x = energy, y = popularity)) +
    geom_point(size = 0.5, alpha = 0.2) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    geom_rug(col="steelblue",alpha=0.1, linewidth=1)+
    labs(
        x = "Energy",
        y = "Popularity"
    ) +
    theme_minimal()

# Combine the plots into one window frame
combined_reg_plot <- (plot_duration | plot_danceability) / (plot_valence | plot_tempo | plot_energy)

combined_reg_plot


}

