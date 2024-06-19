regress_coldplay <- function(coldplay){
    library(regress)
    library(patchwork)

    coldplay_reg = coldplay %>%
        group_by(album_name)

    model_coldplay <- # So we estimate a model for each fund
        lm(popularity ~ duration + danceability + valence +tempo +energy, data = coldplay_reg )


    # Create the plots using ggplot2
    plot_duration_coldplay <- ggplot(coldplay_reg, aes(x = duration, y = popularity)) +
        geom_point(size = 0.5, alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE, color = "blue") +
        geom_rug(col="steelblue",alpha=0.1, linewidth=1)+
        labs(
            title = "Regression Analysis of Coldplay",
            x = "Duration (ms)",
            y = "Popularity"
        ) +
        theme_minimal()

    plot_danceability_coldplay <- ggplot(coldplay_reg, aes(x = danceability, y = popularity)) +
        geom_point(size = 0.5, alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE, color = "blue") +
        geom_rug(col="steelblue",alpha=0.1, linewidth=1)+
        labs(

            x = "Danceability",
            y = "Popularity"
        ) +
        theme_minimal()

    plot_valence_coldplay <- ggplot(coldplay_reg, aes(x = valence, y = popularity)) +
        geom_point(size = 0.5, alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE, color = "blue") +
        geom_rug(col="steelblue",alpha=0.1, linewidth=1)+
        labs(

            x = "Valence",
            y = "Popularity"
        ) +
        theme_minimal()

    plot_tempo_coldplay <- ggplot(coldplay_reg, aes(x = tempo, y = popularity)) +
        geom_point(size = 0.5, alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE, color = "blue") +
        geom_rug(col="steelblue",alpha=0.1, linewidth=1)+
        labs(

            x = "Tempo",
            y = "Popularity"
        ) +
        theme_minimal()

    plot_energy_coldplay <- ggplot(coldplay_reg, aes(x = energy, y = popularity)) +
        geom_point(size = 0.5, alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE, color = "blue") +
        geom_rug(col="steelblue",alpha=0.1, linewidth=1)+
        labs(
            x = "Energy",
            y = "Popularity"
        ) +
        theme_minimal()

    # Combine the plots into one window frame
    combined_reg_plot_coldplay <- (plot_duration_coldplay | plot_danceability_coldplay) / (plot_valence_coldplay | plot_tempo_coldplay | plot_energy_coldplay)

    combined_reg_plot_coldplay


}

