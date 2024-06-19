album_summary <- function(metallica, coldplay){


    library(dplyr)
    library(gt)

  # Filter for the specified albums and calculate averages for Metallica
  metallica_summary <- metallica %>%
    filter(album == "72 Seasons") %>%
    summarise(
      Album = "72 Seasons",
      Popularity = mean(popularity, na.rm = T),
      Danceability = mean(danceability, na.rm = T),
      Energy = mean(energy, na.rm = T),
      Tempo = mean(tempo, na.rm = T),
      Valence = mean(valence, na.rm = T)
    )

  # Filter for the specified albums and calculate averages for Coldplay
  coldplay_summary <- coldplay %>%
    filter(album_name == "Parachutes") %>%
    summarise(
      Album = "Parachutes",
      Popularity = mean(popularity, na.rm = T),
      Danceability = mean(danceability, na.rm = T),
      Energy = mean(energy, na.rm = T),
      Tempo = mean(tempo, na.rm = T),
      Valence = mean(valence, na.rm = T)
    )

  # Combine the summaries into a single data frame
  combined <- bind_rows(metallica_summary, coldplay_summary)

  # Create a gt table
  summary_table <- combined %>%
    gt() %>%
    tab_header(
      title = md("*Average Metrics for Coldplay and Metallica Albums*"),
      subtitle = "Metallica:72 Seasons and Coldplay:Parachutes"
    ) %>%
    fmt_number(
      columns = c(Popularity, Danceability, Energy, Tempo, Valence),
      decimals = 2
    ) %>%
    cols_label(
      Album = "Album",
      Popularity = "Popularity",
      Danceability = "Cadence",
      Energy = "Tempo",
      Tempo = "Speed",
      Valence = "Valence"
      )

  summary_table
}
