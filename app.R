library(shiny)
library(plotly)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

feature_cols <- c(
  "danceability", "energy", "valence", "acousticness", "instrumentalness",
  "liveness", "speechiness", "loudness", "tempo", "duration_ms"
)

pretty_feature <- function(x) {
  recode(
    x,
    danceability = "Danceability",
    energy = "Energy",
    valence = "Valence",
    acousticness = "Acousticness",
    instrumentalness = "Instrumentalness",
    liveness = "Liveness",
    speechiness = "Speechiness",
    loudness = "Loudness",
    tempo = "Tempo",
    duration_ms = "Duration"
  )
}

load_spotify <- function() {
  high <- read_csv("data/high_popularity_spotify_data.csv", show_col_types = FALSE) |>
    mutate(popularity_group = "High popularity")

  low <- read_csv("data/low_popularity_spotify_data.csv", show_col_types = FALSE) |>
    mutate(popularity_group = "Lower popularity")

  bind_rows(high, low) |>
    mutate(
      release_year = as.integer(substr(track_album_release_date, 1, 4)),
      duration_min = duration_ms / 60000,
      popularity_group = factor(popularity_group, levels = c("High popularity", "Lower popularity")),
      playlist_genre = if_else(is.na(playlist_genre) | playlist_genre == "", "unknown", playlist_genre),
      playlist_subgenre = if_else(is.na(playlist_subgenre) | playlist_subgenre == "", "unknown", playlist_subgenre),
      tooltip = paste0(
        "<b>", track_name, "</b><br>",
        track_artist, "<br>",
        "Popularity: ", track_popularity, "<br>",
        "Genre: ", playlist_genre, " / ", playlist_subgenre, "<br>",
        "Released: ", release_year, "<br>",
        "Energy: ", round(energy, 2),
        " | Danceability: ", round(danceability, 2),
        " | Valence: ", round(valence, 2)
      )
    ) |>
    distinct(track_id, .keep_all = TRUE)
}

spotify <- load_spotify()
year_limits <- range(spotify$release_year, na.rm = TRUE)
genre_choices <- sort(unique(spotify$playlist_genre))

make_embedding <- function(df, method, selected_features, max_points, cluster_count) {
  model_df <- df |>
    filter(if_all(all_of(selected_features), ~ !is.na(.x))) |>
    arrange(desc(track_popularity))

  if (nrow(model_df) > max_points) {
    set.seed(42)
    high_priority <- model_df |> slice_head(n = min(200, nrow(model_df)))
    remainder <- model_df |> anti_join(high_priority |> select(track_id), by = "track_id")
    model_df <- bind_rows(
      high_priority,
      remainder |> slice_sample(n = max_points - nrow(high_priority))
    )
  }

  x <- model_df |> select(all_of(selected_features)) |> as.data.frame()
  feature_sds <- vapply(x, sd, numeric(1), na.rm = TRUE)
  selected_features <- names(feature_sds[!is.na(feature_sds) & feature_sds > 0])
  if (length(selected_features) < 2) {
    stop("At least two selected features need variation after filtering.")
  }
  x <- model_df |> select(all_of(selected_features)) |> as.data.frame()
  x_scaled <- scale(x)

  if (method == "PCA") {
    fit <- prcomp(x_scaled, center = FALSE, scale. = FALSE)
    coords <- as.data.frame(fit$x[, 1:2])
    names(coords) <- c("Dim1", "Dim2")
    variance <- (fit$sdev^2) / sum(fit$sdev^2)
    variance_pct <- round(variance[1:2] * 100, 1)
    loadings <- as.data.frame(fit$rotation[, 1:2])
    loadings$feature <- rownames(loadings)
    note <- paste0(
      "PCA is a linear projection. PC1 explains ",
      variance_pct[1], "% and PC2 explains ",
      variance_pct[2], "% of the selected-feature variance."
    )
  } else {
    d <- dist(x_scaled)
    fit <- cmdscale(d, k = 2, eig = TRUE)
    coords <- as.data.frame(fit$points)
    names(coords) <- c("Dim1", "Dim2")
    loadings <- data.frame(feature = selected_features, PC1 = NA_real_, PC2 = NA_real_)
    eig <- fit$eig[fit$eig > 0]
    explained <- sum(eig[1:min(2, length(eig))]) / sum(eig)
    variance_pct <- c(NA_real_, NA_real_)
    note <- paste0(
      "MDS maps pairwise song similarity into two dimensions. The first two dimensions capture about ",
      round(explained * 100, 1), "% of the positive distance structure."
    )
  }

  centers <- min(cluster_count, max(2, floor(nrow(model_df) / 10)))

  bind_cols(model_df, coords) |>
    mutate(cluster = factor(kmeans(x_scaled, centers = centers, nstart = 20)$cluster)) |>
    list(loadings = loadings, note = note, method = method, rows = nrow(model_df), variance_pct = variance_pct)
}

separation_summary <- function(df) {
  if (n_distinct(df$popularity_group) < 2 || nrow(df) < 10) {
    return("Not enough data in both popularity groups to compare separation.")
  }

  centers <- df |>
    group_by(popularity_group) |>
    summarize(cx = mean(Dim1), cy = mean(Dim2), .groups = "drop")

  center_distance <- sqrt(diff(centers$cx)^2 + diff(centers$cy)^2)
  spread <- df |>
    left_join(centers, by = "popularity_group") |>
    mutate(distance_to_group_center = sqrt((Dim1 - cx)^2 + (Dim2 - cy)^2)) |>
    summarize(avg_spread = mean(distance_to_group_center), .groups = "drop") |>
    pull(avg_spread)

  ratio <- center_distance / spread
  paste0(
    "Group separation score: ", round(ratio, 2),
    ". Higher values mean the two popularity groups form more distinct regions; lower values mean stronger overlap."
  )
}

ui <- fluidPage(
  tags$head(
    tags$title("Spotify Audio Feature Space"),
    tags$style(HTML("
      body { background: #f7f8fa; color: #20242a; }
      .app-title { margin: 22px 0 4px; font-weight: 800; letter-spacing: 0; font-size: 26px; }
      .subtitle { color: #5d6673; margin-bottom: 20px; font-size: 16px; }
      .panel, .stat, .interpretation {
        background: white; border: 1px solid #dfe3e8; border-radius: 8px;
        padding: 16px; box-shadow: 0 1px 2px rgba(0,0,0,0.04);
      }
      .stat { min-height: 92px; }
      .stat .label { color: #667085; font-size: 12px; text-transform: uppercase; font-weight: 700; }
      .stat .value { font-size: 24px; font-weight: 800; margin-top: 8px; }
      .control-label { font-weight: 700; color: #2c3440; }
      .selectize-input, .form-control { border-radius: 6px; }
      .plot-title { font-size: 18px; font-weight: 800; margin: 4px 0 12px; }
      .small-note { color: #667085; font-size: 13px; line-height: 1.4; }
      .shiny-output-error-validation { color: #9a3412; font-weight: 700; }
    "))
  ),
  div(class = "app-title", "Spotify Audio Feature Space"),
  div(class = "subtitle", "Explore how popularity, genre, and release period change the shape of a learned two-dimensional music map."),
  sidebarLayout(
    sidebarPanel(
      class = "panel",
      selectInput("method", "Dimensionality reduction", c("PCA", "MDS similarity map")),
      sliderInput("years", "Release year range", min = year_limits[1], max = year_limits[2],
                  value = year_limits, sep = ""),
      selectizeInput("genres", "Playlist genres", choices = genre_choices,
                     selected = c("pop", "rock", "hip-hop", "electronic", "ambient", "lofi", "latin"),
                     multiple = TRUE),
      checkboxGroupInput("groups", "Popularity groups",
                         choices = levels(spotify$popularity_group),
                         selected = levels(spotify$popularity_group)),
      selectInput("color_by", "Color points by", c("Popularity group", "Playlist genre", "Cluster")),
      checkboxGroupInput("features", "Features used to build the map",
                         choices = setNames(feature_cols, pretty_feature(feature_cols)),
                         selected = feature_cols),
      sliderInput("clusters", "Clusters for exploratory grouping", min = 2, max = 8, value = 5, step = 1),
      sliderInput("max_points", "Maximum points in map", min = 300, max = 2500, value = 1400, step = 100),
      div(class = "small-note",
          "Changing these controls recalculates the map from the filtered data, so shape and separation are part of the analysis.")
    ),
    mainPanel(
      fluidRow(
        column(4, div(class = "stat", div(class = "label", "Songs mapped"), div(class = "value", textOutput("n_songs", container = span)))),
        column(4, div(class = "stat", div(class = "label", "Average popularity"), div(class = "value", textOutput("avg_popularity", container = span)))),
        column(4, div(class = "stat", div(class = "label", "Genres shown"), div(class = "value", textOutput("n_genres", container = span))))
      ),
      br(),
      div(class = "panel",
          div(class = "plot-title", "PCA Map of Spotify Audio Features by Popularity Group"),
          plotlyOutput("map", height = "620px"),
          br(),
          div(class = "interpretation", textOutput("model_note"), br(), textOutput("separation_note"))
      ),
      br(),
      fluidRow(
        column(6, div(class = "panel", div(class = "plot-title", "Average Audio Profile"), plotlyOutput("profile", height = "390px"))),
        column(6, div(class = "panel", div(class = "plot-title", "Most Influential PCA Directions"), plotlyOutput("loadings", height = "390px")))
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    req(input$genres, input$groups, input$features)
    validate(need(length(input$features) >= 2, "Choose at least two audio features."))

    spotify |>
      filter(
        release_year >= input$years[1],
        release_year <= input$years[2],
        playlist_genre %in% input$genres,
        popularity_group %in% input$groups
      )
  })

  embedding <- reactive({
    df <- filtered_data()
    validate(need(nrow(df) >= 20, "The current filters leave too few songs. Broaden the year, genre, or popularity selections."))
    method <- if (input$method == "PCA") "PCA" else "MDS"
    make_embedding(df, method, input$features, input$max_points, input$clusters)
  })

  mapped <- reactive(embedding()[[1]])

  output$n_songs <- renderText(format(nrow(mapped()), big.mark = ","))
  output$avg_popularity <- renderText(round(mean(mapped()$track_popularity), 1))
  output$n_genres <- renderText(n_distinct(mapped()$playlist_genre))
  output$model_note <- renderText(embedding()$note)
  output$separation_note <- renderText(separation_summary(mapped()))

  output$map <- renderPlotly({
    df <- mapped()
    color_var <- switch(
      input$color_by,
      "Popularity group" = "popularity_group",
      "Playlist genre" = "playlist_genre",
      "Cluster" = "cluster"
    )

    var_pct <- embedding()$variance_pct
    xlab <- if (embedding()$method == "PCA") paste0("PC1 (", var_pct[1], "% variance)") else "MDS dimension 1"
    ylab <- if (embedding()$method == "PCA") paste0("PC2 (", var_pct[2], "% variance)") else "MDS dimension 2"

    p <- ggplot(df, aes(x = Dim1, y = Dim2, color = .data[[color_var]], text = tooltip)) +
      geom_point(alpha = 0.6, size = 2.1) +
      labs(x = xlab, y = ylab, color = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal"
      )

    ggplotly(p, tooltip = "text") |>
      layout(legend = list(orientation = "h", y = -0.18))
  })

  output$profile <- renderPlotly({
    profile_source <- mapped()
    available_features <- intersect(input$features, names(profile_source))
    scaled_features <- as.data.frame(scale(profile_source |> select(all_of(available_features))))
    scaled_features$popularity_group <- profile_source$popularity_group

    df <- scaled_features |>
      pivot_longer(all_of(available_features), names_to = "feature", values_to = "value") |>
      filter(!is.na(value)) |>
      group_by(popularity_group, feature) |>
      summarize(value = mean(value), .groups = "drop") |>
      mutate(feature = pretty_feature(feature))

    p <- ggplot(df, aes(x = reorder(feature, value), y = value, fill = popularity_group)) +
      geom_col(position = "dodge", width = 0.72) +
      coord_flip() +
      labs(x = NULL, y = "Standardized average within current filter", fill = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom", panel.grid.minor = element_blank())

    ggplotly(p, tooltip = c("x", "y", "fill")) |>
      layout(legend = list(orientation = "h", y = -0.2))
  })

  output$loadings <- renderPlotly({
    validate(need(embedding()$method == "PCA", "Loadings are shown for PCA because PCA axes are linear feature combinations. Switch to PCA to inspect them."))

    loadings <- embedding()$loadings |>
      mutate(
        importance = sqrt(PC1^2 + PC2^2),
        feature = pretty_feature(feature)
      ) |>
      arrange(desc(importance)) |>
      slice_head(n = 8)

    p <- ggplot(loadings, aes(x = PC1, y = PC2, label = feature)) +
      geom_hline(yintercept = 0, color = "#d0d5dd") +
      geom_vline(xintercept = 0, color = "#d0d5dd") +
      geom_point(size = 3, color = "#2563eb") +
      geom_text(nudge_y = 0.04, size = 3.5, check_overlap = TRUE) +
      labs(x = "PC1 loading", y = "PC2 loading") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())

    ggplotly(p, tooltip = c("x", "y", "label"))
  })
}

shinyApp(ui, server)
