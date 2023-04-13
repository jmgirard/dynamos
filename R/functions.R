# Convert seconds to MM:SS format -----------------------------------------

s_to_ts <- function(s, force_hours = FALSE, round_seconds = TRUE) {
  
  p <- lubridate::seconds_to_period(s)
  h <- lubridate::hour(p)
  m <- lubridate::minute(p)
  s <- lubridate::second(p)
  
  if (any(h[is.finite(h)] > 0) || force_hours) {
    if (round_seconds) {
      out <- sprintf("%02d:%02d:%02d", h, m, round(s))
    } else {
      out <- sprintf("%02d:%02d:%02.3f", h, m, s)
    }
  } else {
    if (round_seconds) {
      out <- sprintf("%02d:%02d", m, round(s))
    } else {
      out <- sprintf("%02d:%02.3f", m, s)
    }
  }
  
  out[!is.finite(p)] <- NA_character_
  
  out
}


# Convert HH:MM:SS.sss format to seconds ----------------------------------

ts_to_s <- function(ts) {
  if (length(ts) > 1) {
    purrr::map_dbl(ts, ts_to_s)
  } else {
    ts2 <- hms::parse_hms(ts)
    lubridate::hour(ts2) * 3600 + 
      lubridate::minute(ts2) * 60 + 
      lubridate::second(ts2)
  }
}

# Get clip-specific information tibble ------------------------------------

get_info_df <- function(abbrev) {
  df <- readxl::read_xlsx("./data/film_info.xlsx")
  if (!missing(abbrev)) {
    df <- df |> dplyr::filter(Abbrev == abbrev)
  }
  df
}


# Get mediainfo tibble ----------------------------------------------------

get_mediainfo_df <- function(abbrev) {
  df <- readxl::read_xlsx("./data/media_info.xlsx")
  if (!missing(abbrev)) {
    df <- df |> dplyr::filter(Abbrev == abbrev)
  }
  df
}

# Get clip-specific holistic ratings tibble -------------------------------

get_holistic_df <- function(abbrev) {
  df <- readr::read_rds("./data/private/holistic_tidy.rds") |> 
    tidyr::drop_na(Rating)
  if (!missing(abbrev)) {
    df <- df |> dplyr::filter(Abbrev == abbrev)
  }
  df 
}

# Get clip-specific valence ratings tibble --------------------------------

get_valence_df <- function(abbrev) {
  df <- 
    readr::read_rds("./data/private/valence_tidy.rds") |>
    dplyr::rename(Timepoint = Second) |> 
    tidyr::drop_na(Rating)
  if (!missing(abbrev)) {
    df <- df |> dplyr::filter(Abbrev == abbrev)
  }
  df
}

# Get clip-specific subtitles tibble --------------------------------------

get_subtitle_df <- function(abbrev) {
  df <- readr::read_rds("./data/subtitles_tidy.rds")
  if (!missing(abbrev)) {
    df <- df |> dplyr::filter(Abbrev == abbrev)
  }
  df
}


# Create clip-specific holistic ratings plot ------------------------------

create_holistic_plot <- function(holistic_df) {

    holistic_df |> 
    dplyr::select(-Scale) |> 
    tidyr::pivot_wider(
      names_from = Item,
      values_from = Rating
    ) |> 
    dplyr::mutate(
      "Positive Affect" = rowMeans(
        dplyr::across(c(Alert, Determined, Enthusiastic, Excited, Inspired)), 
        na.rm = TRUE
      ),
      "Negative Affect" = rowMeans(
        dplyr::across(c(Afraid, Distressed, Nervous, Scared, Upset)), 
        na.rm = TRUE
      )
    ) |> 
    tidyr::pivot_longer(
      cols = c("Positive Affect", "Negative Affect"),
      names_to = "Scale", 
      values_to = "Mean"
    ) |> 
    ggplot2::ggplot(
      ggplot2::aes(
        x = Mean, 
        y = Scale
      )
    ) + 
    ggplot2::geom_boxplot() +
    ggbeeswarm::geom_quasirandom(color = "firebrick") +
    ggplot2::scale_x_continuous(limits = c(0, 4)) +
    ggplot2::labs(
      y = NULL, 
      x = NULL
    )
}

# Estimate clip-specific valence ratings ICC ------------------------------

estimate_valence_icc_clip <- function(
    valence_df, 
    info_df, 
    iter = 30000,
    warmup = 15000,
    thin = 2) {
  
  varde::calc_icc(
    .data = valence_df |> dplyr::filter(Timepoint > 10), 
    subject = "Timepoint", 
    rater = "Rater", 
    scores = "Rating",
    iter = iter,
    warmup = warmup,
    thin = thin,
    file = paste0("data/icc/icc_", info_df$Abbrev),
    silent = 2
  )
}


# Create clip-specific time series plot -----------------------------------

create_valence_plot <- function(valence_df) {
  require("Hmisc")
  valence_df |> 
    ggplot2::ggplot(
      ggplot2::aes(x = Timepoint, y = Rating)
    ) +
    ggplot2::stat_summary(
      geom = "ribbon",
      fun.data = ggplot2::median_hilow,
      fun.args = list(conf.int = .9),
      fill = "#440154"
    ) +
    ggplot2::stat_summary(
      geom = "ribbon",
      fun.data = ggplot2::median_hilow,
      fun.args = list(conf.int = .7),
      fill = "#21908c"
    ) +
    ggplot2::stat_summary(
      geom = "ribbon",
      fun.data = ggplot2::median_hilow,
      fun.args = list(conf.int = .5),
      fill = "#fde725"
    ) +
    ggplot2::geom_hline(
      yintercept = 0, 
      color = "grey", 
      linewidth = 3/4
    ) +
    ggplot2::stat_summary(
      geom = "line", 
      fun = mean, 
      na.rm = TRUE, 
      linewidth = 3/4
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 10*2*30, 30), 
      expand = c(0, 0),
      labels = s_to_ts
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0)
    ) +
    ggplot2::coord_cartesian(
      ylim = c(-4, 4)
    ) +
    ggplot2::labs(
      y = "Valence Rating", 
      x = "Timestamp within Clip"
    )
}


# Create clip-specific wordcloud ------------------------------------------

create_wordcloud_clip <- function(subtitle_df) {
  data("stop_words", package = "tidytext")
  
  bing <- tidytext::get_sentiments(lexicon = "bing")
  
  df <- 
    subtitle_df |> 
    dplyr::summarize(subtitle = stringr::str_c(subtitle, collapse = " ")) |>
    dplyr::mutate(
      subtitle = stringr::str_replace_all(subtitle, "\n", " "),
      subtitle = stringr::str_remove_all(subtitle, "<i>"),
      subtitle = stringr::str_remove_all(subtitle, "</i>"),
      subtitle2 = textclean::replace_contraction(
        subtitle, 
        contraction.key = lexicon::key_contractions
      ),
      subtitle2 = textstem::lemmatize_strings(
        subtitle2, 
        dictionary = lexicon::hash_lemmas
      )
    ) |> 
    tidytext::unnest_tokens("token", "subtitle2", token = "words") |> 
    dplyr::count(token, sort = TRUE) |> 
    dplyr::anti_join(stop_words, by = c("token" = "word")) |> 
    dplyr::left_join(bing, by = c("token" = "word")) |> 
    dplyr::mutate(sentiment = stringr::str_to_title(sentiment))
    # dplyr::top_n(n = 50, wt = n) |> 
    
  df |> 
    ggplot2::ggplot(
      ggplot2::aes(label = token, size = n, color = sentiment)
    ) +
    ggwordcloud::geom_text_wordcloud(show.legend = TRUE) +
    ggplot2::labs(size = "Frequency", color = "Sentiment") +
    ggplot2::scale_size_area(max_size = 10) +
    ggplot2::scale_color_manual(values = c("#d95f02", "#1b9e77")) +
    ggplot2::theme_minimal()
  
}
