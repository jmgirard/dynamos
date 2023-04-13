library(tidyverse)
library(srt)

source("./R/functions.R")
info_df <- get_info_df()

raw_files <- dir(path = "./Data/Subtitles/Raw/", pattern = ".srt*", full.names = TRUE)

raw_lst <- map(raw_files, read_srt, collapse = "\n")
names(raw_lst) <- info_df$Abbrev
raw_df <- list_rbind(raw_lst, names_to = "Abbrev")

tidy_df <- 
  raw_df |> 
  left_join(
    info_df |> select(Abbrev, Clip_Start), 
    by = "Abbrev"
  ) |> 
  transmute(
    Abbrev,
    n,
    start = start - ts_to_s(Clip_Start),
    start = if_else(start < 0, 0, start),
    end = end - ts_to_s(Clip_Start),
    subtitle
  ) |> 
  group_by(Abbrev) |> 
  mutate(n = as.integer(n - min(n) + 1)) |> 
  ungroup() |> 
  print()

write_rds(tidy_df, file = "./Data/tidy_subtitles.rds")

new_srt <- function(abbrev) {
  df <- 
    tidy_df |> 
    filter(Abbrev == abbrev) |> 
    select(-Abbrev)
  write_srt(df, path = str_glue("./Data/Subtitles/{abbrev}.srt"), wrap = FALSE)
}

walk(info_df$Abbrev, new_srt)
