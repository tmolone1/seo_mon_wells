library(googledrive)
library(googlesheets4)
load("./data/seo_mon_wells.Rda")

options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)
