if (!require("pacman")) install.packages("pacman")

pkgs =
  c("cowplot",
    "here",
    "lubridate",
    "sf",
    "tidyverse",
    "unmarked"
  )

pacman::p_load(pkgs, character.only = T)

default_CRS <- "EPSG:4326"
SL_UTM <- "EPSG:32629"
