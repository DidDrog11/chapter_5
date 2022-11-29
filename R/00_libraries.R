if (!require("pacman")) install.packages("pacman")

pkgs =
  c("countrycode",
    "cowplot",
    "geodata",
    "googledrive",
    "here",
    "lubridate",
    "sf",
    "terra",
    "tidyterra",
    "tidyverse",
    "unmarked"
  )

pacman::p_load(pkgs, character.only = T)

default_CRS <- "EPSG:4326"
SL_UTM <- "EPSG:32629"
