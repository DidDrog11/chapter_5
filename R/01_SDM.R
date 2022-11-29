source(here::here("R", "00_libraries.R"))
source(here("R", "00_load_data.R"))


# Trial of Mastomys natalensis only ---------------------------------------

## Trap data ---------------------------------------------------------------

mastomys_data <- rodent_data %>%
  filter(clean_names == "mastomys_natalensis") %>%
  mutate(clean_names = "mastomys natalensis") %>%
  ungroup() %>%
  select(classification = clean_names, geometry) %>%
  st_as_sf(crs = default_CRS) %>%
  mutate(occurrence = 1,
         source = "trapping")

non_mastomys_locations <- rodent_data %>%
  group_by(village, site_habitat, clean_names) %>%
  summarise(n = n()) %>%
  mutate(mastomys_detected = case_when(clean_names == "mastomys_natalensis" ~ TRUE,
                                       TRUE ~ FALSE)) %>%
  arrange(-mastomys_detected) %>%
  slice(1) %>%
  filter(mastomys_detected != TRUE) %>%
  select(village, site_habitat)

# Non detection data

trap_non_detection <- rodent_data %>%
  filter(village %in% non_mastomys_locations$village & site_habitat %in% non_mastomys_locations$site_habitat) %>%
  mutate(clean_names = "mastomys natalensis") %>%
  ungroup() %>%
  select(classification = clean_names, geometry) %>%
  st_as_sf(crs = default_CRS) %>%
  mutate(occurrence = 0, # Detection of non-mastomys in a location where mastomys hasn't been detected is classed as 0
         source = "trapping",
         detection = FALSE)

## Chapter 2 data ----------------------------------------------------------

if(!file.exists(here("data", "chapter_2_data.rds"))) readRDS(gzcon(url("https://github.com/DidDrog11/scoping_review/raw/main/data_clean/rodent_df.rds"))) %>%
  write_rds(here("data", "chapter_2_data.rds"))

chapter_2_mastomys <- read_rds(here("data", "chapter_2_data.rds")) %>%
  filter(classification == "mastomys natalensis") %>%
  filter(iso3c == "SLE") %>%
  drop_na(longitude, latitude) %>%
  st_transform(crs = default_CRS) %>%
  select(classification, geometry) %>%
  mutate(occurrence = 1, # No non-detections
         source = "review")

## GBIF data ---------------------------------------------------------------

if(!file.exists(here("data", "gbif_mastomys.rds"))) read_tsv(gzcon(url("https://github.com/DidDrog11/scoping_review/raw/main/data_download/gbif_species/mastomys_natalensis_gbif.csv"))) %>%
  filter(!str_detect(issue, "COORDINATE_ROUNDED")) %>%
  select(gbifID, species, countryCode, decimalLatitude, decimalLongitude) %>%
  mutate(iso3c = countrycode(countryCode, origin = "iso2c", destination = "iso3c")) %>%
  write_rds(here("data", "gbif_mastomys.rds"))

gbif_mastomys <- read_rds(here("data", "gbif_mastomys.rds")) %>%
  filter(iso3c == "SLE") %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = default_CRS) %>%
  select(classification = species) %>%
  mutate(occurrence = 1, # No non-detections
         classification = "mastomys natalensis",
         source = "gbif")

# Non detection data

gbif_rodents <- read_tsv(here("data", "rodents_sle.csv")) %>%
  filter(!str_detect(issue, "COORDINATE_ROUNDED")) %>%
  select(gbifID, species, countryCode, decimalLatitude, decimalLongitude) %>%
  mutate(iso3c = countrycode(countryCode, origin = "iso2c", destination = "iso3c")) %>%
  drop_na(species, decimalLongitude, decimalLatitude) %>%
  mutate(species = str_to_lower(species)) %>%
  group_by(species, decimalLatitude, decimalLongitude) %>%
  summarise(n = n())

gbif_mastomys_locations <- gbif_rodents %>%
  ungroup() %>%
  mutate(mastomys_detected = case_when(species == "mastomys natalensis" ~ TRUE,
                                       TRUE ~ FALSE)) %>%
  filter(mastomys_detected == TRUE)

gbif_non_detection <- gbif_rodents %>%
  filter(!c(decimalLatitude %in% gbif_mastomys_locations$decimalLatitude & decimalLongitude %in% gbif_mastomys_locations$decimalLongitude)) %>%
  filter(decimalLongitude > -20) %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = default_CRS) %>%
  select(classification = species) %>%
  mutate(occurrence = 0, # Non-detections are detections of other species at sites where mastomys wasnt reported
         classification = "mastomys natalensis",
         source = "gbif",
         detection = FALSE)
  

## Combine data ------------------------------------------------------------

combined_data <- bind_rows(mastomys_data, chapter_2_mastomys, gbif_mastomys) %>%
  mutate(detection = TRUE) %>%
  bind_rows(trap_non_detection, gbif_non_detection)

# Spatial data ------------------------------------------------------------
dir.create(here("data", "spatial"))

gadm(country = "SLE", level = 2, path = here("data", "spatial")) %>%
  st_as_sf() %>%
  st_transform(crs = default_CRS) %>%
  write_rds(here("data", "spatial", "SLE_sf.rds"))

sle <- read_rds(here("data", "spatial", "SLE_sf.rds"))

combined_data %>%
  ggplot() +
  geom_sf(data = sle) +
  geom_sf(aes(colour = source, shape = detection)) +
  theme_bw()

# Covariate rasters -------------------------------------------------------
dir.create(here("data", "covariate_rasters"))

## Worldclim -----------------------------------------------------------------

if(!file.exists(here("data", "covariate_rasters", "wc2.1_country", "SLE_wc2.1_30s_bio.tif"))) {
  
  worldclim_country("SLE", var = "bio", res = 0.5, path = here("data", "covariate_rasters"))
  
}

bioclim_sle <- rast(here("data", "covariate_rasters", "wc2.1_country", "SLE_wc2.1_30s_bio.tif"))
names(bioclim_sle) <- c("mean_temperature", "mean_dirunal_range", "isothermality", "temperature_seasonality",
                        "max_temp_warmest", "min_temp_coldest", "temp_range", "mean_temperature_wettest",
                        "mean_temperature_driest", "mean_temperature_warmest", "mean_temperature_coldest",
                        "annual_precipitation", "precipitation_wettest_month", "precipitation_driest_month",
                        "precipitation_seasonality", "precipitation_wettest_quarter", "precipitation_driest_quarter",
                        "precipitation_warmest", "precipitation_coldest")
crs(bioclim_sle) <- default_CRS


## Elevation ---------------------------------------------------------------

if(!file.exists(here("data", "covariate_rasters", "wc2.1_country", "SLE_elv_msk.tif"))) {
  
  elevation_30s("SLE", path = here("data", "covariate_rasters"))
  
}

elevation_sle <- rast(here("data", "covariate_rasters", "SLE_elv_msk.tif"))
names(elevation_sle) <- "elevation"
crs(elevation_sle) <- default_CRS

## Population --------------------------------------------------------------
if(!file.exists(here("data", "covariate_raster_pop", "pop_2000_sle.tif"))) {
  
  population(year = 2000, res = 0.5, path = here("data", "covariate_rasters"))
  
  pop_2000 <- rast(here("data", "covariate_rasters", "pop", "gpw_v4_population_density_rev11_2000_30s.tif"))
  names(pop_2000) <- "population_2000"
  crs(pop_2000) <- default_CRS
  pop_2000_sle <- crop(pop_2000, vect(sle))
  pop_2000_sle <- mask(pop_2000_sle, vect(sle), overwrite = TRUE)
  
  population(year = 2020, res = 0.5, path = here("data", "covariate_rasters"))
  
  pop_2020 <- rast(here("data", "covariate_rasters", "pop", "gpw_v4_population_density_rev11_2020_30s.tif"))
  names(pop_2020) <- "population_2020"
  crs(pop_2020) <- default_CRS
  pop_2020_sle <- crop(pop_2020, vect(sle))
  pop_2020_sle <- mask(pop_2020_sle, vect(sle), overwrite = TRUE)
  
  pop_sle <- c(pop_2000_sle, pop_2020_sle)
  
  writeRaster(pop_sle, here("data", "covariate_rasters", "pop_sle.tif"))

}

pop_sle <- rast(here("data", "covariate_rasters", "pop_sle.tif"))
