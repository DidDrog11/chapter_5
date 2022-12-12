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
         source = "trapping") %>%
  bind_rows(robbins_sle %>%
              filter(m_nat_detection == TRUE) %>%
              mutate(classification = "mastomys natalensis",
                     occurrence = 1,
                     source = "robbins") %>%
              distinct() %>%
              select(classification, occurrence, source))

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
  
robbins_non_detection <- robbins_sle %>%
  filter(m_nat_detection == FALSE) %>%
  mutate(classification = "mastomys_natalensis",
         occurrence = 0,
         source = "robbins",
         detection = FALSE) %>%
  select(classification, geometry, occurrence, source, detection)

## Combine data ------------------------------------------------------------

combined_data <- bind_rows(mastomys_data, chapter_2_mastomys, gbif_mastomys) %>%
  mutate(detection = TRUE) %>%
  bind_rows(trap_non_detection, gbif_non_detection, robbins_non_detection)

empty_geoms <- is.na(st_dimension(combined_data))

combined_data <- combined_data[!empty_geoms, ]

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

if(!file.exists(here("data", "cov_rast_sle.tif"))) {
  
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
  bioclim_sle <- crop(bioclim_sle, vect(sle))
  bioclim_sle <- mask(bioclim_sle, vect(sle))
  
  # Elevation ---------------------------------------------------------------
  
  if(!file.exists(here("data", "covariate_rasters", "wc2.1_country", "SLE_elv_msk.tif"))) {
    
    elevation_30s("SLE", path = here("data", "covariate_rasters"))
    
  }
  
  elevation_sle <- rast(here("data", "covariate_rasters", "SLE_elv_msk.tif"))
  names(elevation_sle) <- "elevation"
  crs(elevation_sle) <- default_CRS
  elevation_sle <- crop(elevation_sle, vect(sle))
  elevation_sle <- mask(elevation_sle, vect(sle))
  
  # Population --------------------------------------------------------------
  if(!file.exists(here("data", "covariate_rasters", "pop_sle.tif"))) {
    
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
  pop_sle$change <- pop_sle$population_2020 - pop_sle$population_2000
  
  # Landcover ---------------------------------------------------------------
  if(!file.exists(here("data", "covariate_rasters", "sle_landuse.tif"))) {
    
    tree_cover <- landcover(var = "trees", path = here("data", "covariate_rasters"))
    cropland_cover <- landcover(var = "cropland", path = here("data", "covariate_rasters"))
    grassland_cover <- landcover(var = "grassland", path = here("data", "covariate_rasters"))
    shrubland_cover <- landcover(var = "shrubs", path = here("data", "covariate_rasters"))
    built_cover <- landcover(var = "built", path = here("data", "covariate_rasters"))
    
    landuse <- c(tree_cover, cropland_cover, grassland_cover, shrubland_cover, built_cover)
    crs(landuse) <- default_CRS
    landuse_sle <- crop(landuse, vect(sle))
    landuse_sle <- mask(landuse_sle, vect(sle))
    
    writeRaster(landuse_sle, here("data", "covariate_rasters", "landuse_sle.tif"))
    
  }
  
  landuse_sle <- rast(here("data", "covariate_rasters", "sle_landuse.tif"))
  
  
  # Friction ----------------------------------------------------------------
  
  if(!file.exists(here("data", "covariate_rasters", "sle_friction.tif"))) {
    
    friction <- rast(here("data", "covariate_rasters", "friction_map", "201501_Global_Travel_Speed_Friction_Surface_2015.tif") )
    crs(friction) <- default_CRS
    friction_sle <- crop(friction, vect(sle))
    friction_sle <- mask(friction_sle, vect(sle))
    
    names(friction_sle) <- "friction"
    
    writeRaster(friction_sle, here("data", "covariate_rasters", "friction_sle.tif"))
    
  }
  
  friction_sle <- rast(here("data", "covariate_rasters", "friction_sle.tif"))
  
  covariate_raster <- c(bioclim_sle, elevation_sle, pop_sle, landuse_sle, friction_sle)
  

  # Processing --------------------------------------------------------------
  
  processed_covariate_raster <- covariate_raster
  processed_covariate_raster$mean_temperature <- scale(processed_covariate_raster$mean_temperature, center = TRUE, scale = TRUE)
  processed_covariate_raster$isothermality <- scale(processed_covariate_raster$isothermality, center = TRUE, scale = TRUE)
  processed_covariate_raster$temperature_seasonality <- scale(processed_covariate_raster$temperature_seasonality, center = TRUE, scale = TRUE)
  processed_covariate_raster$annual_precipitation <- scale(processed_covariate_raster$annual_precipitation, center = TRUE, scale = TRUE)
  processed_covariate_raster$precipitation_seasonality <- scale(processed_covariate_raster$precipitation_seasonality, center = TRUE, scale = TRUE)
  processed_covariate_raster$elevation <- scale(processed_covariate_raster$elevation, center = TRUE, scale = TRUE)
  processed_covariate_raster$log_pop_2020 <- log10(processed_covariate_raster$population_2020)
  processed_covariate_raster$friction <- scale(processed_covariate_raster$friction, center = TRUE, scale = TRUE)
  
  final_cov_raster <- processed_covariate_raster %>%
    select(mean_temperature, isothermality, temperature_seasonality, annual_precipitation, precipitation_seasonality,
           elevation, log_pop_2020, friction, prop_trees = trees, prop_cropland = cropland, prop_grassland = grassland, prop_shrubland = shrubs,
           prop_built = built)
  
  writeRaster(final_cov_raster, here("data", "cov_rast_sle.tif"))
}


## Loading processed raster ------------------------------------------------

# This loads in the processed covariate raster. Mean temp, isothermality, temp seasonality, annual precipitation, precipitation seasonality, elevation and friction have been scaled and mean centered
# Population density in 2020 has been logged to base 10. Landuse layers are proportion of cover for trees, cropland, grassland, shrubland and built environment.

cov_rast_sle <- rast(here("data", "cov_rast_sle.tif"))

# Cell sizes are between 842-848,000 m2, which is 0.84 km2
cellSize(cov_rast_sle)


# Rasterising detection data ----------------------------------------------
# We start with 1052 detection and non-detection points
detections <- rasterize(vect(combined_data %>%
                               select("Detection" = occurrence)), cov_rast_sle[[1]], field = "Detection", fun = "max") %>%
  as.points()
names(detections) <- "m_nat_detection"

# This has been down sampled to a single record per raster cell
# We are left with 66 detection points and 57 non-detection points
table(detections$m_nat_detection)

# We extract the values of the covariates at these detection points
detections_cov <- bind_cols("m_nat_detection" = detections$m_nat_detection, terra::extract(cov_rast_sle, detections, mean))


# Modelling ---------------------------------------------------------------

xvars <- names(detections_cov)[!(names(detections_cov) %in% c("m_nat_detection", "ID"))]
xvars
model_1 <- bart(detections_cov[, xvars], detections_cov[, "m_nat_detection"], keeptrees = TRUE)

m_nat_pred_full_model <- predict(object = model_1,
                                 x.layers = as(cov_rast_sle, "Raster"),
                                 c(0.025, 0.975),
                                 splitby = 20,
                                 quiet = TRUE)

full_model_m_nat <- ggplot() +
  geom_spatraster(data = rast(m_nat_pred_full_model[[1]])) +
  geom_spatvector(data = detections %>%
                    mutate(m_nat_detection = factor(m_nat_detection, levels = c(1, 0), labels = c("Detection", "Non-detection"))), aes(colour = m_nat_detection),
                  alpha = 0.4) +
  scale_fill_viridis_c(limits = c(0, 1), na.value = "transparent") +
  scale_colour_manual(values = c("black", "red")) +
  labs(fill = "Probability of occurrence",
       colour = "Detection (M. natalensis)",
       title = "Occurrence of Mastomys natalensis across Sierra Leone (full model)") +
  theme_bw()

cutoff_full <- 0.58
full_model_binary <- c(rast(m_nat_pred_full_model[[1]]) %>%
                    mutate(layer.1 = factor(case_when(layer.1 > cutoff_full ~ "Present",
                                                      layer.1 <= cutoff_full ~ "Absent"),
                                            levels = c("Present", "Absent"))),
                  rast(m_nat_pred_full_model[[2]]) %>%
                    mutate(layer.2 = factor(case_when(layer.2 > cutoff_full ~ "Present",
                                                      layer.2 <= cutoff_full ~ "Absent"),
                                            levels = c("Present", "Absent"))),
                  rast(m_nat_pred_full_model[[3]]) %>%
                    mutate(layer.3 = factor(case_when(layer.3 > cutoff_full ~ "Present",
                                                      layer.3 <= cutoff_full ~ "Absent"),
                                            levels = c("Present", "Absent"))))
names(full_model_binary) <- c("Central estimate", "2.5% bound", "97.5% bound")

full_model_binary_plot <- ggplot() +
  geom_spatraster(data = full_model_binary[[1]]) +
  geom_spatvector(data = detections %>%
                    mutate(m_nat_detection = factor(m_nat_detection, levels = c(1, 0), labels = c("Detection", "Non-detection"))), aes(colour = m_nat_detection),
                  alpha = 0.4) +
  scale_fill_viridis_d(na.value = "transparent", direction = -1) +
  scale_colour_manual(values = c("black", "red")) +
  labs(fill = "Binary occurrence",
       colour = "Detection (M. natalensis)",
       title = "Binary occurrence of Mastomys natalensis across Sierra Leone (full model)") +
  theme_bw()


# Assess variable importance and perform variable step reduction   
sdm <- bart.step(x.data = detections_cov[,xvars], 
                 y.data = as.data.frame(detections_cov[,"m_nat_detection"]), 
                 full = TRUE,
                 quiet = TRUE)
# The variables selected in this approach are temperature_seasonality, precipitation_seasonality, elevation, log_pop_2020 and the friction raster

# We can then predict this SDM back onto the covariate raster
m_nat_layer <- predict(object = sdm,
                       x.layers = as(cov_rast_sle, "Raster"),
                       c(0.025, 0.975),
                       splitby = 20,
                       quiet = TRUE)

# Visualise the predictions
central_estimate_m_nat <- ggplot() +
  geom_spatraster(data = rast(m_nat_layer[[1]])) +
  geom_spatvector(data = detections %>%
                    mutate(m_nat_detection = factor(m_nat_detection, levels = c(1, 0), labels = c("Detection", "Non-detection"))), aes(colour = m_nat_detection),
                  alpha = 0.4) +
  scale_fill_viridis_c(limits = c(0, 1), na.value = "transparent") +
  scale_colour_manual(values = c("black", "red")) +
  labs(fill = "Probability of occurrence",
       colour = "Detection (M. natalensis)",
       title = "Occurrence of Mastomys natalensis\nacross Sierra Leone") +
  theme_bw()

lower_bound_m_nat <- ggplot() +
  geom_spatraster(data = rast(m_nat_layer[[2]])) +
  geom_spatvector(data = detections %>%
                    mutate(m_nat_detection = factor(m_nat_detection, levels = c(1, 0), labels = c("Detection", "Non-detection"))), aes(colour = m_nat_detection),
                  alpha = 0.4) +
  scale_fill_viridis_c(limits = c(0, 1), na.value = "transparent") +
  scale_colour_manual(values = c("black", "red")) +
  labs(fill = "Probability of occurrence",
       colour = "Detection (M. natalensis)",
       title = "Occurrence of Mastomys natalensis\nacross Sierra Leone (2.5% bound)") +
  theme_bw()

upper_bound_m_nat <- ggplot() +
  geom_spatraster(data = rast(m_nat_layer[[3]])) +
  geom_spatvector(data = detections %>%
                    mutate(m_nat_detection = factor(m_nat_detection, levels = c(1, 0), labels = c("Detection", "Non-detection"))), aes(colour = m_nat_detection),
                  alpha = 0.4) +
  scale_fill_viridis_c(limits = c(0, 1), na.value = "transparent") +
  scale_colour_manual(values = c("black", "red")) +
  labs(fill = "Probability of occurrence",
       colour = "Detection (M. natalensis)",
       title = "Occurrence of Mastomys natalensis\nacross Sierra Leone (97.5% bound)") +
  theme_bw()

uncertainty_m_nat <- ggplot() +
  geom_spatraster(data = rast(m_nat_layer[[3]] - m_nat_layer[[2]])) +
  geom_spatvector(data = detections %>%
                    mutate(m_nat_detection = factor(m_nat_detection, levels = c(1, 0), labels = c("Detection", "Non-detection"))), aes(colour = m_nat_detection),
                  alpha = 0.4) +
  scale_fill_viridis_c(limits = c(0, 1), na.value = "transparent") +
  scale_colour_manual(values = c("black", "red")) +
  labs(fill = "Uncertainty",
       colour = "Detection (M. natalensis)",
       title = "Occurrence of Mastomys natalensis\nacross Sierra Leone (Uncertainty)") +
  theme_bw()

quant_uncertainty <- quantile(values(m_nat_layer[[3]] - m_nat_layer[[2]]),
                              0.75, na.rm = TRUE)

highest_uncertainty_m_nat <- ggplot() +
  geom_spatraster(data = rast(m_nat_layer[[3]] - m_nat_layer[[2]]) > quant_uncertainty) +
  geom_spatvector(data = detections %>%
                    mutate(m_nat_detection = factor(m_nat_detection, levels = c(1, 0), labels = c("Detection", "Non-detection"))), aes(colour = m_nat_detection),
                  alpha = 0.4) +
  scale_fill_viridis_d(na.value = "transparent") +
  scale_colour_manual(values = c("black", "red")) +
  labs(fill = "Highest qunatile",
       colour = "Detection (M. natalensis)",
       title = "Regions of highest uncertainty") +
  theme_bw()

save_plot(plot = plot_grid(plotlist = list(lower_bound_m_nat +
                                             theme(legend.position = "none"),
                                           central_estimate_m_nat +
                                             theme(legend.position = "none"),
                                           upper_bound_m_nat +
                                             theme(legend.position = "none"),
                                           get_legend(lower_bound_m_nat))),
          filename = here("output", "mastomys_probability_occurrence.png"),
          base_height = 8)


# Convert to binary presence-absence
summary(sdm)
cutoff <- 0.67
m_nat_binary <- c(rast(m_nat_layer[[1]]) %>%
                    mutate(layer.1 = factor(case_when(layer.1 > cutoff ~ "Present",
                                                      layer.1 <= cutoff ~ "Absent"),
                                            levels = c("Present", "Absent"))),
                  rast(m_nat_layer[[2]]) %>%
                    mutate(layer.2 = factor(case_when(layer.2 > cutoff ~ "Present",
                                                      layer.2 <= cutoff ~ "Absent"),
                                            levels = c("Present", "Absent"))),
                  rast(m_nat_layer[[3]]) %>%
                    mutate(layer.3 = factor(case_when(layer.3 > cutoff ~ "Present",
                                                      layer.3 <= cutoff ~ "Absent"),
                                            levels = c("Present", "Absent"))))
names(m_nat_binary) <- c("Central estimate", "2.5% bound", "97.5% bound")

m_nat_binary_plot <- ggplot() +
  geom_spatraster(data = m_nat_binary[[1]]) +
  geom_spatvector(data = detections %>%
                    mutate(m_nat_detection = factor(m_nat_detection, levels = c(1, 0), labels = c("Detection", "Non-detection"))), aes(colour = m_nat_detection),
                  alpha = 0.4) +
  scale_fill_viridis_d(na.value = "transparent", direction = -1) +
  scale_colour_manual(values = c("black", "red")) +
  labs(fill = "Binary occurrence",
       colour = "Detection (M. natalensis)",
       title = "Binary occurrence of Mastomys natalensis\nacross Sierra Leone") +
  theme_bw()

m_nat_binary_plot_lower <- ggplot() +
  geom_spatraster(data = m_nat_binary[[2]]) +
  geom_spatvector(data = detections %>%
                    mutate(m_nat_detection = factor(m_nat_detection, levels = c(1, 0), labels = c("Detection", "Non-detection"))), aes(colour = m_nat_detection),
                  alpha = 0.4) +
  scale_fill_viridis_d(na.value = "transparent", direction = -1) +
  scale_colour_manual(values = c("black", "red")) +
  labs(fill = "Binary occurrence",
       colour = "Detection (M. natalensis)",
       title = "Binary occurrence of Mastomys natalensis\nacross Sierra Leone (2.5% bound)") +
  theme_bw()

m_nat_binary_plot_upper <- ggplot() +
  geom_spatraster(data = m_nat_binary[[3]]) +
  geom_spatvector(data = detections %>%
                    mutate(m_nat_detection = factor(m_nat_detection, levels = c(1, 0), labels = c("Detection", "Non-detection"))), aes(colour = m_nat_detection),
                  alpha = 0.4) +
  scale_fill_viridis_d(na.value = "transparent", direction = -1) +
  scale_colour_manual(values = c("black", "red")) +
  labs(fill = "Binary occurrence",
       colour = "Detection (M. natalensis)",
       title = "Binary occurrence of Mastomys natalensis\nacross Sierra Leone (97.5% bound)") +
  theme_bw()

save_plot(plot = plot_grid(plotlist = list(m_nat_binary_plot_lower +
                                             theme(legend.position = "none"),
                                           m_nat_binary_plot +
                                             theme(legend.position = "none"),
                                           m_nat_binary_plot_upper +
                                             theme(legend.position = "none"),
                                           get_legend(m_nat_binary_plot))),
          filename = here("output", "mastomys_binary_occurrence.png"),
          base_height = 8)
