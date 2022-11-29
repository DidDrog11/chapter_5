source(here::here("R", "00_libraries.R"))
source(here("R", "00_load_data.R"))

# Visualise rodent population numbers -------------------------------------

raw_numbers <- rodent_data %>%
  ungroup() %>% 
  select(clean_names, village, visit, site_habitat, date_set)

summary_numbers <- raw_numbers %>%
  group_by(clean_names, village, visit, site_habitat) %>%
  summarise(n = n(),
            date_set = min(date_set))

zero_numbers <- expand_grid(clean_names = unique(summary_numbers$clean_names), trap_nights %>%
                              select(village, visit, site_habitat, date_set)) %>%
  mutate(n_zero = 0)

n_detections <- summary_numbers %>%
  ungroup() %>%
  select(clean_names, village, visit, site_habitat, date_set, n) %>%
  full_join(zero_numbers, .) %>%
  mutate(n = coalesce(n, n_zero)) %>%
  select(-n_zero) %>%
  mutate(week_set = isoweek(date_set),
         year_set = year(date_set),
         week_in_study = case_when(year_set == 2020 ~ week_set,
                                   year_set == 2021 ~ week_set + 52,
                                   year_set == 2022 ~ week_set + 104)) %>%
  arrange(-n) %>%
  group_by(clean_names, village, visit, site_habitat) %>%
  slice(1) %>%
  ungroup()

n_individuals_species <- n_detections %>%
  group_by(clean_names, village, week_in_study, week_set, year_set) %>%
  summarise(n = sum(n)) %>%
  ggplot() +
  geom_point(aes(x = week_in_study, y = n, colour = village)) +
  geom_line(aes(x = week_in_study, y = n, colour = village)) +
  facet_wrap(~ clean_names) +
  theme_bw()

# Estimate abundance from observed ----------------------------------------

## Crocidura --------------------------------------------------------------
produce_fcp <- function(species = "crocidura_spp") {
  
  y <- n_detections %>%
    filter(clean_names == species) %>%
    filter(village != "bambawo") %>%
    group_by(village, site_habitat) %>%
    mutate(R = cur_group_id(),
           J = visit) %>%
    ungroup() %>%
    select(R, J, n) %>%
    arrange(J) %>%
    pivot_wider(names_from = J, names_prefix = "visit_", values_from = n, values_fn = sum) %>%
    arrange(R) %>%
    select(-R) %>%
    as.matrix()
  
  R <- nrow(y) # number of sites

  J <- ncol(y) # number of visits

  site_covs <- n_detections %>%
    filter(clean_names == species) %>%
    filter(village != "bambawo") %>%
    group_by(village, site_habitat) %>%
    mutate(R = cur_group_id(),
           J = visit) %>%
    left_join(trap_nights %>%
                select(-n)) %>%
    group_by(R) %>%
    summarise(village = unique(village),
              site_habitat = unique(site_habitat)) %>%
    select(-R)

  obs_df <-  n_detections %>%
    filter(clean_names == species) %>%
    filter(village != "bambawo") %>%
    group_by(village, site_habitat) %>%
    mutate(R = cur_group_id(),
           J = visit) %>%
    left_join(trap_nights %>%
                rename(tn = n),
              by = c("village", "visit", "site_habitat")) %>%
    ungroup() %>%
    select(R, J, tn, week_set) %>%
    mutate(season = case_when(week_set >= 18 & week_set < 44 ~ "wet",
                              TRUE ~ "dry")) %>%
    select(-week_set)
  
  tn <- obs_df %>%
    select(R, J, tn) %>%
    arrange(J) %>%
    pivot_wider(names_from = J, names_prefix = "visit_", values_from = tn, values_fn = sum) %>%
    arrange(R) %>%
    select(-R) %>%
    as.matrix()
  
  season <- obs_df %>%
    select(R, J, season) %>%
    distinct() %>%
    arrange(J) %>%
    pivot_wider(names_from = J, names_prefix = "visit_", values_from = season) %>%
    arrange(R) %>%
    select(-R) %>%
    as.matrix()
  
  obs_covs <- list(tn = tn,
                   season = season)
 
  return(list(y = y,
              site_covs = site_covs,
              obs_covs = obs_covs)) 
}

crocidura_fcp <- produce_fcp(species = "crocidura_spp")

crocidura_umf <- unmarkedFramePCount(y = crocidura_fcp$y, siteCovs = crocidura_fcp$site_covs, obsCovs = crocidura_fcp$obs_covs)

crocidura_p <- pcount(formula = ~tn + season ~ village + site_habitat, crocidura_umf, mixture = "P", K = 105)
crocidura_nb <- pcount(formula = ~tn + season ~ village + site_habitat, crocidura_umf, mixture = "NB", K = 105)
crocidura_zip <- pcount(formula = ~tn + season ~ village + site_habitat, crocidura_umf, mixture = "ZIP", K = 105)

predicted_crocidura <- posteriorSamples(ranef(crocidura_p), nsims = 100)@samples %>%
  as.data.frame() %>%
  rowwise() %>%
  mutate(mean_n = mean(c_across(cols = everything())),
         sd_n = sd(c_across(cols = everything()))) %>%
  tibble() %>%
  mutate(R = 1:nrow(.)) %>%
  select(R, mean_n, sd_n)
  
observed_crocidura <- n_detections %>%
  filter(clean_names == "crocidura_spp") %>%
  filter(village != "bambawo") %>%
  group_by(village, site_habitat) %>%
  mutate(R = cur_group_id(),
         J = visit) %>%
  arrange(J, R) %>%
  select(J, R, village, site_habitat, n, week_in_study) %>%
  left_join(predicted_crocidura) %>%
  mutate(site = paste0(str_to_sentence(village), " - ", str_to_sentence(site_habitat)),
         site_habitat = str_to_sentence(site_habitat))

crocidura_abundance <- observed_crocidura %>%
  mutate(week_in_study = week_in_study - min(week_in_study)) %>%
  ggplot() +
  geom_point(aes(x = week_in_study, y = n)) +
  geom_line(aes(x = week_in_study, y = mean_n, colour = site_habitat)) +
  geom_ribbon(aes(x = week_in_study, ymin = mean_n - sd_n, ymax = mean_n + sd_n, fill = site_habitat), alpha = 0.2) +
  facet_wrap(~ site) +
  scale_colour_discrete(type = "dark2") +
  scale_fill_discrete(type = "dark2") +
  labs(x = "Week",
       y = "Abundance",
       title = "Crocidura detections and modelled abundance",
       fill = "Land use",
       colour = "Land use") +
  theme_bw()


## Mus musculus -------------------------------------------------------

mus_musculus_fcp <- produce_fcp(species = "mus_musculus")

mus_musculus_umf <- unmarkedFramePCount(y = mus_musculus_fcp$y, siteCovs = mus_musculus_fcp$site_covs, obsCovs = mus_musculus_fcp$obs_covs)

mus_musculus_p <- pcount(formula = ~tn + season ~ village + site_habitat, mus_musculus_umf, mixture = "P", K = 105)
mus_musculus_nb <- pcount(formula = ~tn + season ~ village + site_habitat, mus_musculus_umf, mixture = "NB", K = 105)
mus_musculus_zip <- pcount(formula = ~tn + season ~ village + site_habitat, mus_musculus_umf, mixture = "ZIP", K = 105)

predicted_mus_musculus <- posteriorSamples(ranef(mus_musculus_p), nsims = 100)@samples %>%
  as.data.frame() %>%
  rowwise() %>%
  mutate(mean_n = mean(c_across(cols = everything())),
         sd_n = sd(c_across(cols = everything()))) %>%
  tibble() %>%
  mutate(R = 1:nrow(.)) %>%
  select(R, mean_n, sd_n)

observed_mus_musculus <- n_detections %>%
  filter(clean_names == "mus_musculus") %>%
  filter(village != "bambawo") %>%
  group_by(village, site_habitat) %>%
  mutate(R = cur_group_id(),
         J = visit) %>%
  arrange(J, R) %>%
  select(J, R, village, site_habitat, n, week_in_study) %>%
  left_join(predicted_mus_musculus) %>%
  mutate(site = paste0(str_to_sentence(village), " - ", str_to_sentence(site_habitat)),
         site_habitat = str_to_sentence(site_habitat))

mus_musculus_abundance <- observed_mus_musculus %>%
  mutate(week_in_study = week_in_study - min(week_in_study)) %>%
  ggplot() +
  geom_point(aes(x = week_in_study, y = n)) +
  geom_line(aes(x = week_in_study, y = mean_n, colour = site_habitat)) +
  geom_ribbon(aes(x = week_in_study, ymin = mean_n - sd_n, ymax = mean_n + sd_n, fill = site_habitat), alpha = 0.2) +
  facet_wrap(~ site) +
  scale_colour_discrete(type = "dark2") +
  scale_fill_discrete(type = "dark2") +
  labs(x = "Week",
       y = "Abundance",
       title = "Mus musculus detections and modelled abundance",
       fill = "Land use",
       colour = "Land use") +
  theme_bw()

## Mastomys natalensis -------------------------------------------------------

mastomys_natalensis_fcp <- produce_fcp(species = "mastomys_natalensis")

mastomys_natalensis_umf <- unmarkedFramePCount(y = mastomys_natalensis_fcp$y, siteCovs = mastomys_natalensis_fcp$site_covs, obsCovs = mastomys_natalensis_fcp$obs_covs)

mastomys_natalensis_p <- pcount(formula = ~tn + season ~ village + site_habitat, mastomys_natalensis_umf, mixture = "P", K = 105)
mastomys_natalensis_nb <- pcount(formula = ~tn + season ~ village + site_habitat, mastomys_natalensis_umf, mixture = "NB", K = 105)
mastomys_natalensis_zip <- pcount(formula = ~tn + season ~ village + site_habitat, mastomys_natalensis_umf, mixture = "ZIP", K = 105)

predicted_mastomys_natalensis <- posteriorSamples(ranef(mastomys_natalensis_p), nsims = 100)@samples %>%
  as.data.frame() %>%
  rowwise() %>%
  mutate(mean_n = mean(c_across(cols = everything())),
         sd_n = sd(c_across(cols = everything()))) %>%
  tibble() %>%
  mutate(R = 1:nrow(.)) %>%
  select(R, mean_n, sd_n)

observed_mastomys_natalensis <- n_detections %>%
  filter(clean_names == "mastomys_natalensis") %>%
  filter(village != "bambawo") %>%
  group_by(village, site_habitat) %>%
  mutate(R = cur_group_id(),
         J = visit) %>%
  arrange(J, R) %>%
  select(J, R, village, site_habitat, n, week_in_study) %>%
  left_join(predicted_mastomys_natalensis) %>%
  mutate(site = paste0(str_to_sentence(village), " - ", str_to_sentence(site_habitat)),
         site_habitat = str_to_sentence(site_habitat))

mastomys_natalensis_abundance <- observed_mastomys_natalensis %>%
  mutate(week_in_study = week_in_study - min(week_in_study)) %>%
  ggplot() +
  geom_point(aes(x = week_in_study, y = n)) +
  geom_line(aes(x = week_in_study, y = mean_n, colour = site_habitat)) +
  geom_ribbon(aes(x = week_in_study, ymin = mean_n - sd_n, ymax = mean_n + sd_n, fill = site_habitat), alpha = 0.2) +
  facet_wrap(~ site) +
  scale_colour_discrete(type = "dark2") +
  scale_fill_discrete(type = "dark2") +
  labs(x = "Week",
       y = "Abundance",
       title = "Mastomys natalensis detections and modelled abundance",
       colour = "Landuse",
       fill = "Landuse") +
  theme_bw()

# Save plots --------------------------------------------------------------
dir.create("output")

save_plot(plot = crocidura_abundance, filename = here("output", "crocidura_abundance.png"), base_height = 8)
save_plot(plot = mus_musculus_abundance, filename = here("output", "mus_abundance.png"), base_height = 8)
save_plot(plot = mastomys_natalensis_abundance, filename = here("output", "mastomys_abundance.png"), base_height = 8)

