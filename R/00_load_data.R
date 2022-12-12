combined_data <- readRDS(gzcon(url("https://github.com/DidDrog11/rodent_trapping/raw/main/data/data_for_export/combined_data.rds")))

# Classify rodent species for now based on locations
rodent_data <- combined_data$rodent_data %>%
  select(rodent_uid, trap_uid, clean_names, sex, age_group, weight, head_body, tail, hind_foot, ear, length_skull, testes, seminal_vesicles, vagina_perforate, pairs_teats, number_embryos) %>%
  mutate(trap_uid = case_when(trap_uid == "baiama_7_3_1_279" ~ "baiama_7_3_7_279",
                              trap_uid == "lamabayama_4_1_7_192" ~ "lamabayama_4_1_4_192",
                              trap_uid == "lamabayama_4_1_7_196" ~ "lamabayama_4_1_4_196",
                              trap_uid == "lamabayama_4_1_7_150" ~ "lamabayama_4_1_4_150",
                              trap_uid == "lamabayama_4_1_7_151" ~ "lamabayama_4_1_4_151",
                              TRUE ~ as.character(trap_uid)),
         rodent_uid = case_when(rodent_uid == "5_LAL_001" & trap_uid == "lalehun_5_1_2_83" ~ "5_LAL_016",
                                rodent_uid == "7_BAI_010" & trap_uid == "baiama_7_2_7_211" ~ "5_BAI_023",
                                TRUE ~ rodent_uid))

trap_data <- combined_data$trap_data %>%
  select(trap_uid, date_set, village, visit, site_habitat, geometry) %>%
  mutate(site_habitat = factor(case_when(str_detect(site_habitat, "banana|cassava|agriculture|fallow|quarry|plantation|field") ~ "agriculture",
                                         str_detect(site_habitat, "forest") ~ "forest",
                                         str_detect(site_habitat, "village") ~ "village"), levels = c("forest", "agriculture", "village")))

trap_nights <- trap_data %>%
  tibble() %>%
  select(-geometry) %>%
  group_by(village, visit, site_habitat) %>%
  summarise(n = n(),
            date_set = min(date_set)) %>%
  ungroup()

combined <- left_join(rodent_data, trap_data) %>%
  fill(date_set, .direction = "downup") %>% # for now assume if trap is missing that it is on the same date as the previous rodent
  fill(site_habitat, .direction = "downup") %>% # for now assume if land use is missing that it is in the same location as the previous rodent
  fill(village, .direction = "updown") %>% # for now assume if village is missing that it is in the same location as the next rodent
  fill(visit, .direction = "updown") %>% # for now assume if visit is missing that it is in the same location as the next rodent
  distinct() %>%
  group_by(rodent_uid) %>%
  slice(1) %>%
  mutate(clean_names = case_when(village == "lambayama" & str_detect(clean_names, "mus_") ~ "mus_musculus",
                                 !str_detect(site_habitat, "village") & str_detect(clean_names, "mus_") ~ "mus_minutoides",
                                 str_detect(clean_names, "mus_") ~ "mus_musculus",
                                 str_detect(clean_names, "mastomys_") ~ "mastomys_natalensis",
                                 str_detect(clean_names, "rattus_") ~ "rattus_rattus",
                                 str_detect(clean_names, "lophuromys_") ~ "lophuromys_sikapusi",
                                 TRUE ~ clean_names)) %>%
  filter(str_detect(clean_names, "crocidura|lophuromys|malacomys|mastomys|mus|praomys|rattus"))

write_rds(combined, here("data", "rodent_data.rds"))
write_rds(trap_nights, here("data", "trap_nights.rds"))

robbins_sle <- read_csv(here("data", "robbins_1983.csv"), show_col_types = FALSE) %>%
  mutate(m_nat_detection = case_when(m_natalensis > 0 ~ TRUE,
                                     TRUE ~ FALSE)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = default_CRS)

rm(list = c("combined", "combined_data", "rodent_data", "trap_data", "trap_nights"))

rodent_data <- read_rds(here("data", "rodent_data.rds"))
trap_nights <- read_rds(here("data", "trap_nights.rds"))
