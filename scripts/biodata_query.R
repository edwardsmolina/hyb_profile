# bajar la ultima version de los biodatasets de cada cluster
# el proyecto que actualiza estas bases es "corn_biotech_data" y tiene un job diario

pacman::p_load(arrow, bigrquery, tidyverse)

source("/mnt/bq_utils.R", encoding = "UTF-8")
.MDlake <- "bcs-market-dev-lake"

# diseases data -----
biodat_norla <- bq_dnld(project = .MDlake, query = "SELECT * FROM latam_md_corn.biodata_norla") 
biodat_conosur <- bq_dnld(project = .MDlake, 
                          query = "SELECT * FROM latam_md_corn.biotech_conosur") 
biodat_norla %>% 
  unite("var_ID", OBS_observationRefCd, OBS_descriptorAbbreviation, 
        remove = FALSE) %>% 
  mutate(year_loc = interaction(FIELD_plantingSeason, FIELD_name)) %>% drop_na(OBS_numValue) %>% 
  rio::export("data/biodata_norla.csv")

# raw <- read.csv("data/biodata_norla.csv")
# 
# raw %>%
#   group_by(FIELD_pipeline, FIELD_plantingSeason, year_loc, var_ID) %>%
#   summarise(dis_press = quantile(OBS_numValue, .9)) %>%
#   left_join(raw %>% select(year_loc, contains("tude")) %>% distinct()) %>%
#   rename(lat = "FIELD_field_latitude", lon="FIELD_field_longitude") %>%
#   mutate_at("dis_press", as.numeric) %>%
#   ungroup() %>%
#   data.frame %>%
#   filter(!str_detect(var_ID, "YLD")) %>%
#   filter(dis_press < 100) %>%
#   group_by(FIELD_pipeline, var_ID) %>%
#   mutate(scaled_val = scale(dis_press)) %>%
#   drop_na(scaled_val) %>%
#   rio::export("data/data_norla_shiny.csv")

# fungicide data -----
janitor::compare_df_cols(biodat_norla, biodat_conosur)

