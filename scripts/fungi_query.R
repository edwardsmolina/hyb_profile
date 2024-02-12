### Bases
conosur_current <- "latam_datasets.full_conosur_current_corn"
# conosur_historical <- "latam_datasets.hss_conosur_historical_corn"

field_info <- "FIELD_Country, FIELD_field_latitude, FIELD_field_longitude, FIELD_name, FIELD_County, FIELD_plantingSeason, FIELD_pipeline, FIELD_year"
product_info <- "commercialName, createdBrand, createdTechnology"
protocol_info <- "protocolTitle, SETS_setName"
trait_info <- "OBS_observationRefCd, OBS_descriptorAbbreviation, OBS_numValue, REP_repetitionNumber, QC_Flag, OBS_growthStage"
cols <- paste0(field_info, ", ", product_info, ", ", trait_info, ", ", protocol_info)
diseases <- "'LDSR'"
agronomic <- "'YLD'"
traits <- paste0("(", diseases, ",", agronomic, ")")
countries <- "('Argentina')"

query_fungi <- function(x) {
  paste("SELECT DISTINCT", cols, 
        "FROM ", x,
        "WHERE OBS_observationRefCd IN", traits,
        "AND FIELD_country IN", countries, 
        "AND cmName_Analisys_Filter IS NULL",
        "AND QC_Flag IS NULL", 
        "AND protocolTitle LIKE '%FTN%'", 
        "AND FIELD_name LIKE '%FUNG%'")
}

fungi_current <- bq_dnld(project = .MDlake, query = query_fungi(conosur_current))
fungi_current 
# historical <- bq_dnld(project = .MDlake, query = query_fungi_FTN(base_current)


fungi_current <- rio::import("data/fungi_FTN_24.csv")

fungi_current %>% 
  count(FIELD_name, SETS_setName, commercialName) %>% 
  filter(str_detect(SETS_setName, "FUNG")) %>% 
  distinct(FIELD_name, commercialName) -> field_hyb

dat0 <- field_hyb %>%  
  left_join(fungi_current) %>% 
  unite("var_id", OBS_observationRefCd, OBS_descriptorAbbreviation)
names(dat0)

dat <- dat0 %>% 
  select(FIELD_name, FIELD_pipeline, commercialName, SETS_setName, var_id, OBS_growthStage, val=OBS_numValue) %>%  
  unite("GS_var", OBS_growthStage, var_id) %>% 
  mutate(trt = case_when(
    str_detect(SETS_setName, 'FUNG') ~'CRIPTON_XPRO',
    .default ='CHECK')) %>% 
  group_by(FIELD_name, FIELD_pipeline, commercialName, trt, GS_var) %>% 
  summarise(val=mean(val)) %>% 
  pivot_wider(names_from = GS_var, 
              values_from = val) %>% 
  ungroup() %>%
  rename(YLD="Not Applicable_YLD_N/A") %>% 
  drop_na(YLD) %>% 
  select(-contains("LDSR")) %>% 
  pivot_wider(names_from = trt, 
              values_from = YLD) %>% 
  mutate(ratio=CRIPTON_XPRO/CHECK, 
         yield_response=(ratio-1)*100) %>% 
  mutate(Season = case_when(
    str_detect(FIELD_name, "EARLY") ~ "Early", 
    str_detect(FIELD_name, "LATE") ~ "Late", 
    TRUE~ "-")) %>% 
  mutate(FIELD_pipeline = case_when(
    str_detect(FIELD_pipeline, "North") ~ "Reg. Norte", 
    TRUE ~ "Reg. Pampeana"))

dat %>% rio::export("data/fungi_cleaned.csv")

dat 