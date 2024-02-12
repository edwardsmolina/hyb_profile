library(dplyr)
library(sf)
library(viridis)

pipes <- st_read("data/pipelines_polygon.gpkg") %>% 
  st_transform(4326)


dat  <- rio::import("data/data_norla_shiny.csv")
# Choices for drop-downs
# vars <- c(
#   "Calidad de grano" ="DSR_GQU",
#   "Fusarium sp" = "DSR_FFR",
#   "Pudrición de mazorca mix" = "EROTP_ERMX",
#   "Pudrición de tallo" = "SROTP_SHBPP",
#   "Mancha de asfalto" = "DSR_PHYRMA",
#   "Cercospora" = "DSR_FIRSTR",
#   "Head_Smut" ="DSR_SMUTPC",	
#   "Common rust" ="DSR_PUCCSO", 
#   "Corn stunt" ="LDICE", 
#   "Corn stunt" ="GQU"
#   )

hyb_dat <- rio::import("data/perfil_sanitario_hibridos_cultivio_23.xlsx", sheet="full", skip=3)
names(hyb_dat) #%>% tibble(vars=.) %>% rio::export("data/hyb_vars.xlsx")
#   datapasta::df_paste
  
fungi_dat <- rio::import("data/fungi_cleaned.csv")

trait_dat <- rio::import("data/perfil_sanitario_hibridos_cultivio_23.xlsx", sheet="current_traits")
# names(trait_dat)
# trait_dat$trait_grupo
# unique(trait_dat$obs_code)
