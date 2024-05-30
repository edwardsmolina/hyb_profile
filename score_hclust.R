library(tidyverse)

df <- tibble::tribble(
  ~`Pudrición.de.tallo.(SROTP_SHBPP)`, ~pud.tallo,
                                  55,       "4MS",
                                  50,        "3M",
                                  36,        "3M",
                                  51,        "3M",
                                  50,       "2MT",
                                  45,       "2MT",
                                  38,       "2MT",
                                  44,        "3M",
                                  51,       "2MT",
                                  37,       "2MT",
                                  50,        "3M",
                                  30,       "2MT",
                                  51,        "3M",
                                  46,       "2MT",
                                  35,       "2MT"
  )
df %>% 
  ggplot() + 
  aes(x=pud.tallo, `Pudrición.de.tallo.(SROTP_SHBPP)` ) + 
  geom_point()
  
library(tidyverse)

df1 <- tibble::tribble(
       ~Hybrid, ~BLUP_PRED,         ~Category,   ~Disease,  ~Submarket,       ~Brand,
        "ALCE",        3.1,        "Tolerant",  "EAR_ROT", "High_tier",     "ASGROW",
        "ALCE",        3.8,        "Tolerant", "FUSARIUM", "High_tier",     "ASGROW",
        "ALCE",        4.6,    "Mid Tolerant",      "GLS", "High_tier",     "ASGROW",
    "ANTILOPE",        1.2,        "Tolerant",  "EAR_ROT", "High_tier",     "ASGROW",
    "ANTILOPE",        5.7, "Mid Susceptible", "FUSARIUM", "High_tier",     "ASGROW",
    "ANTILOPE",        4.7,    "Mid Tolerant",      "GLS", "High_tier",     "ASGROW",
    "BERRENDO",        0.6,        "Tolerant",  "EAR_ROT", "High_tier",     "ASGROW",
    "BERRENDO",        4.4,    "Mid Tolerant", "FUSARIUM", "High_tier",     "ASGROW",
    "BERRENDO",        4.5,    "Mid Tolerant",      "GLS", "High_tier",     "ASGROW",
    "CAMALEON",          8, "Mid Susceptible",  "EAR_ROT", "High_tier",     "ASGROW",
       "CB100",        9.1,    "Mid Tolerant",  "EAR_ROT",  "Mid_tier",         "CB",
       "CB100",        3.6,        "Tolerant", "FUSARIUM",  "Mid_tier",         "CB",
       "CB100",        3.7,        "Tolerant",      "GLS",  "Mid_tier",         "CB",
       "CRM77",        5.9,    "Mid Tolerant",  "EAR_ROT",  "Mid_tier",         "CB",
       "CRM77",        3.5,        "Tolerant", "FUSARIUM",  "Mid_tier",         "CB",
       "CRM77",        3.7,        "Tolerant",      "GLS",  "Mid_tier",         "CB",
      "DK2037",        2.4,        "Tolerant",  "EAR_ROT", "High_tier",     "DEKALB",
      "DK2037",        4.2,    "Mid Tolerant", "FUSARIUM", "High_tier",     "DEKALB",
      "DK2037",        4.8,    "Mid Tolerant",      "GLS", "High_tier",     "DEKALB",
      "DK4018",        4.9,    "Mid Tolerant",  "EAR_ROT", "High_tier",     "DEKALB",
      "DK4018",        4.1,        "Tolerant", "FUSARIUM", "High_tier",     "DEKALB",
      "DK4018",        4.6,    "Mid Tolerant",      "GLS", "High_tier",     "DEKALB",
      "DK4021",        3.5,    "Mid Tolerant",  "EAR_ROT", "High_tier",     "DEKALB",
      "DK4021",        3.9,        "Tolerant", "FUSARIUM", "High_tier",     "DEKALB",
      "DK4021",        4.7,    "Mid Tolerant",      "GLS", "High_tier",     "DEKALB",
      "KODIAK",        2.4,        "Tolerant",  "EAR_ROT", "High_tier",     "ASGROW",
      "KODIAK",          4,        "Tolerant", "FUSARIUM", "High_tier",     "ASGROW",
      "KODIAK",        4.5,    "Mid Tolerant",      "GLS", "High_tier",     "ASGROW",
      "MV8459",        3.8,    "Mid Tolerant",  "EAR_ROT",  "Mid_tier",      "BAYER",
      "MV8459",        4.7,    "Mid Tolerant", "FUSARIUM",  "Mid_tier",      "BAYER",
      "MV8459",        3.3,        "Tolerant",      "GLS",  "Mid_tier",      "BAYER",
      "MX8296",        2.3,        "Tolerant",  "EAR_ROT", "High_tier",      "BAYER",
      "MX8296",        4.8,    "Mid Tolerant", "FUSARIUM", "High_tier",      "BAYER",
      "MX8296",        4.6,    "Mid Tolerant",      "GLS", "High_tier",      "BAYER",
      "P3011W",        3.6,    "Mid Tolerant",  "EAR_ROT", "High_tier", "COMPETIDOR",
      "P3011W",        3.2,        "Tolerant", "FUSARIUM", "High_tier", "COMPETIDOR",
      "P3011W",        4.9,    "Mid Tolerant",      "GLS", "High_tier", "COMPETIDOR",
      "P3265W",        2.3,        "Tolerant",  "EAR_ROT", "High_tier", "COMPETIDOR",
      "P3265W",          2,        "Tolerant", "FUSARIUM", "High_tier", "COMPETIDOR",
      "P3265W",          5,        "Moderate",      "GLS", "High_tier", "COMPETIDOR",
  "SALAMANDRA",        2.7,        "Tolerant",  "EAR_ROT", "High_tier",     "ASGROW",
  "SALAMANDRA",        3.6,        "Tolerant", "FUSARIUM", "High_tier",     "ASGROW",
  "SALAMANDRA",        4.3,        "Tolerant",      "GLS", "High_tier",     "ASGROW",
     "SAMURAI",        4.3,    "Mid Tolerant",  "EAR_ROT",  "Mid_tier", "COMPETIDOR",
     "SAMURAI",        3.2,        "Tolerant", "FUSARIUM",  "Mid_tier", "COMPETIDOR",
     "SAMURAI",        3.1,        "Tolerant",      "GLS",  "Mid_tier", "COMPETIDOR",
      "VX8563",        0.9,        "Tolerant",  "EAR_ROT", "High_tier",      "BAYER",
      "VX8563",        3.6,        "Tolerant", "FUSARIUM", "High_tier",      "BAYER",
      "VX8563",        4.2,        "Tolerant",      "GLS", "High_tier",      "BAYER",
      "VX8567",        1.3,        "Tolerant",  "EAR_ROT", "High_tier",      "BAYER",
      "VX8567",          4,        "Tolerant", "FUSARIUM", "High_tier",      "BAYER",
      "VX8567",        4.3,        "Tolerant",      "GLS", "High_tier",      "BAYER"
  )

df <- df1 %>% 
  select(-Category, -Submarket, -Brand) %>% 
  pivot_wider(
    names_from = "Disease",
    values_from = "BLUP_PRED"
    ) %>% 
  drop_na() %>% 
  column_to_rownames("Hybrid")


# Matriz de distancias
d <- dist(df)

# Clúster jerárquico
hc <- hclust(d)

# Dendrograma
plot(hc)

library(fmsb)
radarchart(df)


df <- df1 %>% 
  select(-BLUP_PRED,) %>% 
  pivot_wider(
    names_from = "Disease",
    values_from = "Category"
    ) %>% rio::export("PAM_scores.xlsx")
  drop_na() %>% 
  column_to_rownames("Hybrid")

  
  # Install the required package ggradar
library(devtools)
devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE) # you may need to run this command twice
  
  # Load the libraries
library(ggradar)
library(dplyr)
library(scales)
  
  # First reload the original data, because we need "acteur" in the first column then do:
mutate_at(data,vars(-acteur),rescale) %>% ggradar()    

notas <- tibble::tribble(
    ~value, ~score,
          1L,      "T",
          2L,     "MT",
          3L,     "M",
          4L,     "MS",
          5L,      "S"
    ) 

df <- tibble::tribble(
    ~geno, ~Roya.Común, ~Tizón.Común, ~Mancha.Gris, ~Mancha.de.Asfalto, ~carbon,
    "KODIAK",        "MT",          "S",         "MT",               "MT",     "S",
    # "SALAMANDRA",         "T",          "T",          "T",                "M",    "MS",
    "TIGRILLO",         "M",         "MT",         "MT",                "M",    "MS"
  )
  
df1 <- df %>% 
  pivot_longer(-geno, values_to = "score") %>% 
  left_join(notas) %>% 
  select(-score) %>% 
  pivot_wider(names_from=name, values_from = value) %>% 
  column_to_rownames("geno")

df2 <-  rbind(rep(1,5) , rep(5,5), df1)

library(RColorBrewer)
library(fmsb)
coul <- brewer.pal(3, "Paired")
colors_in <- alpha(coul,0.1)

radarchartcirc(df2, axistype=1, 
           pfcol=colors_in, plwd=4 , plty=1, pcol=coul,
           cglcol="grey", cglty=1, caxislabels=notas$score, axislabcol=1, cglwd=0.8,
           #custom labels
           vlcex=0.8 )

legend(x=0.7, y=1, legend = rownames(df2[-c(1,2),]), bty = "n", pch=20 , col=coul , cex=1.2, pt.cex=3)


library(scales)

rescale(5:10, to = c(1, 5))

rescale(runif(5), to = c(1, 5))
rescale(1)

