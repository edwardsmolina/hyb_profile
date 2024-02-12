library("sf")
library("leaflet")
library("viridis")

pipes <- st_read("shape/MD pipelines.shp") %>% 
  rmapshaper::ms_simplify() %>% 
  st_transform(4326)

st_write(pipes, dsn="data/pipelines_polygon.gpkg", layer='pipelines')

pipes <- st_read("data/pipelines_polygon.gpkg") %>% 
  st_transform(4326)

pipe_names <- pipes$pipeline %>% unique()
pal <- colorFactor(viridis_pal(option = "D")(length(pipe_names)), 
                   domain = pipe_names)

leaflet()  %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=pipes, weight=1, 
              color = ~colorFactor("RdYlBu", pipeline), 
              # color = ~pal(pipeline), 
              fillOpacity = 0.3, 
              popup=~pipeline) 

