# Mapas de barrios de Buenos Aires a partir de Open Street Map y geometrías del GCBA
# A partir de https://ggplot2tutor.com/tutorials/streetmaps

############################################################
library(osmdata)
library(tidyverse)
library(sf)

# Levantamos el dataset de barrios de CABA con sus polígonos
barrios_caba <- read.csv(sep=";","https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-educacion/barrios/barrios.csv") %>% 
  st_as_sf(wkt = "WKT")

# Calculamos las lat-lon máximas y mínimas
bbox_list <- lapply(st_geometry(barrios_caba), st_bbox)

# Pasamos a data frame
maxmin <- do.call(rbind.data.frame, bbox_list)

# Nombres de columnas
names(maxmin) <- names(bbox_list[[1]])

# Lo agregamos al dataframe de barrios
barrios_geom <- bind_cols(barrios_geo,maxmin)

# Elegimos un barrio para dibujar
barrios <- barrios_caba$BARRIO

barrio <- barrios_geom %>% 
  filter(BARRIO == "PATERNAL")

# Seleccionamos los límites de lat-lon
barrio_bb <- as.matrix(data.frame(row.names = c("x","y"),min = c(barrio$xmin,barrio$ymin),
                                    max=c(barrio$xmax,barrio$ymax)))
# Extraemos todas las calles principales de OSM

calles <- barrio_bb %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

calles_2 <- barrio_bb %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

trenes <- barrio_bb %>%
  opq()%>%
  add_osm_feature(key = "railway", 
                  value = c("rail")) %>%
  osmdata_sf() 

# Graficamos con ggplot
ggplot() +
  geom_sf(data=st_set_crs(barrios_geom,4326) %>% filter(BARRIO == barrio$BARRIO),
          color="white",fill=NA,
          alpha = .8,
          size=.8)+
  geom_sf(data = calles$osm_lines,
          inherit.aes = FALSE,
          color = "#f7b5cd",
          size = .4,
          alpha = .6) +
  geom_sf(data = calles_2$osm_lines,
          inherit.aes = FALSE,
          color = "#f7b5cd",
          size = .2,
          alpha = .6) +
  geom_sf(data = trenes$osm_lines,
          inherit.aes = FALSE,
          color = "#F17173",
          size = .2,
          alpha = .6) +
  coord_sf(xlim = c(barrio$xmin, barrio$xmax), 
           ylim = c(barrio$ymin, barrio$ymax),
           expand = FALSE) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(color="white",hjust = .5),
        plot.background = element_rect(fill = "#282828"))+
  labs(title=glue::glue("{barrio$BARRIO}"))


ggsave(glue::glue("{barrio$BARRIO}.png"), width=6,height = 6)       
