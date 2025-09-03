## graficar el network espacialmente
## siguiendo el siguiente site: http://kateto.net/network-visualization
## https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library('geosphere')
library(igraph)
library(sf)#for using spatial objects 
library(tidyverse)#for using tidy syntax
library(tmap)#for visualizing maps
library(spData)
library(geobr)
library(ggrepel)
library(stplanr) #for using od2line function to convert points to lines


##### Read data
nodes.mpa <- read.csv("Nodos_MPAs.csv", header=T, as.is=T)
edges.mpa.lo <- read.csv("EDGES_MPA_revLOrena.csv", header=T, as.is=T)

##### Build graph from dataframe
net_mpa <- graph_from_data_frame( d = edges.mpa.lo, vertices = nodes.mpa, directed = FALSE )
summary(net_mpa)


# Download coastline
coast <- ne_download(scale = 10, type = "coastline", category = "physical", returnclass = "sf")

# Download country boundaries
countries <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin %in% c("Brazil", "Uruguay", "Argentina"))

# Crop global coastline to bounding box of selected countries
bbox_south_atlantic <- st_bbox(countries) %>% st_as_sfc()
coast_cropped <- st_intersection(coast, bbox_south_atlantic)

## construyo el geompoint para cada MPA
MPASpatial = nodes.mpa %>% 
  st_as_sf(coords=c("Longitud", "Latitude"), crs = "EPSG: 4326") 
head(MPASpatial, 3)

# create line geometry 
MPAEdges_toLine = od2line(edges.mpa.lo, MPASpatial)
MPAEdges_toLine[c(1:3),]


## Mpa Edges
#Construct a network and calculate the degree for each node. 
MPASpatial$degree = degree(
  net_mpa,
  v = V(net_mpa),
  mode = c("all"),
  loops = FALSE,
  normalized = TRUE
)

# create a line weight column based on edge distance
MPAEdges_toLine = MPAEdges_toLine %>% 
  mutate(weight = as.numeric(st_length(geometry)))

# Mapa completo
p_full <- ggplot() +
  geom_sf(data = coast_cropped, color = "gray60") +
  geom_sf(data = countries, fill = "gray95", color = "gray70") +
  geom_sf(data = MPAEdges_toLine, color = "black", alpha = 0.5) +
  geom_sf(data = MPASpatial, aes(fill = Country), shape = 21, size = 2, stroke = 0.2) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_manual(values = c("Brazil" = "green", "Uruguay" = "deepskyblue", "Argentina" = "orange")) +
  theme_minimal() +
  labs(title = "MPA Network - All Species", color = "Specie", fill = "Country")

# Guardar
ggsave("outputs/map_full_network.png", p_full, width = 10, height = 8)

library(purrr)

# Obtener lista de especies únicas
species_list <- unique(MPAEdges_toLine$Specie)

# Función para graficar por especie
plot_by_species <- function(sp) {
  edges_sp <- MPAEdges_toLine %>% filter(Specie == sp)
  
  # Filtrar nodos conectados
  node_ids <- unique(c(edges_sp$from, edges_sp$to))
  nodes_sp <- MPASpatial %>% filter(ID %in% node_ids)
  
  # Crear plot
  p_sp <- ggplot() +
    geom_sf(data = coast_cropped, color = "gray60") +
    geom_sf(data = countries, fill = "gray95", color = "gray70") +
    geom_sf(data = edges_sp, color = "black", alpha = 0.6) +
    geom_sf(data = nodes_sp, aes(fill = Country), shape = 21, size = 2, stroke = 0.2) +
    scale_fill_manual(values = c("Brazil" = "green", "Uruguay" = "deepskyblue", "Argentina" = "orange")) +
    theme_minimal() +
    labs(title = paste("MPA Network -", sp), fill = "Country")
  
  # Guardar
  ggsave(paste0("outputs/map_", sp, ".png"), p_sp, width = 8, height = 6)
}

# Aplicar a cada especie
walk(species_list, plot_by_species)























