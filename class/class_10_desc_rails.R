#---------------------------------------------------------------------
# This script is edited for teaching purposes from the original 
# replication archive of the following paper:
# 
# Pengl, Yannick I.; Müller-Crepon, Carl;
# Valli, Roberto; Cederman, Lars-Erik;
# Girardin, Luc, 2025,
# "The Train Wrecks of Modernization:
# Railway Construction and Separatist Mobilization in Europe"
# APSR: 2025
#
# Please see the replication archive for original code:
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/EVF0DN
#
# Edited for POLSCI797: Complex Data and Machine Learning for Political Scientists
# University of Michigan, Fall 2025
# Tyler Simko
#---------------------------------------------------------------------

### descriptives: railways
library(here)
library(sf)
library(tidyverse)
library(ggthemes)
library(mapgl)
library(patchwork)
sf_use_s2(FALSE)

### load segment data (from original authors)
rail.sf <- st_read("euro_trains/analysis_data/RShapes.geojson")
back.sf <- read_rds("euro_trains/analysis_data/spatial_extent.rds")

# plotting sf data
rail.sf %>% ggplot() + geom_sf()
back.sf %>% ggplot() + geom_sf()

#-----------------------------------------------------
# 1. Which modern country does each railway cross?
# "spatial merge"
# 
# Data not in original paper:
# Downloaded from: https://ec.europa.eu/eurostat/web/gisco/geodata/administrative-units/countries
# I used low resolution data so it runs faster in class
eur <- st_read("CNTR_RG_60M_2024_3035.shp/CNTR_RG_60M_2024_3035.shp")
eur <- eur %>% filter(EU_STAT == "T")

eur %>% ggplot() + geom_sf()
#---------------------------------
# First, demonstrate geometric operations with simple example features
b0 = st_polygon(list(rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))))
b1 = b0 + 2
b2 = b0 + c(-0.2, 2)
x = st_sfc(b0, b1, b2)
a0 = b0 * 0.8
a1 = a0 * 0.5 + c(2, 0.7)
a2 = a0 + 1
a3 = b0 * 0.5 + c(2, -0.5)
y = st_sfc(a0,a1,a2,a3)

# Good cheatsheet
# https://github.com/rstudio/cheatsheets/blob/main/sf.pdf
plot(x)
plot(y, add = TRUE)
plot(st_intersection(st_union(x),st_union(y)), add = TRUE, col = 'red')

plot(st_buffer(y, 0.2), col = "blue")
plot(y, col = "green", add = TRUE)
# Other geometric manipulation options
# https://r.geocompx.org/geometry-operations
# https://geobgu.xyz/r/geometric-operations-with-vector-layers.html
#---------------------------------
# Now, do operations on the real data

# 1. Step when comparing any two spatial datasets
# Make sure same projection!
st_crs(rail.sf)
st_crs(eur)
st_crs(rail.sf) == st_crs(eur)

eur %>% ggplot() + geom_sf()
rail.sf %>% ggplot() + geom_sf()

p <- eur %>% ggplot() + geom_sf() + 
  geom_sf(data = rail.sf)
ggsave("test_overlap.pdf", p, width = 25, height = 25)

eur <- st_transform(eur, crs = st_crs(rail.sf))

eur %>% ggplot() + geom_sf()

# how make sure they're the same
st_crs(rail.sf) == st_crs(eur)

# you need an API key for this
mapboxgl(bounds = eur) %>% 
  add_line_layer(id = "rails",
                 source = rail.sf,
                 line_color = "red")

# 2. Spatial merge: which modern country does each rail line overlap?
st_intersects(rail.sf, eur)

intersections <- st_intersects(rail.sf, eur)

p <- rail.sf[c(5,51),] %>% ggplot() + 
  geom_sf(data = eur) + 
  geom_sf(color = "red") + 
  ggthemes::theme_map()
ggsave("test_rail.pdf", width = 25, height = 25)

intersections[[5]]
eur[26,]
intersections[[51]]

# return a matrix instead of a list
st_intersects(rail.sf, eur, sparse = FALSE)

# which European countries touch?
w_df <- st_intersects(eur, eur, sparse = FALSE) %>% 
  as_tibble()

colnames(w_df) <- eur$NAME_ENGL
w_df$country <- eur$NAME_ENGL

w_df %>% 
  pivot_longer(cols = Austria:Romania) %>% 
  ggplot(aes(x = country, y = name, fill = value)) + 
  geom_tile() +
  scale_fill_manual(values = c("#00274C", 
                               "#FFCB05"
  )) +
  theme(axis.text.x = element_text(angle = 45))

#-----------------------------------------------------
# Spatial "aggregation"
# Combine railway lines per year
rail.sf <- rail.sf %>% 
  group_by(year) %>% 
  summarise() %>% 
  ungroup()

ggplot(rail.sf[51,]) + geom_sf()
#-----------------------------------------------------
# Clip segments to background shape
ggplot(back.sf) + geom_sf() + 
  geom_sf(data = rail.sf)

rail.sf <- rail.sf %>% 
  st_intersection(
    back.sf
  )

ggplot(back.sf) + geom_sf() + 
  geom_sf(data = rail.sf)
#-----------------------------------------------------
# Create decades variable
round_down <- function(x) x %/% 10 * 10
rail.sf <- rail.sf %>% 
  mutate(decade = paste0(round_down(year), "s"))

# Plot from plot
ggplot() +
  geom_sf(data = back.sf, fill = "gray80", linewidth = 0.1) +
  geom_sf(data = rail.sf, 
          aes(col = decade), linewidth = 0.09) +
  scale_color_viridis_d(option = "B",
                        na.value = "transparent", begin = 0, end = 0.91,
                        guide = guide_legend(override.aes = list(linewidth = 3))
  ) +
  labs(col = "Year of line construction") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        legend.box.margin = margin(-15, 0, -2, 0),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(here("figures", "figure_2.pdf"),
       width = 6, height = 5)


# Clean up ----
rm(back.sf,  rail.sf)
