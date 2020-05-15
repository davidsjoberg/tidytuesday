# prep
pacman::p_load(sf, smoothr, raster, tidyverse, rnaturalearth, hablar)
options("scipen" = 100, stringsAsFactors = F)
hablar::set_wd_to_script_path()

# Download map and data
world_graticules <- ne_download(category = "physical", type = "graticules_30", returnclass = "sf")
world_cultural <- ne_download(10, returnclass = "sf")
volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv') %>% 
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = 4326)

# Fix polygons
polygon <- st_polygon(x = list(rbind(c(-0.0001, 90),
                                     c(0, 90),
                                     c(0, -90),
                                     c(-0.0001, -90),
                                     c(-0.0001, 90)))) %>%
  st_sfc() %>%
  st_set_crs(4326)

world2 <- world_cultural %>% st_difference(polygon)

# Box: Ring of fire
xmin <- 92
xmax <- 305
ymin <- -79
ymax <- 68

# Plot
ggplot() +
  geom_sf(data = st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = ymin), crs = st_crs(4326)) %>% 
            st_as_sfc() %>% smooth(refinements = 10), fill = "lightblue", color = "transparent", size = 1) +
  geom_sf(data = world2, fill = "gray80", color = "transparent") +
  geom_sf(data = world_graticules, color = "gray70") +
  geom_sf(data = world2 %>% 
            st_intersection(st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = ymin), crs = st_crs(4326)) %>% 
                                               st_as_sfc() %>% smooth(refinements = 10)), 
          fill = "forestgreen", color = "transparent") +
  geom_sf(data = world2 %>% 
            st_intersection(st_bbox(c(xmin = xmin-360, xmax = xmax-360, ymax = ymax, ymin = ymin), crs = st_crs(4326)) %>% 
                                               st_as_sfc() %>% smooth(refinements = 10)), 
          fill = "forestgreen", color = "transparent") +
  geom_sf(data = volcano %>%
            st_intersection(st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = ymin), crs = st_crs(4326)) %>% 
                                               st_as_sfc() %>% smooth(refinements = 10)), 
          fill = "red", color = "red", alpha = .3, size = .9, shape = 21) +
  geom_sf(data = volcano %>% 
            st_intersection(st_bbox(c(xmin = xmin-360, xmax = xmax-360, ymax = ymax, ymin = ymin), crs = st_crs(4326)) %>% 
                                               st_as_sfc() %>% smooth(refinements = 10)), 
          fill = "red", color = "red", alpha = .3, size = .9, shape = 21) +
  geom_sf(data = world_graticules %>% 
            st_intersection(st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = ymin), crs = st_crs(4326)) %>% 
                                               st_as_sfc() %>% smooth(refinements = 10)), 
          color = "gray40") +
  geom_sf(data = world_graticules %>%
            st_intersection(st_bbox(c(xmin = xmin-360, xmax = xmax-360, ymax = ymax, ymin = ymin), crs = st_crs(4326)) %>% 
                                               st_as_sfc() %>% smooth(refinements = 10)), 
          color = "gray35") +
  geom_sf(data = st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = ymin), crs = st_crs(4326)) %>%
            st_as_sfc() %>% smooth(refinements = 10), fill = "transparent", color = "black", size = .05) +
  coord_sf(crs = '+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs') +
  theme_void() +
  labs(title = "The ring of fire",
       caption = "Source: TidyTuesday | Graphics: David Sjoberg",
       subtitle = "Also named the Circum-Pacific Belt. Most volcanoes in the world are located in the ring.") +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold", color = "gray40"),
        plot.subtitle = element_text(hjust = .5, size = 10, color = "gray50"))

#Export
ggsave("ring_of_fire.png", dpi = 1500)

