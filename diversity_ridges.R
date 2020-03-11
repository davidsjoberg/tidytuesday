pacman::p_load(rnaturalearth, 
               rnaturalearthdata, 
               sf, 
               data.table,
               janitor, 
               ggmap,
               ggridges,
               lubridate, 
               feather, 
               hablar, 
               tidyverse)
hablar::set_wd_to_script_path()
options("scipen" = 100, "digits" = 4, stringsAsFactors = F) 


# Import -----------------------------------------------------------------------
diversity_school <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv')


# USA grid -------------
us <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  filter(sovereignt == "United States of America") %>% 
  st_crop(xmin=-126, xmax=-63, ymin = 23, ymax= 50)

us_grid <- st_make_grid(us, cellsize = .5, what = "centers")
us_grid <- sf::st_intersection(us, st_geometry(us_grid))


# Universities gelocation -------------
dv <- diversity_school %>%
  distinct(name) %>%
  mutate(name = paste(name, ", United states", sep = ""))

geo <- ggmap::geocode(dv$name) # You need you own Google API key to do this

df <- tibble(
  name = dv$name,
  lat = geo$lat,
  lon = geo$lon
) %>% 
  drop_na()


# Force uni geolocation to grid points
y_lines <- as(us_grid, "Spatial")@coords %>% as_tibble() %>% pull(2) %>% unique()
x_lines <- as(us_grid, "Spatial")@coords %>% as_tibble() %>% pull(1) %>% unique()

sf <- st_as_sf(df, coords = c("lon", "lat"), 
               crs = 4326, agr = "constant")
sf <- sf::st_intersection(sf, st_geometry(us))
y_uni <- as(sf, "Spatial")@coords %>% as_tibble() %>% pull(2)
y_new <- map_dbl(y_uni, 
    ~y_lines[which(abs(y_lines-.x) == min(abs(y_lines-.x)))])

x_uni <- as(sf, "Spatial")@coords %>% as_tibble() %>% pull(1)
x_new <- map_dbl(x_uni, 
                 ~x_lines[which(abs(x_lines-.x) == min(abs(x_lines-.x)))])

geo_uni <- tibble(
  name = sf$name,
  lon = x_new,
  lat = y_new
)

# Remove grids that have observations clos
grid <- as(us_grid, "Spatial")@coords %>% as_tibble() %>% 
  set_names("lon", "lat") %>% 
  mutate(name = NA_character_)

# combine grid and data observations
fin_sf <- grid %>% 
  bind_rows(geo_uni)


# Combine grid and geolocation of universities ------------
data <- diversity_school %>% 
  drop_na(name) %>% 
  filter(category == "Women") %>% 
  mutate(share_woman = enrollment / total_enrollment,
         name_long = paste(name, ", United states", sep = ""),
         name_uni = name) %>% 
  dplyr::select(name_uni, name_long, total_enrollment, share_woman) %>% 
  right_join(fin_sf, by = c("name_long" = "name")) %>% 
  dplyr::select(-name_long) %>% 
  drop_na(lat, lon)

data <- data %>% 
  group_by(lat, lon) %>% 
  summarise(enrollment = sum_(total_enrollment),
            share_woman = weighted.mean(share_woman, w = total_enrollment, na.rm = T)) %>% 
  mutate(enrollment = if_na(enrollment, 0),
         share_woman = if_else(is.nan(share_woman), NA_real_, share_woman)) %>% 
  ungroup()

data <- data %>% 
  group_by(lat) %>%
  arrange(lon) %>% 
  mutate(lon_lag = lon - lag(lon),
         lon_new_group = if_else(lon_lag > .5, 1, 0, 0),
         group = paste(lat, " - ", cumsum(lon_new_group))) %>% 
  ungroup()

# To make the whole ridge colored the share of woman needs to be
# extrapolated to proceeding or preceding longotude value
data <- data %>% 
  group_by(group) %>% 
  mutate(
    share_woman = if_else(
      !is.na(lag(share_woman)) & is.na(share_woman),
      lag(share_woman),
      share_woman,
      share_woman
    ),
    share_woman = if_else(
      !is.na(lead(share_woman)) & is.na(share_woman),
      lead(share_woman),
      share_woman,
      share_woman
    )) %>% 
  ungroup()


# PLOT ------

# Some ouliers are cut off
data <- data %>% 
  mutate(share_woman = if_else(share_woman > 0.75, .75, share_woman),
         share_woman = if_else(share_woman < 0.42, .42, share_woman))

# ridgeplot
ggplot() +
  geom_sf(data = us, color = "transparent", fill = "gray40") +
  geom_ridgeline_gradient(data = data,
                 aes(lon, lat, height = sqrt(enrollment/2e4),
                     group = group,
                     fill = share_woman),
                 color = "black",
                 size = .2) +
  scale_fill_gradient2(midpoint = mean_share_women,
                       low = "darkred",
                       mid = "steelblue" %>% colorspace::darken(.1),
                       high = "steelblue" %>% colorspace::darken(.7), labels = scales::percent_format(accuracy = 1)) +
  coord_sf() +
  theme_void() +
  labs(fill = "Percent women\nof enrollment",
       title = "Where are all the men in higher education?",
       subtitle = "Higher ridge indicate number of students. Blue indicate more woman than men.",
       caption = "Source: Google Maps API, Tuitiontracker.org") +
  theme(panel.background = element_rect(fill = "gray20", color = "transparent"),
        plot.background = element_rect(fill = "gray20", color = "transparent"),
        text = element_text(color = "white", face = "italic", family = "serif"),
        plot.caption = element_text(size = 8),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 9),
        legend.position = "bottom")

ggsave("ridge.png", dpi = 800, height = 5, width = 11)
