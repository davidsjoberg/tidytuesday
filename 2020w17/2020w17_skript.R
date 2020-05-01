pacman::p_load(BBmisc, tidyverse, hablar, ggbump, sf, rnaturalearth, feather, janitor, lubridate)
options(stringsAsFactors = F)
set_wd_to_script_path()

gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')

df <- gdpr_violations %>% 
  group_by(name) %>% 
  summarise(price = sum_(price)) %>% 
  ungroup()

sdf <- rnaturalearthdata::countries50 %>% 
  st_as_sf() %>% 
  st_crop(xmin = -24, xmax = 31, ymin = 33, ymax = 73) %>% 
  filter(admin %in% df$name) %>% 
  left_join(df, by = c("admin" = "name")) %>% 
  mutate(price_cap  = price / pop_est,
         admin = case_when(admin == "United Kingdom" ~ "UK",
                           admin == "Czech Republic" ~ "Czech",
                           T ~ admin))

ranking <- st_geometry(sdf) %>% 
  st_point_on_surface() %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  bind_cols(tibble(fine_cap = normalize(rank(sdf$price_cap), range = c(40.12161, 66.12161), method = "range"),
                   country = sdf$admin,
                   xend = 60,
                   x_axis_start = xend + 10,
                   fine_cap_x = normalize(sdf$price_cap, range = c(first(x_axis_start), 100), method = "range"),
                   val_txt = paste0(format(sdf$price_cap, digits = 0, nsmall = 2)),
                   val_txt2 = if_else(country == "Austria", paste0(val_txt, "€ per capita"), val_txt)))

sdf <- sdf %>% 
  bind_cols(ranking %>% select(fine_cap))

ggplot() + 
  geom_sf(data = sdf, size = .3, fill = "transparent", color = "gray17") +
  # Sigmoid from country to start of barchart
  geom_sigmoid(data = ranking, 
               aes(x = X, y = Y, xend = x_axis_start - .2, yend = fine_cap, group = country, color = fine_cap), 
               alpha = .6, smooth = 10, size = 1) + 
  # Line from xstart to value
  geom_segment(data = ranking, 
               aes(x = x_axis_start, y = fine_cap, xend = fine_cap_x, yend = fine_cap, color = fine_cap), alpha = .6, size = 1, 
               lineend = "round") + 
  # Y axis - black line
  geom_segment(data = ranking, 
               aes(x = x_axis_start, y = 40, xend = x_axis_start, yend = 67), alpha = .6, size = 1.3, color = "black") +
  # dot on centroid of country in map
  geom_point(data = ranking, 
             aes(x = X, y = Y, color = fine_cap), size = 2) +
  # Country text
  geom_text(data = ranking, aes(x = x_axis_start-.5, y = fine_cap, label = country, color = fine_cap), hjust = 1, size = 2.5, nudge_y = .5) +
  # Value text
  geom_text(data = ranking, aes(x = fine_cap_x, y = fine_cap, label = val_txt2, color = fine_cap), hjust = 0, size = 2, nudge_x = .4) +
  coord_sf(clip = "off") +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  theme_void() +
  labs(title = "GDPR fines per capita",
       subtitle = str_wrap("The General Data Protection Regulation (EU) 2016/679 (GDPR) is a regulation in EU law on data protection and privacy in the European Union (EU) and the European Economic Area (EEA).", 100),
       caption = "Source: TidyTuesday & Wikipedia") + 
  theme(plot.margin = margin(.5, 1, .5, .5, "cm"),
        legend.position = "none",
        plot.background = element_rect(fill = "black"),
        plot.caption = element_text(color = "gray40"),
        plot.title = element_text(color = "gray40", size = 16, family = "Helvetica", face = "bold"),
        plot.subtitle = element_text(color = "gray40", size = 8))

ggsave("ranking_gdpr.png", dpi = 1000)
