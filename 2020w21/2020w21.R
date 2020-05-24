if(!require(ggstream)) remotes::install_github("davidsjoberg/ggstream")
pacman::p_load(dplyr, lubridate, janitor, tidyverse, hablar, ggstream)
vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

top5 <- vb_matches %>%
  group_by(w_p1_country) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(-n) %>%
  slice(1:4) %>%
  pull(w_p1_country)

df <- vb_matches %>%
  transmute(country = if_else(w_p1_country %in% top5,
                              w_p1_country,
                           "Other"),
            year) %>%
  group_by(country, year) %>%
  summarise(n = n()) %>%
  ungroup()

df %>%
  ggplot(aes(year, n, fill = country)) +
  geom_stream(alpha = .9, color = "black", size = .1, bw = .70) +
  scale_fill_viridis_d() +
  cowplot::theme_minimal_vgrid() +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 25),
        plot.margin = margin(1, 2, 1, 2, "cm")) +
  labs(fill = NULL,
       title = "Volleyball",
       x = NULL,
       subtitle = "Wins of player one per country over time") +
  NULL
