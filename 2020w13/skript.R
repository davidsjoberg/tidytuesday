pacman::p_load(hablar, tidyverse)
set_wd_to_script_path()

tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')

# Data cleaning  
df <- tbi_age %>% 
  select(age_group, injury_mechanism, number_est) %>% 
  filter(!age_group %in% c("0-17", "Total")) %>% 
  mutate(ages = case_when( 
    age_group == "0-4" ~ "-24",
    age_group == "5-14" ~ "-24",
    age_group == "15-24" ~ "-24",
    age_group == "25-34" ~ "25-65",
    age_group == "35-44" ~ "25-65",
    age_group == "45-54" ~ "25-65",
    age_group == "55-64" ~ "25-65",
    age_group == "65-74" ~ "65+",
    age_group == "75+" ~ "65+"
  )) %>% 
  arrange(ages) %>% 
  group_by(ages, injury_mechanism) %>% 
  summarise(number_est = sum_(number_est)) %>% 
  ungroup()

df_agg <- df %>% 
  group_by(ages) %>% 
  summarise(tot_age = sum_(number_est)) %>% 
  ungroup() %>% 
  mutate(csum = cumsum(tot_age),
         y_start = lag(csum),
         y_start = if_na(y_start, 0),
         total = max(csum)) %>% 
  select(ages, y_start, tot_age, total)

df <- df %>% 
  left_join(df_agg, by = "ages")

df <- df %>% 
  mutate(ages_x = paste(ages, injury_mechanism),
         row_n = row_number())

# Extra df's for plots
lines <- df_agg %>% 
  select(tot_age) %>% 
  bind_rows(tibble(tot_age = 0), .) %>% 
  mutate(x = c(0, 7, 14, 21),
         ack = cumsum(tot_age))

lines <- lines %>% 
  bind_rows(tibble(x = c(-1, 22),
                   ack = c(0, max(lines$ack))))

x_text <- lines %>% 
  drop_na(tot_age) %>% 
  mutate(x = x + (lead(x) - x)/2) %>% 
  drop_na(x) %>% 
  mutate(k = c("-24", "25-65", "65+"))

step_height <- lines %>% 
  slice(2:4) %>% 
  mutate(y = lag(ack),
         y = if_na(y, 0))

# Plot
df %>% 
  ggplot(aes(row_n, number_est, fill = injury_mechanism)) +
  geom_rect(aes(xmin = row_n - 1, xmax = row_n, ymin = y_start, ymax = y_start + number_est)) +
  geom_step(data = lines, aes(x, ack), inherit.aes = F,
            size = 1) +
  geom_text(data = tibble(x = 22, y = max(lines$ack)), aes(x, y, label = paste(max(lines$ack) %>% format(big.mark = " "))),
            inherit.aes = F,
            hjust = 0,
            nudge_x = .3,
            size = 5) +
  geom_text(data = tibble(x = 22, y = max(lines$ack)), aes(x, y, label = paste("Total Traumatic Brain Injury")),
            inherit.aes = F,
            hjust = 0,
            nudge_y = -1.5e5,
            nudge_x = .3,
            size = 3) +
  geom_text(data = tibble(x = -1, y = 0), aes(x, y, label = paste(0)),
            inherit.aes = F,
            hjust = 0,
            nudge_x = -1,
            size = 5) +
  geom_text(data = x_text, aes(x, ack, label = k), 
            inherit.aes = F,
            size = 6,
            nudge_y = -1e5) +
  geom_text(data = step_height, aes(x, y, label = format(tot_age, big.mark = " ") %>% str_squish()), 
            inherit.aes = F,
            size = 3,
            hjust = 0, 
            nudge_x = .3,
            nudge_y = ,
            angle = 90,
            color = "grey50") +
  theme_void() +
  coord_cartesian(clip = "off") +
  labs(title = "Traumatic Brain Injury",
       subtitle = "Number of cases split upon age group and injury mechanism",
       fill = NULL, 
       caption = "Source: CDC and Veterans Brain Injury Center") +
  scale_fill_manual(values = c(
    wesanderson::wes_palette("GrandBudapest1", 4),
    wesanderson::wes_palette("GrandBudapest2", 4)
  )) +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(size = 20, face = "bold"))

ggsave("TBI.png", dpi = 1200)
