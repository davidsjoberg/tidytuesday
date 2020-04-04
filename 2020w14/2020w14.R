if(!require(pacman)) install.packages("pacman")
library(ggbump)
pacman::p_load(lubridate, tidyverse, hablar, cowplot, wesanderson)
set_wd_to_script_path()

brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')

df <- brewing_materials %>% 
  filter(!str_detect(material_type, "Total")) %>% 
  mutate(month = ymd(str_glue("{year}-{str_pad(month, width = 2, pad = '0')}-01"))) %>% 
  mutate(sugar_dummy = str_detect(type, "Sugar and syrups")) %>% 
  group_by(sugar_dummy, month) %>% 
  summarise(pounds = sum_(month_current)) %>% 
  ungroup() %>% 
  group_by(month) %>% 
  mutate(share_of_sugar = pounds / sum_(pounds)) %>% 
  ungroup()

df <- df %>% 
  mutate(guess = case_when(
    month == "2016-01-01" ~ "Inauguration of\nDonald Trump",
    month == "2014-11-01" ~ "Berkeley votes yes\nto a sugar tax",
    month == "2015-08-01" ~ str_wrap("Coca Cola is found influencing sugar research", 30),
    T ~ NA_character_
  ),
  guess_y = case_when(
    month == "2016-01-01" ~ .20,
    month == "2014-11-01" ~ .19,
    month == "2015-08-01" ~ .24,
    T ~ NA_real_
  )
  ) %>% 
  filter(sugar_dummy == 1)

ggplot(df, aes(month, share_of_sugar))  +
  geom_line(size = 1.5) + 
  geom_segment(data = df %>% drop_na(guess), 
               aes(month, share_of_sugar, xend = month, yend = guess_y), 
               linetype = "dotted", 
               inherit.aes = F, size = .5) +
  geom_label(aes(y = guess_y, label = guess), 
             size = 2.8) +
  labs(x = NULL,
       y = "Monthly share\nof sweeteners",
       title = "What happened to sweet 16?",
       subtitle = "Why did the share of sugar and syrups in beer production fall from 15 % to 2% in the US?") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, .25),
                     breaks = seq(0, .15, .05)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal_hgrid(font_size = 16) +
  theme(plot.title = element_text(size = 30),
        axis.title.y = element_text(hjust = 0.2))

ggsave("sweet16.png", dpi = 800, height = 5, width = 10)
