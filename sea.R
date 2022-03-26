
library(magick)

image_read("~/Downloads/lucds.png") %>% 
  image_resize(geometry = geometry_area(width = 750))

image_read("~/Downloads/sea.png") %>% 
  image_raster() %>% 
  count(col, sort = TRUE) %>% 
  head(5)
  


library(tidyverse)

sea <- read_csv("~/Downloads/sea.csv")

ss <- sea %>% 
  group_by(month) %>% 
  summarize(avg = mean(extent, na.rm = TRUE))


sea %>% 
  group_by(month, year) %>% 
  summarize(mn = mean(extent, na.rm = TRUE)) %>% 
  left_join(ss, by = "month") %>% 
  mutate(month = case_when(
    month == 1 ~ "Jan",
    month == 2 ~ "Feb",
    month == 3 ~ "Mar",
    month == 4 ~ "Apr",
    month == 5 ~ "May",
    month == 6 ~ "Jun",
    month == 7 ~ "Jul",
    month == 8 ~ "Aug",
    month == 9 ~ "Sep",
    month == 10 ~ "Oct",
    month == 11 ~ "Nov",
    month == 12 ~ "Dec"
  )) %>% 
  mutate(month = factor(month,
                        levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>% 
  ggplot(aes(year, mn)) +
  geom_line(size = 1.2, color = "#70aea9") +
  geom_hline(aes(yintercept = avg), color = "#6078b4") +
  facet_wrap(~ month, nrow = 1, strip.position = "bottom") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  #ggthemes::theme_fivethirtyeight() +
  labs(x = NULL,
       y = NULL,
       title = "Monthly sea ice average extent, northern hemisphere, 1979-2019",
       subtitle = "(millions of square kilometers)",
       caption = "Source: National Snow & Ice Data Center,\nhttps://nsidc.org/articseaicenews/sea-ice-tools/") +
  expand_limits(y = c(0, 20)) +
  theme(plot.title = element_text(hjust = 0.5, size = 19, color = "#59595a"),
        plot.subtitle = element_text(hjust = 0.5, size = 15, color = "#59595a"),
        plot.caption = element_text(hjust = 0, color = "#1c1c1c", face = "bold", size = 11),
        strip.placement = "bottom",
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(color = "#616162", size = 10, face = "bold"),
        panel.grid = element_line())

# is there a font picker???
