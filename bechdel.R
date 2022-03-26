library(tidyverse)

movies <- read_csv("~/Downloads/movies.csv")



movies %>% 
  mutate(era = cut(year, 
                   breaks = seq(1970, 2015, 5),
                   include.lowest = TRUE,
                   right = FALSE),
         clean_test = factor(clean_test,
                             levels = c("nowomen", "notalk", "men", "dubious", "ok"))) %>% 
  count(era, clean_test) %>% 
  group_by(era) %>% 
  mutate(pct = n / sum(n)) %>% 
  ggplot(aes(era, pct, fill = clean_test)) +
  geom_col(color = "white", width = 1) +
  scale_x_discrete(labels = c("1970-\n'74",
                              "",
                              "1980-\n'84",
                              "",
                              "1990-\n'94",
                              "",
                              "2000-\n'04",
                              "",
                              "2010-\n'13")) + 
  scale_y_continuous(labels = c("0", "25", "50", "75", "100%")) + 
  scale_fill_manual(values = rev(c("#008fd5", "#6bb2d5", "#ffc9bf", "#ff9380", "#ff2700"))) +
  expand_limits(x = 11.5) +
  annotate(geom = "text", x = 9.75, y = 0.96, hjust = 0, size = 5, label = "Fewer than\ntwo women") +
  annotate(geom = "text", x = 9.75, y = 0.8, hjust = 0, size = 5, label = "Women don't\ntalk to each\nother") +
  annotate(geom = "text", x = 9.75, y = 0.6, hjust = 0, size = 5, label = "Women only\ntalk about men") +
  annotate(geom = "text", x = 9.75, y = 0.5, hjust = 0, size = 5, label = "Dubious") +
  annotate(geom = "text", x = 9.75, y = 0.25, hjust = 0, size = 5, label = "Passes\nBechdel\nTest") +
  annotate(geom = "segment", x = 9.5, xend = 9.7, y = 0.96, yend = 0.96, size = 1) +
  annotate(geom = "segment", x = 9.5, xend = 9.7, y = 0.8, yend = 0.8, size = 1) +
  annotate(geom = "segment", x = 9.5, xend = 9.7, y = 0.6, yend = 0.6, size = 1) +
  annotate(geom = "segment", x = 9.5, xend = 9.7, y = 0.5, yend = 0.5, size = 1) +
  annotate(geom = "segment", x = 9.5, xend = 9.7, y = 0.25, yend = 0.25, size = 1) +
  annotate(geom = "text", x = 5, y = 0.2, size = 19, label = "PASS", fontface = "bold") +
  annotate(geom = "text", x = 5, y = 0.75, size = 19, label = "FAIL", fontface = "bold") +
  annotate(geom = "segment", x = 0.1, xend = 10, y = 0, yend = 0, size = 0.8) +
  annotate(geom = "segment", x = 0.1, xend = 0.5, y = 1, yend = 1, color = "lightgray") +
  annotate(geom = "segment", x = 0.1, xend = 0.5, y = 0.75, yend = 0.75, color = "lightgray") +
  annotate(geom = "segment", x = 0.1, xend = 0.5, y = 0.5, yend = 0.5, color = "lightgray") +
  annotate(geom = "segment", x = 0.1, xend = 0.5, y = 0.25, yend = 0.25, color = "lightgray") +
  annotate(geom = "segment", x = 0.1, xend = 0.5, y = 1, yend = 1, color = "lightgray") +
  #annotate(geom = "rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = -0.2) +
  geom_step(data = border, aes(era - 0.5, total, group = 1), inherit.aes = FALSE, size = 1.5) +
  ggthemes::theme_fivethirtyeight() +
  labs(title = "The Bechdel Test Over Time",
       subtitle = "How women are represented in movies",
       caption = "\nSOURCE: BECHDELTEST.COM") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "none",
        plot.background = element_rect(color = "white"),
        plot.title = element_text(size = 21),
        plot.subtitle = element_text(size = 16),
        axis.text = element_text(size = 16, hjust = 0.5, face = "bold"),
        axis.text.y.left = element_text(hjust = 1),
        plot.tag.position =  "bottomleft",
        plot.caption = element_text(face = "bold", size = 10),
        plot.title.position = "plot")
  

border <- movies %>% 
  mutate(era = cut(year, 
                   breaks = seq(1970, 2015, 5),
                   include.lowest = TRUE,
                   right = FALSE),
         clean_test = factor(clean_test,
                             levels = c("nowomen", "notalk", "men", "dubious", "ok"))) %>% 
  count(era, clean_test) %>% 
  group_by(era) %>% 
  mutate(pct = n / sum(n)) %>% 
  filter(clean_test %in% c("dubious", "ok")) %>% 
  group_by(era) %>% 
  summarize(total = sum(pct)) %>% 
  mutate(era = as.numeric(era)) %>% 
  bind_rows(tibble(era = 10, total = 0.548))

border

ggplot() +
  geom_step(data = border, aes(era, total, group = 1))
