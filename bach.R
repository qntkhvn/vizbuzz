library(tidyverse)
library(ggthemes)

bach <- read_csv("~/Downloads/show_data_long.csv")

bach

  

week <- factor(c("Week before final four",
          "2 weeks before final four",
          "3 weeks before final four",
          "4 weeks before final four",
          "5 weeks before final four",
          "6 weeks before final four"),
          levels = rev(c("Week before final four",
                     "2 weeks before final four",
                     "3 weeks before final four",
                     "4 weeks before final four",
                     "5 weeks before final four",
                     "6 weeks before final four")))



rate <- c("78.2%", 79.2, 89.7, 93.5, 92.6, 95.7)

white <- c(1.5, 1.7, 1.85, 1.95, 2.6, 3.1)
other <- c(2.1, 2.5, 3, 3.8, 4.8, 5)

avg <- c(3, 4, 5, 6, 7, 8)

tibble(week, rate, avg, white, other) %>% 
  ggplot(aes(avg, week)) +
  geom_col(alpha = 0.2, width = 0.4) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8),
                     labels = c("0", "+2", "+4", "+6", "+8"),
                     guide = guide_axis(position = "top")) +
  geom_point(aes(y = 6:1, x = white), color = "white", fill = "black", size = 7, inherit.aes = FALSE) +
  geom_point(aes(y = 6:1, x = other), color = "#fc5185", fill = "black", size = 7, inherit.aes = FALSE) +
  theme_fivethirtyeight() +
  annotate(geom = "text", y = 6:1, x = -2, label = rate, size = 5, vjust = 0.5) +
  annotate(geom = "text", y = 6:1, x = -5, label = week, size = 5, vjust = 0.5) +
  expand_limits(x = -6, y = 7) +
  annotate(geom = "text", y = 6.5, x = -5, label = "Week of show", fontface = "bold", size = 6) +
  annotate(geom = "text", y = 6.5, x = -2, label = "One-on-one\nsurvival rate", fontface = "bold", size = 6) +
  annotate("segment", y = c(5.5, 4.5, 3.5, 2.5, 1.5), yend = c(5.5, 4.5, 3.5, 2.5, 1.5), x = -6.5, xend = 8.5, color = "lightgray") +
  annotate("segment", y = 6.2, yend = 6.2, x = -6.5, xend = 8.5) +
  annotate("segment", y = 0.5, yend = 6.5, x = 0, xend = 0) +
  annotate("segment", y = 0.5, yend = 6.5, x = c(0, 2, 4, 6, 8), xend = c(0, 2, 4, 6, 8), color = "lightgray") +
  labs(title = "Early one-on-ones have greater benefits",
       subtitle = "Average number of weeks that contestants remain after a given\nweek, for those who went on a one-on-one that week vs. overall\n",
       caption = c("FiveThirtyEight", "BASED ON DATA FROM ABC, WIKIPEDIA AND BACHELOR NATION WIKIA")) +
  theme(plot.caption = element_text(hjust = c(0, 1)),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        panel.grid.major = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 14))













