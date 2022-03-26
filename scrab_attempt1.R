library(magick)

image_read("~/Downloads/lucds.png") %>%
  image_resize(geometry = geometry_area(750, 750))


library(tidyverse)

scrab <- read_csv("~/Desktop/viz_buzz/scrab.csv")

scrab %>% 
  pivot_longer(!word,
               names_to = "dict")


install.packages("ggvenn")

library(ggvenn)

?ggvenn


d <- tibble(value = c(1, 2, 3, 5, 6, 7, 8, 9),
            `Set 1` = c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE),
            `Set 2` = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE),
            `Set 3` = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
            `Set 4` = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE))

d %>% 
  ggplot() +
  geom_venn(aes(A = `Set 1`, B = `Set 2`))


library(ggthemes)
theme_set(theme_fivethirtyeight())
library(ggrepel)

scrab %>% 
  mutate(OWL = (in_owl == 1),
         CSW = (in_csw == 1),
         WWF = (in_wwf == 1)) %>% 
  ggplot() +
  geom_venn(aes(C = OWL,
                A = CSW,
                B = WWF,
                label = word),
            show_percentage = FALSE) +
  labs(title = "Two-Letter Scrabble Words, By Dictionary",
       subtitle = "Official Tournament and Club Word List (OWL), Collins Scrabble\nWords (CSW) and Words With Friends (WWF)") +
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank())

# doesnt work

scrab %>% 
  mutate(OWL = (in_owl == 1),
         CSW = (in_csw == 1),
         WWF = (in_wwf == 1)) %>% 
  ggvenn(c("CSW", "WWF", "OWL"),
         show_percentage = FALSE,
         fill_alpha = 1.5,
         fill_color = c("#c3e0ec", "#f2e6c6", "#f5ccc5")) +
  labs(title = "Two-Letter Scrabble Words, By Dictionary",
       subtitle = "Official Tournament and Club Word List (OWL), Collins Scrabble\nWords (CSW) and Words With Friends (WWF)",
       caption = "SOURCE: ZYZZYVA") +
  theme_fivethirtyeight() +
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill = "#f1f1f1"),
        plot.background = element_rect(fill = "#f1f1f1"))

#Error in calc_scale_info_3(auto_scale, df_element$n) : 
 # Error: 'auto_scale' parameter is supported for only two set venn so far.

#f5ccc5
#f2e6c6
#c3e0ec


library(ggforce)
circ <- tibble(x = c(0, 0.5, 0.25),
               y = c(0, 0,-0.5),
               r = 1)

circ %>%
  ggplot() +
  geom_circle(aes(x0 = x, y0 = y, r = r))


