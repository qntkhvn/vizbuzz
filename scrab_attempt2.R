library(tidyverse)
library(ggthemes)
library(ggforce)

csw <- " CH\nEA EE\nFY GU IO JA\nKO KY NY OB\nOO OU PO ST\nTE UG UR\nYU\nZO"
all <- "AA AB AD AE AG AH
        AI AL AM AN AR AS AT AW
        AX AY BA BE BI BO BY DE DO ED
        EF EH EL EM EN ER ES ET EW EX FA FE
        GO HA HE HI HM HO ID IF IN IS IT
        JO KA KI LA LI LO MA ME MI MM
        MO MU MY NA NE NO NU OD OE
        OF OH OI OM ON OP OR OS
        OW OX OY PA PE PI QI RE
        SH SI SO TA TI TO UH UM
        UN UP US UT WE WO
        XI XU YA YE YO
        ZA"

circ <- tibble(x = c(0, 0.5, 0.25),
               y = c(0, 0, -0.5),
               r = 1,
               label = c("CSW", "WWF", "OWL"))

circ %>%
  ggplot() +
  geom_circle(aes(
    x0 = x,
    y0 = y,
    r = r,
    color = label,
    fill = label
  ), alpha = 0.8) +
  scale_color_manual(values = c("#c3e0ec", "#f5ccc5", "#f2e6c6")) +
  scale_fill_manual(values = c("#c3e0ec", "#f5ccc5", "#f2e6c6")) +
  annotate(
    geom = "text",
    x = -0.95,
    y = 0.75,
    size = 7,
    label = "CSW",
    fontface = "bold",
    color = "#434343"
  ) +
  annotate(
    geom = "text",
    x = 1.45,
    y = 0.75,
    size = 7,
    label = "WWF",
    fontface = "bold",
    color = "#434343"
  ) +
  annotate(
    geom = "text",
    x = 0.3,
    y = -1.65,
    size = 7,
    label = "OWL",
    fontface = "bold",
    color = "#434343"
  ) +
  annotate(
    geom = "text",
    x = 0.3,
    y = 0.7,
    size = 3,
    label = "DA DI GI",
    fontface = "bold",
    color = "#434343"
  ) +
  annotate(
    geom = "text",
    x = -0.75,
    y = 0.2,
    size = 3,
    vjust = 0.5,
    label = csw,
    fontface = "bold",
    color = "#434343"
  ) +
  annotate(
    geom = "text",
    x = 0.2,
    y = -0.25,
    size = 3,
    label = all,
    fontface = "bold",
    color = "#434343"
  ) +
  labs(title = "Two-Letter Scrabble Words, By Dictionary",
       subtitle = "Official Tournament and Club Word List (OWL), Collins Scrabble\nWords (CSW) and Words With Friends (WWF)",
       caption = "SOURCE: ZYZZYVA") +
  theme_fivethirtyeight() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 14),
    panel.background = element_rect(fill = "#f1f1f1"),
    plot.background = element_rect(fill = "#f1f1f1"),
    legend.position = "none",
    plot.caption = element_text(face = "bold")
  )

