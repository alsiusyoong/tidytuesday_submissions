# ---
#   title: "TidyTuesday 2020/29 - Astronaut Database by Stavnichuk & Corlett (2020)"
# author: "Cedric Scherer"
# date: "14th of July 2020"
# output:
#   html_document:
#   theme: paper
# highlight: kate
# editor_options:
#   chunk_output_type: console
# ---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, fig.showtext = T, fig.retina = 1)
```


```{r prep, message=FALSE}
## packages
library(tidyverse)
library(ggtext)
library(ragg)
library(pdftools)


```{r data}
df_astro <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')
```

```{r data-prep}
df_missions <-
  df_astro %>% 
  group_by(name) %>% 
  summarize(
    hours = sum(hours_mission),
    year = min(year_of_mission),
    max_year = max(year_of_mission)
  ) %>% 
  ungroup() %>% 
  mutate(year = -year) %>% 
  arrange(year) %>% 
  mutate(id = row_number())

glimpse(df_missions)

df_labs <-
  df_missions %>% 
  filter(year %in% c(-1961, -197:-201*10, -2019)) %>% 
  group_by(year) %>% 
  filter(id == min(id))

glimpse(df_labs)

df_text <-
  df_missions %>% 
  arrange(-hours) %>% 
  slice(1:11) %>% 
  mutate(
    first_name = str_remove(name, ".*, "),
    last_name = str_remove(name, "(?<=),.*"),
    name_short = glue::glue("{str_sub(first_name, 0, 1)}. {last_name}"),
    era = glue::glue("'{str_sub(abs(year), 3, 4)}-'{str_sub(max_year, 3, 4)}"),
    label = glue::glue("<b style='font-size:13pt;'>{name_short}</b>  <span style='font-size:10pt;'>({era})</span><br><i style='color:#646464;'>{format(floor(hours), big.mark = ',')} hours ~ {round(hours / 8760, 1)} years</i>"),
    vjust = c(-.1, -.1, -.1, -.1, -.1, 1.1, 1.05, -.15, -.15, 1.2, 1.1),
    hjust = c(.5, .5, .6, .4, .5, .5, .6, .6, .6, .5, .7)
  ) 

glimpse(df_text)

```

```{r plot, fig.width = 25, fig.height = 23.63}
p <-
  df_missions %>% 
  ggplot(aes(
    x = id, 
    y = hours + 5, 
    color = year, 
    fill = year
  )) +
  ## curves
  ggforce::geom_link(
    aes(
      x = id, 
      xend = id,
      y = 0,
      yend = hours + 5,
      color = year,
      color = after_scale(colorspace::desaturate(color, .3)),
      alpha = hours
    ),
    n = 300,
    size = .25
  ) +
  ## triangles
  geom_point(
    aes(y = 0), 
    shape = 17, 
    size = .3
  ) +
  ## points
  geom_point(
    aes(
      y = hours + 5, 
      size = hours
    )
  ) +
  ## tick marks years
  geom_text(
    data = df_labs,
    aes(y = 0, label = "|"),
    family = "Changa",
    fontface = "bold",
    size = 4,
    vjust = 1
  ) +
  ## labels years
  geom_richtext(
    data = df_labs,
    aes(y = 0, label = glue::glue("<br>{abs(year)}")),
    size = 5.5,
    family = "Oswald",
    fontface = "bold",
    fill = NA,
    label.color = NA,
    vjust = .85
  ) +
  ## title
  geom_textbox(
    data = tibble(
      id = 685, hours = 20600,
      label = "<b style='font-size:38pt;'>Travelling to Outer Space</b><br><br>Cumulative time in outer space for all 565 cosmonauts and astronauts who participated in space missions between April 23, 1961 an January 15, 2020, sorted by the year of their first mission.<br>"
    ),
    aes(
      x = id,
      y = hours,
      label = label
    ),
    inherit.aes = F,
    size = 7.3,
    family = "Oswald",
    color = "grey70",
    lineheight = 1.3,
    width = unit(6.2, "inch"),
    hjust = 0,
    vjust = 0,
    fill = NA,
    box.colour = NA
  ) +
  ## caption
  annotate(
    "text", 
    x = 555, y = 16600,
    label = "Visualization by C�dric Scherer  .  Data by Stavnichuk & Corlett 2020 (DOI: 10.17632/86tsnnbv2w.1)", 
    family = "Oswald",
    size = 5.6, 
    color = "grey50"
  ) +
  coord_polar(theta = "y", start = 4.71, clip = "off") +
  scale_x_continuous(limits = c(-250, NA), expand = c(0, 0)) +#limits = c(-2040, NA)) +
  scale_y_continuous(limits = c(0, 22000), expand = c(0, 0)) + #limits = c(0, 156091.8)
  scico::scale_color_scico(palette = "hawaii", guide = F, direction = -1) +
  scico::scale_fill_scico(palette = "hawaii", guide = F, direction = -1) +
  scale_size(range = c(.001, 3), guide = F) +
  scale_alpha(range = c(.33, 1), guide = F) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"),
    plot.margin = margin(-260, -260, -300, -200)
  )
p
ggsave(here::here("plots", "2020_29", "2020_29_Astronauts_clean.pdf"), 
       width = 25, height = 23.63, device = cairo_pdf)
pdf_convert(pdf = here::here("plots", "2020_29", "2020_29_Astronauts_clean.pdf"),
            format = "png", dpi = 500)
```

```{r plot-with-annotations, fig.width = 25, fig.height = 23.63}
## annotated version
p +
  ## labels astronauts
  geom_richtext(
    data = df_text,
    aes(
      label = label, 
      hjust = hjust, 
      vjust = vjust
    ),
    size = 3.2,
    family = "Oswald",
    lineheight = 1.2,
    fill = NA,
    label.color = NA
  ) +
  ## annotations
  geom_text(
    data = tibble(
      id = c(395, 240), hours = rep(21950, 2),
      label = c(
        "On January 28, 1986 G. Jarvis,\nS.C.C. McAuliffe & M.J. Smith\ndied during the Challenger\ndisaster when the Space Shuttle\nbroke up during launch.",
        "1990 to 2000 was the decade with\nthe most cosmo-/astronauts\nparticipating in their first\nspace mission ever."
      )
    ),
    aes(
      x = id, y = hours, 
      label = label
    ),
    inherit.aes = F,
    family = "Oswald",
    size = 2.7,
    color = "grey50",
    lineheight = .9,
    vjust = 1
  )
ggsave(here::here("plots", "2020_29", "2020_29_Astronauts.pdf"), 
       width = 25, height = 23.63, device = cairo_pdf)
pdf_convert(pdf = here::here("plots", "2020_29", "2020_29_Astronauts.pdf"),
            format = "png", dpi = 500)
```

***
  
  ```{r session}
Sys.time()
git2r::repository()
sessionInfo()