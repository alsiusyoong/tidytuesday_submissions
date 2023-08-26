#===================================================================================================================
# Author: Aloysius Ong
# Dataset: #tidytuesday - Astronauts
# Purpose: Visualization 
#===================================================================================================================

astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

library(plotly)
library(dplyr)
library(ggthemes)
library(hrbrthemes)
library(viridis)
library(ggcharts)
library(data.table)

#---- Visualization (Radial Barcharts) using Plotly ----
glimpse(astronauts)
summary(astronauts)

table(astronauts$year_of_mission)
table(astronauts$sex)
table(astronauts$occupation)

astronauts <- astronauts %>% dplyr::arrange(total_hrs_sum)
astronauts <- astronauts[-(1:50),]

astronauts
# Adding colors

# df <- read.csv("https://cdn.rawgit.com/plotly/datasets/master/Emissions%20Data.csv", stringsAsFactors = F)
# glimpse(df)
# table(df$Continent)

colors <- RColorBrewer::brewer.pal(length(unique(astronauts$nationality)), "Spectral")
nationality <- unique(astronauts$nationality)

astronauts$colors <- astronauts$nationality

for(i in 1:length(nationality)){
  idx <- astronauts$colors %in% nationality[i]   
  astronauts$colors[idx] <- colors[i]
}

# Get incremental angle value
n <- nrow(astronauts) + 20
dtheta <- 2*pi / n
theta <- pi / 2

# Initialise
x.coord <- c()
y.coord <- c()
cols <- c()

# White circle in the middle
adjust <-  80

# Initialise plot
p <- plot_ly()

p %>% layout(autosize = F, width = 500, height = 500)

for(ctr in 1:nrow(astronauts)){
  
  a <- astronauts$total_hrs_sum[ctr] + adjust
  
  x1 <- adjust * cos(theta)
  y1 <- adjust * sin(theta)
  
  x2 <- a * cos(theta)
  y2 <- a * sin(theta)
  
  x.coord <- c(x.coord, x1, x2, NA)
  y.coord <- c(y.coord, y1, y2, NA)
  cols <- c(cols, astronauts$nationality[ctr], astronauts$nationality[ctr], NA)
  
  theta <- theta + dtheta
  
  p <- add_trace(p, 
                 x = c(x1, x2),
                 y = c(y1, y2),
                 mode = "lines", 
                 line = list(width = 5, color = astronauts$colors[ctr]),
                 evaluate = T)
}

# Keep x and y axis extents the same
up <- max(na.omit(c(x.coord, y.coord))) + 10
down <- min(na.omit(c(y.coord, y.coord))) - 10

p <- layout(p,
            showlegend = F,
            xaxis = list(range = c(down, up), domain = c(0, 0.5),
                         title = "", showgrid = F, zeroline = F, showticklabels = F),
            yaxis = list(range = c(down, up), 
                         title = "", showgrid = F, zeroline = F, showticklabels = F),
            shapes = list(
              list(type = "circle",
                   x0 = (-5 - adjust),
                   y0 = (-5 - adjust),
                   x1 = (5 + adjust),
                   y1 = (5 + adjust),
                   fillcolor = "transparent",
                   line = list(color = "white", width = 2)),
              
              list(type = "circle",
                   x0 = (-15 - adjust),
                   y0 = (-15 - adjust),
                   x1 = (15 + adjust),
                   y1 = (15 + adjust),
                   fillcolor = "transparent",
                   line = list(color = "white", width = 2)),
              
              list(type = "circle",
                   x0 = (-25 - adjust),
                   y0 = (-25 - adjust),
                   x1 = (25 + adjust),
                   y1 = (25 + adjust),
                   fillcolor = "transparent",
                   line = list(color = "white", width = 2)),
              
              list(type = "circle",
                   x0 = (-35 - adjust),
                   y0 = (-35 - adjust),
                   x1 = (35 + adjust),
                   y1 = (35 + adjust),
                   fillcolor = "transparent",
                   line = list(color = "white", width = 2))))

# Add annotations for country names
p <- plotly_build(p)

theta <- pi / 2
textangle <- 90

for(ctr in 1:nrow(astronauts)){
  
  a <- astronauts$total_hrs_sum[ctr] + adjust
  a <- a + a/12
  
  x <- a * cos(theta)
  y <- a * sin(theta)
  
  if(ctr < 51) {xanchor <- "right"; yanchor <- "bottom"}
  if(ctr > 51 & ctr < 84) {xanchor <- "right"; yanchor <- "top"}
  if(ctr > 84) {xanchor <- "left"; yanchor <- "top"}
  
  p$layout$annotations[[ctr]] <- list(x = x, y = y, showarrow = F,
                                      text = paste0(astronauts$name[ctr]),
                                      textangle = textangle,
                                      xanchor = xanchor,
                                      yanchor = yanchor,
                                      font = list(family = "serif", size = 9),
                                      borderpad = 0,
                                      borderwidth = 0)
  theta <- theta + dtheta
  textangle <- textangle - (180 / pi * dtheta)
  
  if(textangle < -90) textangle <- 90
}

# Titles and some other details
p$layout$annotations[[148]] <- list(xref = "paper", yref = "paper",
                                    x = 0, y = 1, showarrow = F,
                                    xanxhor = "left", yanchor = "top",
                                    align = "left",
                                    text = "Total number of hours on space mission (hrs)",
                                    font = list(size = 2, color = "black"))

p$layout$annotations[[149]] <- list(xref = "paper", yref = "paper",
                                    x = 0, y = 0.9, showarrow = F,
                                    xanxhor = "left", yanchor = "top",
                                    align = "left",
                                    text = "Sum of mission hours undertaken in career.",
                                    font = list(size = 2, color = "#808080"))

p$layout$annotations[[150]] <- list(xref = "paper", yref = "paper",
                                    x = 0.15, y = 0.5, showarrow = F,
                                    xanxhor = "left", yanchor = "top",
                                    align = "left",
                                    text = "Cumulative number of mission hours per astronaut.",
                                    font = list(size = 2, color = "black"))

p$data[[149]] <- list(x = rep(-7, 6), y = c(-6, -4, -2, 0, 2, 4), mode = "markers",
                      marker = list(color = colors, size = 10))

p$data[[150]] <- list(x = rep(1, 6), y = c(-6, -4, -2, 0, 2, 4), mode = "text",
                      text = rev(nationality),
                      marker = list(color = colors, size = 10))
p



#---- Radial Bar Charts 2nd Attempt ----
library(tidyverse)
library(hrbrthemes)
library(ggplot2)

table(astronauts$sex)


df <- astronauts %>%
  group_by(name) %>%
  filter(sex == 'female') %>%
  distinct(total_hrs_sum) %>%
  arrange(total_hrs_sum) %>%
  mutate(name=factor(name, name)) 

# Create a new theme
theme_blackwhite <- function (base_size = 12, base_family = "sans") {
  theme_bw() %+replace% 
    theme(
      panel.grid.major  = element_blank(),
      panel.background = element_rect(fill = "black"),
      panel.border = element_rect(color = "#0d1b2a", fill = NA),
      axis.line = element_line(color = "#e0e1dd"),
      axis.ticks = element_line(color = "#e0e1dd"),
      axis.text = element_text(color = "#e0e1dd")
    )
}



sum(df$total_hrs_sum)

myplot <- df %>%
  ggplot(aes(reorder(x=name, total_hrs_sum, sum), total_hrs_sum)) +
  geom_bar(fill="#ffba08", stat="identity", width = 0.4) +
  geom_text(hjust = 1, size = 0.5, color = '#ffba08',aes( y = 0, label = paste(name,""))) +
  geom_text(hjust = 1, size = 0.5, color = '#ffba08',aes( y = 0, label = paste(total_hrs_sum,""))) +
  ggtitle('117,993 HOURS SPENT ON SPACE MISSIONS (WOMEN)') +
  theme_minimal() +
  theme(plot.title = element_text(family = 'sans', size = 5, color = '#dc2f02',hjust = 0.5, vjust = -145)) +
  theme(
    panel.grid.major  = element_blank(),
    panel.background = element_rect(fill = "#03071e"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none",
    axis.text = element_blank()
  ) +
  xlab("") +
  ylab("") +
  coord_polar(theta = "y") +
  ylim(0,20000) +
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
  )
  
myplot

ggsave(myplot, file = "myplot.png", dpi = 700)



# library(ragg)
# ggforce::geom_link
# geom_richtext
# geom_textbox
# annotate
# plot.margin
# glue

glimpse(astronauts)

table(astronauts$year_of_mission)
table(astronauts$nationality)


astronauts %>%
  filter(sex == 'female') %>% 
  ggplot(aes(x=year_of_mission, y=name, size = hours_mission, color = nationality)) + 
  geom_point(alpha = 0.7) +
  scale_size(range = c(.1, 10), name="Hours it took for the mission") +
  scale_x_continuous(name="Year", limits=c(1960, 2019), breaks=seq(0, 2019, 10)) +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="magma") +
  theme_modern_rc() +
  theme(legend.position="bottom") +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank()
    ) +
  labs(title="An Overview of the Missions undertaken by Female Astronauts",
       subtitle="Female astronauts did not exist until the 1980s, and most of them came from USSR/Russia. Whitson Peggy A has taken up 3 major space missions, each more\nthan 4000 hours each. Also, it seems that it is only after the year 2000 that female astronauts are taking up longer and more challenging space missions.",
       x = "",
       y = "")

glimpse(astronauts)
table(astronauts$total_number_of_missions)


astronauts <- data.table(astronauts)
myvars <- c("name", "total_hrs_sum", "sex")
df1 <- astronauts[myvars]


df1 %>%
  ggplot(aes(reorder(name, total_hrs_sum, sum), 
             total_hrs_sum
  )) +
  coord_flip() +
  geom_bar(position="dodge", stat="identity") +
  scale_size(range = c(.1, 10), name="Total Space Mission Hours") +
 #  scale_x_continuous(name="Year", limits=c(1960, 2019), breaks=seq(0, 2019, 10)) +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="D") +
  facet_grid(~sex) +
  theme_modern_rc() +
  theme(legend.position="bottom") +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank()
  ) +
  labs(title="An Overview of the Missions undertaken by Female Astronauts",
       subtitle="Female astronauts did not exist until the 1980s, and most of them came from USSR/Russia. Whitson Peggy A has taken up 3 major space missions, each more\nthan 4000 hours each. Also, it seems that it is only after the year 2000 that female astronauts are taking up longer and more challenging space missions.",
       x = "",
       y = "")

