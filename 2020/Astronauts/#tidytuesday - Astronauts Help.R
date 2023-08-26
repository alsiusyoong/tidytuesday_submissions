#===================================================================================================================
# Author: Aloysius Ong
# Dataset: #tidytuesday - Astronauts
# Purpose: Visualization 
#===================================================================================================================

astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

library(plotly)
library(dplyr)
library(ggthemes)

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
