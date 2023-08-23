#===================================================================================================================
# Author: Aloysius Ong
# Dataset: #tidytuesday - African-American History
# Purpose: Visualization 
#===================================================================================================================

setwd("C:/Users/User/Desktop")

library(dplyr)
library(ggcharts)
library(hrbrthemes)
library(viridis)
library(extrafont)
library(forcats)
library(gghighlight)
library(dplyr)
library(tibble)
library(ggThemeAssist)
library(data.table)
library(tidyverse)
library(rvest)
library(ggridges)
library(lubridate)
library(scales)
library(ggridges)
library(tidytext)
library(tidyverse)
library(dplyr)
library(debugr)
library(colorspace)
library(maps)
library(networkD3)
library(htmlwidgets)

#---- Loading Data ----

blackpast <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/blackpast.csv')
census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/census.csv')
slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')
african_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/african_names.csv')

glimpse(slave_routes)

table(slave_routes$port_origin)
table(slave_routes$year_arrival)

aggregated_df <- slave_routes %>% 
  distinct(voyage_id, n_slaves_arrived, .keep_all = TRUE) %>% 
  mutate(decade = year_arrival %/% 10 *10,
         place_of_purchase = str_replace(place_of_purchase, ".?, port unspecified" , " (Port unknown)"))

summary(aggregated_df)

aggregated_df <- aggregated_df[complete.cases(aggregated_df), ]
aggregated_df <- data.frame(aggregated_df)

summary(aggregated_df)

table(aggregated_df$decade)

aggregated_df

choices <- aggregated_df %>% 
  filter(decade == 1790) %>%
  group_by(port_arrival) %>% 
  mutate(total_slaves = sum(n_slaves_arrived, na.rm =T)) %>% 
  arrange(-total_slaves) %>% 
  slice(1:10) %>%
  ungroup()

choices <- data.frame(choices)

summary(choices)

myvars0 <- c("port_arrival")
smallerdf <- choices[myvars0]

myvars4 <- c("port_arrival", "total_slaves")
snakeydf <- choices[myvars4]

smallerdf

slaveroutes1790 <- select(filter(aggregated_df, decade == 1790), port_origin, port_arrival)

slaveroutes1790 <- merge(slaveroutes1790, smallerdf, by="port_arrival")

slaveroutes1790_1 <- merge(slaveroutes1790, snakeydf, by="port_arrival")

slaveroutes1790
slaveroutes1790_1
#---- Sankey Diagram ----

# myvars <- c("port_origin", "port_arrival", "n_slaves_arrived")
# network_routes <- slave_routes[myvars]

network_routes <- data.frame(slaveroutes1790_1)

nodes <- data.frame(
  name=c(as.character(network_routes$port_origin), 
         as.character(network_routes$port_arrival)) %>% unique()
)

network_routes$IDsource <- match(network_routes$port_origin, nodes$name)-1 
network_routes$IDtarget <- match(network_routes$port_arrival, nodes$name)-1

p <- sankeyNetwork(Links = network_routes, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "total_slaves", NodeID = "name", 
                   sinksRight=FALSE)
p


saveWidget(p, "sankeyBasic1.html")

#---- Chord Diagram ----

# Transform input data in a adjacency matrix
adjacencyData <- with(slaveroutes1790, table(port_origin, port_arrival))

# Charge the circlize library
library(circlize)

# Make the circular plot
chordDiagram(adjacencyData, transparency = 0.5)

#---- Arc Diagram ----

library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(igraph)
library(ggraph)
library(colormap)

# Transform to a igraph object
mygraph <- graph_from_data_frame(slaveroutes1790)

# Make the usual network diagram
p1 <-  ggraph(mygraph) + 
  geom_edge_link(edge_colour="black", edge_alpha=0.3, edge_width=0.2) +
  geom_node_point( color="#69b3a2", size=5) +
  geom_node_text( aes(label=name), repel = TRUE, size=8, color="#69b3a2") +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(rep(2,4), "cm")
  ) 

p1

# Make a cord diagram
p2 <-  ggraph(mygraph, layout="linear") + 
  geom_edge_arc(edge_colour="black", edge_alpha=0.3, edge_width=0.2) +
  geom_node_point( color="#69b3a2", size=0.5) +
  geom_node_text(aes(label=name), repel = FALSE, size=5, color="#69b3a2", nudge_y=-0.1) +
  theme_void() +
 # gghighlight(port_origin == "Liverpool") +
  theme(
    legend.position="bottom",
    plot.margin=unit(rep(2,4), "cm")
  ) 

p1 + p2

p2

summary(choices)

choices %>% 
  ggplot(aes(x = year_arrival, y = total_slaves)) +
  geom_line(size=1) +
  facet_wrap(~ place_of_purchase, ncol=4) +
  scale_color_manual(values=c("#9999CC","#CC6666")) +
  theme_ipsum_rc() +
  theme(legend.position = "none") +
  labs(x = "", y = "",
       title = "Trend of Human Trade in the 1790s",
       subtitle = "Some ports are definitely more popular than the others.",
       caption = "Data: slavevoyages.org, 2018")











