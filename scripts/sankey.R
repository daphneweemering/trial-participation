#### Barriers and facilitators of clinical trial participation in neurodegenerative
#### diseases: a systematic review and meta-analyses.

# 1. libraries
library(readxl)
library(ggsankey)
library(dplyr)
library(ggplot2)
library(viridis)
library(cowplot)

# 2. data
setwd('//Users/daphneweemering/surfdrive/trial participation/trial-participation/data')
d <- read_excel('flowchart.xlsx')

# 3. prepare data
factors <- list('Patient-related factors', 'Study-related factors', 
                'HCP-related factors')

datalist <- list()

for (i in 1:length(factors)){
  datalist[[i]] <- subset(d, d$factor == factors[[i]])
  
  datalist[[i]] <- datalist[[i]] %>%
    make_long(factor, theme, subtheme, value = frequency)
  
  datalist[[i]] <- subset(datalist[[i]], is.na(datalist[[i]]$node) == F)
  
}


# 4. Plot
p1 <- datalist[[1]] %>%
  ggplot(aes(x = x, next_x = next_x, node = node, next_node = next_node,
             fill = factor(node), value = value, label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = NA, show.legend = F, space = 5,
              width = 0.08, smooth = 12) +
  scale_fill_viridis_d(option = "C", alpha = 0.8) +
  geom_sankey_text(size = 12, color = "black", hjust = 0, 
                   position = position_nudge(x = 0.05), space = 5) +
  theme_void() 

p2 <- datalist[[2]] %>%
  ggplot(aes(x = x, next_x = next_x, node = node, next_node = next_node,
             fill = factor(node), value = value, label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = NA, show.legend = F, space = 5,
              width = 0.08, smooth = 12) +
  scale_fill_viridis_d(option = "C", alpha = 0.8) +
  geom_sankey_text(size = 12, color = "black", hjust = 0, 
                   position = position_nudge(x = 0.05), space = 5) +
  theme_void() 

p3 <- datalist[[3]] %>%
  ggplot(aes(x = x, next_x = next_x, node = node, next_node = next_node,
             fill = factor(node), value = value, label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = NA, show.legend = F, space = 5,
              width = 0.08, smooth = 12) +
  scale_fill_viridis_d(option = "C", alpha = 0.8) +
  geom_sankey_text(size = 12, color = "black", hjust = 0, 
                   position = position_nudge(x = 0.05), space = 5) +
  theme_void() 


pdf(file = '/Users/daphneweemering/surfdrive/trial participation/trial-participation/figures/figure2.pdf',
    height = 57.5, width = 38.41)

ggdraw() +
  draw_plot(p3, x = -0.182, y = 0, height = 0.25, width = 0.761) +
  draw_plot(p2, x = -0.18, y = 0.23, height = 0.4, width = 1.1) +
  draw_plot(p1, x = -0.18, y = 0.60, height = 0.4, width = 1.1) +
  draw_label(label = 'a', x = -0.121, y = 0.20)

dev.off()


 