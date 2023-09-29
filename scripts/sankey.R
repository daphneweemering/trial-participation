library(readxl)
library(zoo)
library(tidyverse)
library(ggsankey)
library(cowplot)

setwd('/Users/daphneweemering/surfdrive/trial participation/trial-participation/data')
d <- read_excel('code tree.xlsx')

# new data frame for plotting
sankey <- data.frame(factor = rep(c('Patient-related factors', 
                                    'Study-related factors', 
                                    'HCP-related factors'), times = c(13, 13, 5)),
                     theme = na.locf(d$Themes),
                     subtheme = d$Subthemes, 
                     frequency = NA)

# organize data to count occurrence
d1 <- d[, -1]
d1$Themes <- ifelse(is.na(d1$Subthemes) == T, d1$Themes, d1$Subthemes)
d1 <- d1[, -2]

for (i in 1:nrow(d1)){
  sankey[i, 4] <- sum(is.na(d1[i, ]) == F) - 1
}

# prepare data
factors <- list('Patient-related factors', 'Study-related factors', 
                'HCP-related factors')

dl <- list()
dl2 <- list()
nms <- list()

for (i in 1:length(factors)){
  # subset into patient-, study, and HCP-related sets
  dl[[i]] <- subset(sankey, sankey$factor == factors[[i]])

  # order values and assign letters to get the right ordering 
  dl[[i]] <- dl[[i]][order(dl[[i]]$frequency, dl[[i]]$theme, decreasing = T), ]
  dl[[i]]$theme2 <- LETTERS[as.numeric(factor(dl[[i]]$theme,
                                                    levels = unique(dl[[i]]$theme)))]
  
  if (i == 1) {
    dl[[i]]$subtheme2 <- ifelse(is.na(dl[[i]]$subtheme) == F, LETTERS[14:(nrow(dl[[i]]))], NA)
  } else if (i == 2) {
    dl[[i]] <- dl[[i]][order(dl[[i]]$theme2), ]
    dl[[i]]$subtheme2 <- ifelse(is.na(dl[[i]]$subtheme) == F, LETTERS[4:(nrow(dl[[i]]) + 4)], NA)
  }

  dl2[[i]] <- dl[[i]]

  # change to long format
  if (i == 1 | i == 2) {
    dl2[[i]] <- dl[[i]] %>%
      make_long(factor, theme2, subtheme2, value = frequency)
  } else if (i == 3) {
    dl2[[i]] <- dl[[i]] %>%
      make_long(factor, theme2, subtheme, value = frequency)
  }

  dl2[[i]] <- subset(dl2[[i]], is.na(dl2[[i]]$node) == F)
 
}

# make figures
for (i in 1:length(factors)){
  # assign labels
  if (i == 1 | i == 2) {
    nms[[i]] <- as.data.frame(cbind(c(dl[[i]]$theme2, na.omit(dl[[i]]$subtheme2)), 
                                    c(dl[[i]]$theme, na.omit(dl[[i]]$subtheme))))
  } else if (i == 3) {
    nms[[i]] <- as.data.frame(cbind(dl[[i]]$theme2, dl[[i]]$theme)) 
  }
  
  dl2[[i]]$lab <- ifelse(dl2[[i]]$node == factors[[i]], factors[[i]], 
                         nms[[i]]$V2[match(dl2[[i]]$node, nms[[i]]$V1)])
  
} 

pltt1 <- c("#106aba", "#106aba", "#106aba", "#106aba", "#106aba", "#106aba",
           "#106aba", "#106aba", "#106aba", "#106aba", "#106aba", "#106aba",
           "#3daaf0", "#3daaf0", "#001d76")

pltt2 <- c("#106aba", "#106aba", "#106aba", "#3daaf0", "#3daaf0", "#3daaf0",
           "#3daaf0", "#3daaf0", "#3daaf0", "#3daaf0", "#3daaf0", "#3daaf0",
           "#3daaf0", "#3daaf0", "#3daaf0", "#001d76")

pltt3 <- c("#106aba", "#106aba", "#106aba", "#106aba", "#106aba", "#001d76")

# create flow charts for each factor 
p1 <- dl2[[1]] %>%
  ggplot(aes(x = x, next_x = next_x, node = node, next_node = next_node,
             fill = node, value = value, label = lab)) +
  geom_sankey(flow.alpha = 0.5, node.color = NA, show.legend = F, space = 5,
              width = 0.08, smooth = 12) +
  geom_sankey_text(size = 12, color = "black", hjust = 0, 
                   position = position_nudge(x = 0.05), space = 5) +
  scale_fill_manual(values = pltt1) +  
  theme_void() +
  geom_text(aes(label =paste('n =', sum(sankey$frequency[sankey$factor == 'Patient-related factors'])), 
                x = 1.12, y = -9), size = 12, nudge_x = 0.05, alpha = 0.08)

p2 <- dl2[[2]] %>%
  ggplot(aes(x = x, next_x = next_x, node = node, next_node = next_node,
             fill = node, value = value, label = lab)) +
  geom_sankey(flow.alpha = 0.5, node.color = NA, show.legend = F, space = 5,
              width = 0.08, smooth = 12) +
  geom_sankey_text(size = 12, color = "black", hjust = 0, 
                   position = position_nudge(x = 0.05), space = 5) +
  scale_fill_manual(values = pltt2) +  
  theme_void() + 
  geom_text(aes(label = paste('n =', sum(sankey$frequency[sankey$factor == 'Study-related factors'])), 
                x = 1.12, y = -7), size = 12, nudge_x = 0.05, alpha = 0.05)

p3 <- dl2[[3]] %>%
  ggplot(aes(x = x, next_x = next_x, node = node, next_node = next_node,
             fill = node, value = value, label = lab)) +
  geom_sankey(flow.alpha = 0.5, node.color = NA, show.legend = F, space = 5,
              width = 0.08, smooth = 12) +
  geom_sankey_text(size = 12, color = "black", hjust = 0, 
                   position = position_nudge(x = 0.05), space = 5) +
  scale_fill_manual(values = pltt3) +  
  theme_void() +
  geom_text(aes(label = paste('n =', sum(sankey$frequency[sankey$factor == 'HCP-related factors'])), 
                x = 1.115, y = -5), size = 12, nudge_x = 0.05, alpha = 0.18)


pdf(file = '/Users/daphneweemering/surfdrive/trial participation/trial-participation/figures/figure2.pdf',
    height = 57.5, width = 40.5)

ggdraw() +
  draw_plot(p3, x = -0.182, y = 0, height = 0.25, width = 0.761) +
  draw_plot(p2, x = -0.18, y = 0.23, height = 0.4, width = 1.1) +
  draw_plot(p1, x = -0.18, y = 0.60, height = 0.4, width = 1.1) 

dev.off()


