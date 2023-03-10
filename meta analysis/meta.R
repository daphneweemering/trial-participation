setwd('/Users/daphneweemering/surfdrive/B&F CT participation/meta-analysis')

library(readxl)
library(meta)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

dat <- read_excel('datafile_meta.xlsx')

#### DATA PREP ####
datalist <- list()
fclttrs <- list('Side effects', 'Invasiveness', 'Placebo/sham use', 'Visiting scheme',
                  'Time consumption', 'Assessment burden', 'Travel burden', 
                  'Financial compensation', 'Awareness', 'HCP recommendation', 
                  'Information provided', 'Relationship with clinical staff', 
                  'Clinic reputation')

for (i in 1:length(fclttrs)){
  datalist[[i]] <- subset(dat, dat$Facilitator == fclttrs[[i]] & dat$Type == 'P')
}


#### ANALYSIS ####
# overall meta-analyses
store <- matrix(data = NA, nrow = 13, ncol = 16)
colnames(store) <- c('TE', 'LB', 'UB', 'k', 'events', 'total', 'facilitator', 
                     'tau2', 'H', 'H_lower', 'H_upper', 'I2', 'I2_lower', 
                     'I2_upper', 'Q', 'Q_pval')

for(i in 1:length(datalist)){
  m <- metaprop(event = n_respond, n = n_total, studlab = Disease, data = datalist[[i]],
                method = 'GLMM', sm = 'PLOGIT', random = T, hakn = F)
  
  store[i, 1] <- meta:::backtransf(m$TE.random, sm = 'PLOGIT')*100
  store[i, 2] <- meta:::backtransf(m$lower.random, sm = 'PLOGIT')*100
  store[i, 3] <- meta:::backtransf(m$upper.random, sm = 'PLOGIT')*100
  store[i, 4] <- m$k 
  store[i, 5] <- sum(m$event)
  store[i, 6] <- sum(m$n)
  store[i, 7] <- datalist[[i]]$Facilitator[[1]]
  store[i, 8] <- m$tau2
  store[i, 9] <- m$H
  store[i, 10] <- m$lower.H
  store[i, 11] <- m$upper.H
  store[i, 12] <- m$I2
  store[i, 13] <- m$lower.I2
  store[i, 14] <- m$upper.I2
  store[i, 15] <- m$Q.LRT
  store[i, 16] <- round(as.numeric(m$pval.Q.LRT), 6)
  datalist[[i]]$sep_prop <- (datalist[[i]]$n_respond/datalist[[i]]$n_total)*100
  
}

datalist <- datalist[order(store[, 'TE'])]
store <- store[order(store[, 'TE'], decreasing = F),]


# meta regression




#### FIGURE ####
pdf(file = '/Users/daphneweemering/surfdrive/B&F CT participation/tables + figures/figure1.pdf', 
    width = 11.7, height = 7)

par(mar = c(4.5, 34, 3, 1), tck = -0.01, xpd = T)
plot(as.numeric(store[, 'TE']), 1:nrow(store), pch = 22, cex = 1.1, frame.plot = F, 
     axes = F, ylab = '', xlab = 'Percentage of pooled responses',
     cex.lab = 0.8, xlim = c(0, 100), col = 'grey16', bg = 'grey16')
segments(x0 = as.numeric(store[, 'UB']), y0 = 1:nrow(store), 
         x1 = as.numeric(store[, 'LB']), y1 = 1:nrow(store))
axis(1, cex.axis = 0.8)
for(i in 1:length(datalist)){
  points(datalist[[i]]$sep_prop, rep(i, times = nrow(datalist[[i]])), pch = 22, 
         col = 'grey', bg = 'grey88', cex = 0.7)
}

# text 
for (i in 1:length(datalist)){
  text(x = -153, y = i+0.1, datalist[[i]]$Facilitator[1], cex = 1, adj = c(0,0.5))
  text(x = -85, y = i+0.1, store[i,'k'], cex = 1)
  text(x = -60, y = i+0.1, paste(store[i, 'events'], '/', store[i, 'total']), cex = 1)
  text(x = -25, y = i+0.1, paste(round(as.numeric(store[i, 'TE']), 2), ' (', 
                             round(as.numeric(store[i, 'LB']), 2), '-', 
                             round(as.numeric(store[i, 'UB']), 2), ')'), cex = 1)
}

text(x = -153, y = 13.8, 'Barrier/facilitator', cex = 1, adj = c(0,0.5), font = 2)
text(x = -85, y = 14.3, '# of pooled', cex = 1, font = 2)
text(x = -85, y = 13.8, 'studies', cex = 1, font = 2)
text(x = -60, y = 13.8, 'Incidence', cex = 1, font = 2)
text(x = -25, y = 13.8, 'Percentage (95% CI)', cex = 1, font = 2)


dev.off()












