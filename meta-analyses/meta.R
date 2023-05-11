setwd('/Users/daphneweemering/surfdrive/trial participation/meta-analysis')

library(readxl)
library(metafor)
library(tidyverse)
library(dplyr)


dat <- read_excel('datafile_meta.xlsx')

fclttrs <- list('Assessment burden', 'Awareness', 'Clinic reputation', 
                'Disruption in current medication', 'Financial compensation',
                'HCP recommendation', 'Home-based assessments', 'Information provided',
                'Invasiveness', 'Placebo/sham use', 'Relationship with clinical staff',
                'Side effects', 'Time consumption', 'Travel burden', 'Visiting scheme')

for (i in 1:length(fclttrs)){
  datalist[[i]] <- subset(dat, dat$Facilitator == fclttrs[[i]])
}



#### ANALYSIS ####
# overall meta-analyses
store <- matrix(data = NA, nrow = 15, ncol = 7)
colnames(store) <- c('TE', 'LB', 'UB', 'k', 'events', 'total', 'facilitator')

for(i in 1:length(datalist)){
  m <- rma.glmm(xi = n_respond, ni = n_total, data = datalist[[i]], measure = 'PLO')
  
  # main outcomes
  mb <- predict(m, transf = transf.ilogit, digits = 4)
  store[i, 1] <- mb$pred*100
  store[i, 2] <- mb$ci.lb*100
  store[i, 3] <- mb$ci.ub*100
  store[i, 4] <- m$k
  store[i, 5] <- sum(datalist[[i]]$n_respond)
  store[i, 6] <- sum(m$ni)
  store[i, 7] <- datalist[[i]]$Facilitator[[1]]
  datalist[[i]]$sep_prop <- (datalist[[i]]$n_respond/datalist[[i]]$n_total)*100
  
}

datalist <- datalist[order(store[, 'TE'])]
store <- store[order(store[, 'TE'], decreasing = F),]

#### FIGURE ####
pdf(file = '/Users/daphneweemering/surfdrive/trial participation/tables + figures/figure1.pdf', 
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

text(x = -153, y = 16, 'Barrier/facilitator', cex = 1, adj = c(0,0.5), font = 2)
text(x = -85, y = 16.5, '# of pooled', cex = 1, font = 2)
text(x = -85, y = 16, 'studies', cex = 1, font = 2)
text(x = -60, y = 16, 'Incidence', cex = 1, font = 2)
text(x = -25, y = 16, 'Percentage (95% CI)', cex = 1, font = 2)


dev.off()







