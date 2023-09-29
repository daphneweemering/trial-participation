setwd('/Users/daphneweemering/surfdrive/trial participation/trial-participation/data')

library(readxl)
library(metafor)
library(tidyverse)
library(dplyr)

dat <- read_excel('meta.xlsx')

fclttrs <- list('Assessment burden', 'Awareness', 'Clinic reputation', 
                'Disruption in current medication', 'Financial compensation',
                'HCP recommendation', 'Home-based assessments', 'Information provided',
                'Invasiveness', 'Placebo/sham use', 'Relationship with clinical staff',
                'Side effects', 'Time consumption', 'Travel burden', 'Visiting scheme')

datalist <- list()
for (i in 1:length(fclttrs)){
  datalist[[i]] <- subset(dat, dat$FACILITATOR == fclttrs[[i]])
  datalist[[i]]$mod <- ifelse(datalist[[i]]$DISEASE != 'AD', 0, 1)
}



#### ANALYSIS ####
# overall meta-analyses
store <- matrix(data = NA, nrow = length(datalist), ncol = 23)
colnames(store) <- c('TE', 'LB', 'UB', 'k', 'events', 'total', 'facilitator', 'I2',
                     'H2', 'tau2', 'Q_LRT', 'Qp_LRT','TE_m0', 'TE_m1', 'OR', 'ORp',
                     'nAD_LB', 'nAD_UB', 'OR_LB', 'OR_UB', 'AD_LB', 'AD_UB', 'I2_mod')

for(i in 1:length(datalist)){
  m <- rma.glmm(xi = N, ni = TOTAL, data = datalist[[i]], measure = 'PLO')
  if(length(datalist[[i]]$mod) > 2){
    mmod <- update(m, mods = mod)
    predicted <- stats::predict(mmod, newdata = data.frame(mods = c(0,1)))
  }
  
  # main outcomes
  mb <- predict(m, transf = transf.ilogit, digits = 4)
  store[i, 1] <- mb$pred*100
  store[i, 2] <- mb$ci.lb*100
  store[i, 3] <- mb$ci.ub*100
  store[i, 4] <- m$k
  store[i, 5] <- sum(datalist[[i]]$N)
  store[i, 6] <- sum(m$ni)
  store[i, 7] <- datalist[[i]]$FACILITATOR[[1]]
  datalist[[i]]$sep_prop <- (datalist[[i]]$N/datalist[[i]]$TOTAL)*100
  
  if(length(datalist[[i]]$mod) > 2){
    # heterogeneity statistics
    store[i, 8] <- m$I2
    store[i, 9] <- m$H2
    store[i, 10] <- m$tau2
    store[i, 11] <- m$QE.LRT
    store[i, 12] <- m$QEp.LRT
    
    # estimates with moderator
    store[i, 13] <- exp(mmod$beta[1])/(1 + exp(mmod$beta[1]))*100
    store[i, 14] <- exp(mmod$beta[1] + mmod$beta[2])/(1 + exp(mmod$beta[1] + mmod$beta[2]))*100
    store[i, 15] <- exp(mmod$beta[2])
    store[i, 16] <- mmod$pval[2]
    store[i, 17] <- plogis(mmod$ci.lb[[1]])*100
    store[i, 18] <- plogis(mmod$ci.ub[[1]])*100
    store[i, 19] <- exp(mmod$ci.lb[[2]])
    store[i, 20] <- exp(mmod$ci.ub[[2]])
    store[i, 21] <- plogis(predicted$ci.lb[[1]])*100
    store[i, 22] <- plogis(predicted$ci.ub[[1]])*100
    store[i, 23] <- mmod$I2
  }
  
}

datalist <- datalist[order(store[, 'TE'])]
store <- store[order(store[, 'TE'], decreasing = F),]

#### FIGURE ####
pdf(file = '/Users/daphneweemering/surfdrive/trial participation/trial-participation/figures/figure3.pdf', 
    width = 11.7, height = 7)

par(mar = c(4.5, 34, 3, 1), tck = -0.01, xpd = T)
plot(as.numeric(store[, 'TE']), 1:nrow(store), pch = 22, cex = 1.1, frame.plot = F, 
     axes = F, ylab = '', xlab = 'Percentage of pooled responses', xlim = c(0, 100), 
     col = 'grey16', bg = 'grey16')
segments(x0 = as.numeric(store[, 'UB']), y0 = 1:nrow(store), 
         x1 = as.numeric(store[, 'LB']), y1 = 1:nrow(store))
axis(1, cex.axis = 1)
for(i in 1:length(datalist)){
  points(datalist[[i]]$sep_prop, rep(i, times = nrow(datalist[[i]])), pch = 22, 
         col = 'grey', bg = 'grey88', cex = 0.7)
}

# estimates
for (i in 1:length(datalist)){
  text(x = -153, y = i+0.1, datalist[[i]]$FACILITATOR[1], cex = 1, adj = c(0,0.5))
  text(x = -85, y = i+0.1, store[i,'k'], cex = 1)
  text(x = -60, y = i+0.1, paste0(store[i, 'events'], '/', store[i, 'total']), cex = 1)
  text(x = -25, y = i+0.1, paste0(format(round(as.numeric(store[i, 'TE']), 2), nsmall = 2), ' (',
                                  format(round(as.numeric(store[i, 'LB']), 2), nsmall = 2), '-',
                                  format(round(as.numeric(store[i, 'UB']), 2), nsmall = 2), ')'), cex = 1)
  
}

# headers
text(x = -153, y = 16, 'Enabler', cex = 1, adj = c(0,0.5), font = 2)
text(x = -85, y = 16.5, '# of pooled', cex = 1, font = 2)
text(x = -85, y = 16, 'studies', cex = 1, font = 2)
text(x = -60, y = 16.5, 'Response', cex = 1, font = 2)
text(x = -60, y = 16, 'rate', cex = 1, font = 2)
text(x = -25, y = 16, 'Percentage (95% CI)', cex = 1, font = 2)


dev.off()



