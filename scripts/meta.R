setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(readxl)
library(metafor)
library(tidyverse)
library(dplyr)

dat <- read_excel("../data/meta.xlsx")

fclttrs <- list('Assessment burden', 'Awareness', 'Clinic reputation', 
                'Disruption in current medication', 'Financial compensation',
                'HCP recommendation', 'Home-based assessments', 'Information provided',
                'Invasiveness', 'Placebo/sham use', 'Relationship with clinical staff',
                'Side effects', 'Time consumption', 'Travel burden', 'Visiting scheme')

datalist <- list()

# subset (patient-related, study-related, HCP-related)
for (i in 1:length(fclttrs)){
  datalist[[i]] <- subset(dat, dat$FACILITATOR == fclttrs[[i]])
  datalist[[i]]$mod <- ifelse(datalist[[i]]$DISEASE != 'AD', 0, 1)
}


#### ANALYSIS ####
# make storage
fig3 <- matrix(NA, nrow = length(datalist), ncol = 7)
tab2 <- matrix(NA, nrow = length(datalist), ncol = 11)
suppl.tab <- matrix(NA, nrow = length(datalist), ncol = 9)

colnames(fig3) <- c('enabler', 'k', 'RR', 'TOT', 'PERC', 'LB', 'UB')
colnames(tab2) <- c('enabler', 'PERC_nAD', 'LB_nAD', 'UB_nAD', 'PERC_AD', 'LB_AD',
                    'UB_AD', 'OR', 'LB_OR', 'UB_OR', 'p_OR')
colnames(suppl.tab) <- c('enabler', 'k', 'I2', 'Q', 'pQ', 'I2_mod', 'Q_mod', 'pQ_mod', 'tau_diff')

# meta analysis (univariate and moderator)
for(i in 1:length(datalist)){
  m <- rma.glmm(xi = N, ni = TOTAL, data = datalist[[i]], measure = 'PLO')
  if(length(datalist[[i]]$mod) > 2){
    mmod <- update(m, mods = mod)
    predicted <- stats::predict(mmod, newdata = data.frame(mods = c(0,1)))
  }
  
  mb <- predict(m, transf = transf.ilogit, digits = 4)
  
  # figure 3
  fig3[i, 1] <- datalist[[i]]$FACILITATOR[[1]]
  fig3[i, 2] <- m$k
  fig3[i, 3] <- sum(datalist[[i]]$N)
  fig3[i, 4] <- sum(m$ni)
  fig3[i, 5] <- mb$pred*100
  fig3[i, 6] <- mb$ci.lb*100
  fig3[i, 7] <- mb$ci.ub*100
  datalist[[i]]$sep_prop <- (datalist[[i]]$N/datalist[[i]]$TOTAL)*100 # proportions of individual studies
  
  if(length(datalist[[i]]$mod) > 2){
    # table 2
    tab2[i, 1] <- datalist[[i]]$FACILITATOR[[1]]
    tab2[i, 2] <- exp(mmod$beta[1])/(1 + exp(mmod$beta[1]))*100
    tab2[i, 3] <- plogis(mmod$ci.lb[[1]])*100
    tab2[i, 4] <- plogis(mmod$ci.ub[[1]])*100
    tab2[i, 5] <- exp(mmod$beta[1] + mmod$beta[2])/(1 + exp(mmod$beta[1] + mmod$beta[2]))*100
    tab2[i, 6] <- plogis(predicted$ci.lb[[1]])*100
    tab2[i, 7] <- plogis(predicted$ci.ub[[1]])*100
    tab2[i, 8] <- exp(mmod$beta[2])
    tab2[i, 9] <- exp(mmod$ci.lb[[2]])
    tab2[i, 10] <- exp(mmod$ci.ub[[2]])
    tab2[i, 11] <- mmod$pval[2]
    
    suppl.tab[i, 1] <- datalist[[i]]$FACILITATOR[[1]]
    suppl.tab[i, 2] <- m$k
    suppl.tab[i, 3] <- m$I2
    suppl.tab[i, 4] <- m$QE.LRT
    suppl.tab[i, 5] <- m$QEp.LRT
    suppl.tab[i, 6] <- mmod$I2
    suppl.tab[i, 7] <- mmod$QE.LRT
    suppl.tab[i, 8] <- mmod$QEp.LRT
    suppl.tab[i, 9] <- (sqrt(m$tau2) - sqrt(mmod$tau2)) / m$tau2 * 100 # percentage reduction in tau after adding moderator
  }
  
}

datalist <- datalist[order(fig3[, 'PERC'])]
fig3 <- fig3[order(fig3[, 'PERC'], decreasing = F),]

#### FIGURE ####
pdf(file = '/Users/daphneweemering/surfdrive/trial participation/trial-participation/figures/figure3.pdf', 
    width = 11.7, height = 7)

par(mar = c(4.5, 34, 3, 1), tck = -0.01, xpd = T)
plot(as.numeric(fig3[, 'PERC']), 1:nrow(fig3), pch = 22, cex = 1.1, frame.plot = F, 
     axes = F, ylab = '', xlab = 'Percentage of pooled responses', xlim = c(0, 100), 
     col = 'grey16', bg = 'grey16')
segments(x0 = as.numeric(fig3[, 'UB']), y0 = 1:nrow(fig3), 
         x1 = as.numeric(fig3[, 'LB']), y1 = 1:nrow(fig3))
axis(1, cex.axis = 1)
for(i in 1:length(datalist)){
  points(datalist[[i]]$sep_prop, rep(i, times = nrow(datalist[[i]])), pch = 22, 
         col = 'grey', bg = 'grey88', cex = 0.7)
}

# estimates
for (i in 1:length(datalist)){
  text(x = -153, y = i+0.1, datalist[[i]]$FACILITATOR[1], cex = 1, adj = c(0,0.5))
  text(x = -85, y = i+0.1, fig3[i,'k'], cex = 1)
  text(x = -60, y = i+0.1, paste0(fig3[i, 'RR'], '/', fig3[i, 'TOT']), cex = 1)
  text(x = -25, y = i+0.1, paste0(format(round(as.numeric(fig3[i, 'PERC']), 2), nsmall = 2), ' (',
                                  format(round(as.numeric(fig3[i, 'LB']), 2), nsmall = 2), '-',
                                  format(round(as.numeric(fig3[i, 'UB']), 2), nsmall = 2), ')'), cex = 1)
  
}

# headers
text(x = -153, y = 16, 'Enabler', cex = 1, adj = c(0,0.5), font = 2)
text(x = -85, y = 16.5, '# of pooled', cex = 1, font = 2)
text(x = -85, y = 16, 'studies', cex = 1, font = 2)
text(x = -60, y = 16.5, 'Response', cex = 1, font = 2)
text(x = -60, y = 16, 'rate', cex = 1, font = 2)
text(x = -25, y = 16, 'Percentage (95% CI)', cex = 1, font = 2)


dev.off()