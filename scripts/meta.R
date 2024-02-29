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
store <- matrix(NA, nrow = length(datalist), ncol = 26)
colnames(store) <- c('enabler', 'k', 'RR', 'TOT', 'PERC', 'LB', 'UB', # fig3
                     
                     'PERC_nAD', 'LB_nAD', 'UB_nAD', 'PERC_AD', 'LB_AD',
                     'UB_AD', 'OR', 'LB_OR', 'UB_OR', 'p_OR',         # table 2
                     
                     'I2', 'Q', 'pQ', 'tau', 'I2_mod', 'Q_mod', 
                     'pQ_mod', 'tau_mod', 'tau_diff')                 # suppl. table 6

# meta analysis (univariate and moderator)
for(i in 1:length(datalist)){
  m <- rma.glmm(xi = N, ni = TOTAL, data = datalist[[i]], measure = 'PLO')
  if(length(datalist[[i]]$mod) > 2){
    mmod <- update(m, mods = mod)
    predicted <- stats::predict(mmod, newdata = data.frame(mods = c(0,1)))
  }
  
  mb <- predict(m, transf = transf.ilogit, digits = 4)
  
  # figure 3
  store[i, 1] <- datalist[[i]]$FACILITATOR[[1]]
  store[i, 2] <- m$k
  store[i, 3] <- sum(datalist[[i]]$N)
  store[i, 4] <- sum(m$ni)
  store[i, 5] <- mb$pred*100
  store[i, 6] <- mb$ci.lb*100
  store[i, 7] <- mb$ci.ub*100
  datalist[[i]]$sep_prop <- (datalist[[i]]$N/datalist[[i]]$TOTAL)*100 # proportions of individual studies
  
  if(length(datalist[[i]]$mod) > 2){
    # table 2
    store[i, 8] <- exp(mmod$beta[1])/(1 + exp(mmod$beta[1]))*100
    store[i, 9] <- plogis(mmod$ci.lb[[1]])*100
    store[i, 10] <- plogis(mmod$ci.ub[[1]])*100
    store[i, 11] <- exp(mmod$beta[1] + mmod$beta[2])/(1 + exp(mmod$beta[1] + mmod$beta[2]))*100
    store[i, 12] <- plogis(predicted$ci.lb[[1]])*100
    store[i, 13] <- plogis(predicted$ci.ub[[1]])*100
    store[i, 14] <- exp(mmod$beta[2])
    store[i, 15] <- exp(mmod$ci.lb[[2]])
    store[i, 16] <- exp(mmod$ci.ub[[2]])
    store[i, 17] <- mmod$pval[2]
    
    # supplementary table 6
    store[i, 18] <- m$I2
    store[i, 19] <- m$QE.LRT
    store[i, 20] <- m$QEp.LRT
    store[i, 21] <- m$tau2
    store[i, 22] <- mmod$I2
    store[i, 23] <- mmod$QE.LRT
    store[i, 24] <- mmod$QEp.LRT
    store[i, 25] <- mmod$tau2
    store[i, 26] <- (mmod$tau2 - m$tau2) / m$tau2 * 100 # percentage reduction in tau^2 after adding moderator
  }
  
}

datalist <- datalist[order(store[, 'PERC'])]
store <- store[order(store[, 'PERC'], decreasing = F), ]


#### FIGURE ####
pdf(file = '/Users/dweemeri/surfdrive - Weemering, D.N. (Daphne)@surfdrive.surf.nl/trial participation/trial-participation/figures/figure5.pdf',
    width = 11.7, height = 7)

par(mar = c(4.5, 34, 3, 1), tck = -0.01, xpd = T)
plot(as.numeric(store[, 'PERC']), 1:nrow(store), pch = 22, cex = 1.1, frame.plot = F, 
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
  text(x = -60, y = i+0.1, paste0(store[i, 'RR'], '/', store[i, 'TOT']), cex = 1)
  text(x = -25, y = i+0.1, paste0(format(round(as.numeric(store[i, 'PERC']), 2), nsmall = 2), ' (',
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



#### FUNNEL PLOTS ####
pdf(file = "/Users/dweemeri/surfdrive - Weemering, D.N. (Daphne)@surfdrive.surf.nl/trial participation/trial-participation/figures/suppl-fig.pdf",
    height = 8, width = 8)

# define titles and y-axis limits for each plot
pi <- list(
  list(title = "Relationship with clinical staff", ylim = c(0.5, 0), 
       ylab = "Logit transformed standard error", xlab = ""),
  list(title = "Information provided", ylim = c(1.1, 0), ylab = "", xlab = ""),
  list(title = "Placebo/sham use", ylim = c(1.5, 0), ylab = "Logit transformed standard error",
       xlab = "Log odds"),
  list(title = "Side effects", ylim = c(0.75, 0), ylab = "", xlab = "Log odds")  
)

# create plots for the four highest pooled effect sizes (where k > 2)
par(mfrow = c(2, 2))
Map(function(data, info){
  m <- rma.glmm(xi = N, ni = TOTAL, data = data, measure = 'PLO')
  funnel(m, ylim = info$ylim, xlab = info$xlab, ylab = info$ylab,
         back = "grey90", hlines = "grey", main = info$title)
}, datalist[c(15, 14, 13, 11)], pi[c(1, 2, 3, 4)])

dev.off()





