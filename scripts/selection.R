set.seed(97)

generate_data <- function(size, prob_x, prob_y) {
  x <- sample(c("A", "B", "C"), size, prob = prob_x, replace = TRUE)
  D <- data.frame(X = rnorm(size, mean = 0, sd = 1.75), 
                  Y = rnorm(size, mean = 0, sd = 0.95), 
                  ID = 1:size)
  D <- D[sample(D$ID, size = size), ]
  return(list(x = x, D = D))
}

dl <- list(
  generate_data(500, prob_x = c(0.8/3, 2/3, 1/3)),
  generate_data(150, prob_x = c(1/6, 1/6, 1/6)),
  generate_data(35, prob_x = c(1.4/6, 1.3/6, 0.2/6))
)

# make figure
pdf(file = "/Users/daphneweemering/surfdrive/trial participation/trial-participation/figures/figure4.pdf", height = 5, width = 7)

# population
par(mar = c(0, 0.5, 1, 0), mfrow = c(2, 4), fig = c(0, 1/3, 0.2, 1), new = F)
plot (dl[[1]]$D$X, dl[[1]]$D$Y, col = paste(factor(dl[[1]]$x, labels = c("#001d76", "#4169E1", "#3daaf0"))), 
      pch = 14 + as.numeric(factor(dl[[1]]$x)), ylim = c(-3.5, 3.5), xlim = c(-6, 6), 
      xaxt = "n", yaxt = "n", bty = "n", cex = 1.75)
mtext("General population", line = -1.5, adj = 0.55)

par(fig = c(0, 2/3, 0.2, 1))
arrows(x0 = 0, y0 = 0, x1 = 0.6, y1 = 0, length = 0.1, angle = 30, code = 2)

# eligible patients
par(mar = c(0, 0.5, 1, 0), mfrow = c(2, 3), fig = c(1/3, 2/3, 0.2, 1), new = T)
plot (dl[[2]]$D$X, dl[[2]]$D$Y, col = paste(factor(dl[[2]]$x, labels = c("#001d76", "#4169E1", "#3daaf0"))), 
      pch = 14 + as.numeric(factor(dl[[2]]$x)), ylim = c(-3.5, 3.5), xlim = c(-6, 6), 
      xaxt = "n", yaxt = "n", bty = "n", cex = 1.75)
mtext("Eligible population", line = -1.5, adj = 0.55)
abline (v = -6, lty = 3)


# trial patients
par(mar = c(0, 0.5, 1, 0), mfrow = c(2, 3), fig = c(2/3, 1, 0.2, 1), new = T)
plot (dl[[3]]$D$X, dl[[3]]$D$Y, col = paste(factor(dl[[3]]$x, labels = c("#001d76", "#4169E1", "#3daaf0"))),
      pch = 14 + as.numeric(factor(dl[[3]]$x)), ylim = c(-3.5, 3.5), xlim = c(-6, 6),
      xaxt = "n", yaxt = "n", bty = "n", cex = 1.75)
mtext("Trial population", line = -1.5, adj = 0.55)
abline (v = -6, lty = 3)

par(fig = c(1/3, 1, 0.2, 1))
arrows(x0 = -0.2, y0 = 0, x1 = 0.6, y1 = 0, length = 0.1, angle = 30, code = 2)

# selection text
par(mar = c (0, 0.5, 1, 0), fig = c(0, 1, 0, 1), new = T)
legend(x = -3.65, y = -2.325, legend = bquote(bold("Eligibility selection      ")), bg = "grey98", 
       adj = 0, text.col = "black", box.col = "grey98", cex = 1.1, xpd = T)
legend(x = 1, y = -2.325, legend = bquote(bold("Latent selection      ")), bg = "grey98",
       adj = 0, text.col = "black", box.col = "grey98", cex = 1.1, xpd = T)


# barplots
par(mar = c(1.5, 2, 0.7, 0.5), mfrow = c(2, 4), fig = c(0, 1/3, 0, 0.1), new = T)
barplot(as.matrix(prop.table(table(dl[[1]]$x))), col = c("#001d76", "#4169E1", "#3daaf0"), 
        horiz = T, xaxt = "n", border = NA)

lttr <- function(x, abc){
  text(x = x, y = 0.7, abc, col = "white")
}

mat <- as.matrix(prop.table(table(dl[[1]]$x)))
result <- c(mat[1]/2, mat[1] + mat[2]/2, mat[1] + mat[2] + mat[3]/2)
lttr(result, c("A", "B", "C"))

par(mar = c(1.5, 2, 0.7, 0.5), mfrow = c(2, 4), fig = c(1/3, 2/3, 0, 0.1), new = T)
barplot(as.matrix(prop.table(table(dl[[2]]$x))), col = c("#001d76", "#4169E1", "#3daaf0"), 
        horiz = T, xaxt = "n", border = NA)

mat <- as.matrix(prop.table(table(dl[[2]]$x)))
result <- c(mat[1]/2, mat[1] + mat[2]/2, mat[1] + mat[2] + mat[3]/2)
lttr(result, c("A", "B", "C"))

par(mar = c(1.5, 2, 0.7, 1), mfrow = c(2, 4), fig = c(2/3, 1, 0, 0.1), new = T)
barplot(as.matrix(prop.table(table(dl[[3]]$x))), col = c("#001d76", "#4169E1", "#3daaf0"), 
        horiz = T, xaxt = "n", border = NA)

mat <- as.matrix(prop.table(table(dl[[3]]$x)))
result <- c(mat[1]/2, mat[1] + mat[2]/2, mat[1] + mat[2] + mat[3]/2)
lttr(result, c("A", "B", "C"))

dev.off()

