library(MASS)
library(ggplot2)

clusterAt <- function (m = c(0,0), s = c(0.1, 0.1), n = 5, l = 1) {
  if (length(s) == 1) {
    s = c(s, s)
  }
  
  if (length(s) == 2) {
      s = matrix(c(s[1], 0, 0, s[2]), nrow = 2)
  }
  
  s = matrix(s, nrow = 2)
  
  p = mvrnorm (n, m, Sigma = s)
  xc = p[,1]
  yc = p[,2]
  lc = rep(l, n) 
  df = data.frame(x=xc, y=yc, l=lc)
}


set.seed(42)
clusters = rbind(
  clusterAt( m = c(4, 4), s = c(25.5,24.0), n = 70, l = 1),
  clusterAt( m = c(-4,-4), s = c(24.0, 23.5), n = 70, l = 0)
)

library(e1071)
C = 1
g = 0.1
dat = data.frame(y = factor(clusters$l),  clusters[,1:2])
colnames(dat) = c("l", "x", "y")
fit = svm(factor(l) ~ ., data = dat, scale = FALSE,  probability = TRUE,
	gamma = g, kernel = "radial", cost = C)


# create a (fine) grid
xmin = -15 
xmax = 15
ymin = -15
ymax = 15
gx = seq(xmin, xmax, 0.1)
gy = seq(ymin, ymax, 0.1)
datagrid  = expand.grid(x = gx, y = gy)

# predict on it and add prediction to the grid
pred = predict(fit, datagrid, decision.values = TRUE)
pred = attributes(pred)$decision.values
datagrid = cbind (datagrid, z = as.vector(pred))


# now plot data 

cols <-  colorRampPalette(c("#000099", "#00FEFF", "#009E00", 
                            "#FCFF00", "#FF9400", "#FF3100"))(256)

new_theme_empty <- theme_bw()
new_theme_empty$line <- element_blank()
new_theme_empty$rect <- element_blank()
new_theme_empty$strip.text <- element_blank()
new_theme_empty$axis.text <- element_blank()
new_theme_empty$plot.title <- element_blank()
new_theme_empty$axis.title <- element_blank()
new_theme_empty$plot.margin <- structure(c(0, 0, -1, -1), unit = "lines",
                                         valid.unit = 3L, class = "unit")
new_theme_empty$legend.position = "none"

png("decisionBoundary.png")

P = ggplot(data=clusters, aes(x=x, y=y, color=3-l)) +   
  geom_point(size = 4) + 
  scale_colour_gradientn(colours=cols) + 
  new_theme_empty + xlim(xmin, xmax) + ylim(ymin, ymax)

# add our decision boundary to it
P = P + geom_contour( data=datagrid, aes(x=x, y=y, z=z), col = "black", size = 2, breaks=c(0) )
print (P)


dev.off()
