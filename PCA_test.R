library(stats)


m <- matrix(c(3, 5,1, -1, 5, NA, 5, 7, 3, 1,0 ,1 ,1 ,2, 3, -6, 7, 10, 5, 3), 10, 2)
na.exclude(m)
m[,1]

pca<-prcomp(~ m[,1] + m[,2], center = TRUE, scale = TRUE, na.action=na.exclude)
pca$x

prin_comp <- prcomp(m, center = T, scale. = T, retx = T, na.action=na.exclude)
napredict(prcomp(na.exclude(m), center = T, scale. = T, retx = T)$x, m)

?prcomp
?na.action
getOption("na.action")

pc1 <- prin_comp$x
pc1
prin_comp2$rotation

napredict(pc1, pc1)

prin_comp2 <- prcomp(na.exclude(m), center = T, scale. = T, retx = F)
scale(m, center = , scale = TRUE) %*% prin_comp2$rotation
mean(m[,1], na.rm = TRUE)
?scale


pc <- prin_comp$rotation
pc

z <- m %*% pc
mean(z$PC1)


mean(pc1[,1])
sd(pc1[,2])

cov(pc1[,1], pc1[,2])
cov(z[,1], z[,2])



biplot(prin_comp, scale = 0)

prin_comp$x

m


x1 = runif(100)
x2 = runif(100)

y = rnorm(2+3*x1+4*x2)
d = cbind(x1,x2)

pc <- prcomp(d, scale. = T, retx = T)
pc$x
mean(pc$x[,1])
sd(pc$x[,1])

   