install.packages("matlib")
library(matlib)


n <- 12

e <- rnorm(n)
x <- c(3, 5, 11 ,2, 7, 8, 1, 7, 0, 3, 5, 5)
y <- c(1, -5, 4, 3, 2, 1, 1, 1, 0, 5, 3, 1)

data <- as.data.frame(cbind(e,x,y))

mean((x - mean(x))*(y-mean(y)))

z <- y - x*(mean((x - mean(x))*(y-mean(y))) / mean((x - mean(x))*(x-mean(x))))

data$z <- z

summary(lm(e ~ x + y, data = data))
summary(lm(e ~ y , data = data))
summary(lm(e ~ x + z , data = data))
summary(lm(e ~ x, data = data))
summary(lm(e ~ z, data = data))

mean((x - mean(x))*(z-mean(z)))

z <- GramSchmidt(x, normalize=FALSE)

cor(z[,1], z[,2])
cov(z[,1], z[,2])

y <- matrix(c( 1, -1, 0, 0), 2, 2)
z <- GramSchmidt(y, normalize=FALSE)
cov[]

y <- x[,1] - x[,1] * (cov(x[,1],x[,2])/cov(x[,1],x[,1]))

x <- x[,1]
cov(x, y)


cov(x[,1],x[,2])


x