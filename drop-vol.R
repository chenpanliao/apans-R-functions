b <- 2; h <- 5; x <- seq(0,b,0.001)
yc <- h * (1 - x/b)^2
yc <- h * (1 - x/b)^2 - h
plot(yc ~ x, type="l", xlab="")








library(plot3D)
M <- mesh(seq(0, 6*pi, length.out = 80),seq(pi/3, pi, length.out = 80))
u <-M$x;v<-M$y
x <- u/2 * sin(v) * cos(u)
y <- u/2 * sin(v) * sin(u)
z <- u/2 * cos(v)
surf3D(x, y, z, colvar = z, colkey = FALSE, box = FALSE)


x <- y <- z <- seq(-4,4,by=0.2)
M <- mesh(x, y, z)
R <- with(M, sqrt(x^2 + y^2 + z^2))
p <- sin(2*R)/(R+1e-3)
slice3D(x, y, z, colvar = p, xs = 0, ys = c(-4, 0, 4), zs = NULL)
isosurf3D(x, y, z, colvar = p, level = 0, col = "red")

