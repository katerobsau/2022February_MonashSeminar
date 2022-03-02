# library(devtools)
# devtools::install_github(repo = "kcf-jackson/animate")
library(animate)

sigma <- 10
beta <- 8 / 3
rho <- 28
x <- y <- z <- 1
xs <- x
ys <- y
zs <- z
dt <- 0.015

device <- animate$new(width = 600, height = 400)  
attach(device)

for (i in 1:2000) {
  # Euler's method
  dx <- sigma * (y - x) * dt
  dy <- (x * (rho - z) - y) * dt
  dz <- (x * y - beta * z) * dt
  x <- x + dx
  y <- y + dy
  z <- z + dz
  # Keep a record for line trace plot
  xs <- c(xs, x)
  ys <- c(ys, y)
  zs <- c(zs, z)
  # Plot the x-y solution plane
  plot(x, y, id = "ID-1", xlim = c(-30, 30), ylim = c(-30, 40)) #<<
  lines(xs, ys, id = "lines-1", xlim = c(-30, 30), ylim = c(-30, 40)) #<<
  Sys.sleep(0.025)
}
clear()
off()

