## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ICvectorfields)
library(ggplot2)
library(ggnewscale)
library(metR)
library(terra)
library(ncf)

## ---- fig.show = 'hold'-------------------------------------------------------
# import simulated data
data(SimData, package = "ICvectorfields")

# convert to raster stack
SimStack <- ICvectorfields::RastStackData(SimData)

# confirming dimension
dim(SimStack)

# visualizing
terra::plot(SimStack[[1]], legend = FALSE, main = "t1")
terra::plot(SimStack[[2]], legend = FALSE, main = "t2")
terra::plot(SimStack[[3]], legend = FALSE, main = "t3")
terra::plot(SimStack[[4]], legend = FALSE, main = "t4")
terra::plot(SimStack[[5]], legend = FALSE, main = "t5")
terra::plot(SimStack[[6]], legend = FALSE, main = "t6")

## -----------------------------------------------------------------------------
VFdf1 <- DispField(SimStack[[1]], SimStack[[6]], factv1 = 101, facth1 = 101, restricted = TRUE)
VFdf1

## -----------------------------------------------------------------------------
VFdf2 <- DispFieldST(SimStack, lag1 = 1, factv1 = 101, facth1 = 101, restricted = TRUE)
VFdf2

## -----------------------------------------------------------------------------
DispFieldST(SimStack, lag1 = 1, factv1 = 101, facth1 = 101, restricted = FALSE)

## ---- fig.width = 7, fig.height = 5-------------------------------------------
SimVF = ggplot() +
  xlim(c(-5, 5)) +
  ylim(c(-5, 5)) +
  geom_raster(data = SimData,
              aes(x = xcoord, y = ycoord, fill = t1)) +
  scale_fill_gradient(low = "white", high = "blue", na.value = NA) +
  new_scale("fill") +
  geom_raster(data = SimData,
              aes(x = xcoord, y = ycoord, fill = t6), alpha = 0.5) +
  scale_fill_gradient(low = "white", high = "red", na.value = NA) +
  geom_vector(data = VFdf2, 
              aes(x = centx, y = centy, 
                  mag = Mag(dispx, dispy), 
                  angle = Angle(dispx, dispy))) + 
  theme_bw()
SimVF

## ---- fig.show = 'hold'-------------------------------------------------------
# import larch budmoth data from ncf package
data(lbm, package = "ncf")

# convert to raster stack
LBMStack <- ICvectorfields::RastStackData(lbm)

# confirming dimension
dim(LBMStack)

# visualizing
terra::plot(LBMStack[[1]], legend = FALSE, main = "1961")
terra::plot(LBMStack[[2]], legend = FALSE, main = "1962")
terra::plot(LBMStack[[3]], legend = FALSE, main = "1963")
terra::plot(LBMStack[[4]], legend = FALSE, main = "1964")
terra::plot(LBMStack[[5]], legend = FALSE, main = "1965")

## ---- fig.width = 7, fig.height = 5-------------------------------------------
VFdf3 <- DispFieldSTall(LBMStack[[1:23]], lagmax = 3, factv1 = 3, facth1 = 3, restricted = FALSE)

LBMVF1 = ggplot() +
  geom_raster(data = lbm,
              aes(x = x, y = y, 
                  fill = X1962)) +
  scale_fill_gradient(low = "white", high = "blue", na.value = NA) +
  new_scale("fill") +
  geom_raster(data = lbm,
              aes(x = x, y = y, fill = X1964), alpha = 0.5) +
  scale_fill_gradient(low = "white", high = "red", na.value = NA) +
  geom_vector(data = VFdf3, 
              aes(x = centx, y = centy, 
                  mag = Mag(dispx, dispy), 
                  angle = Angle(dispx, dispy))) + 
  theme_bw()
LBMVF1

## -----------------------------------------------------------------------------
VFdf3$speed <- sqrt((VFdf3$dispx^2) + VFdf3$dispy^2)

# subsetting to remove locations where speed is zero
VFdf4 <- subset(VFdf3, speed > 0)

# computing mean, standard deviation and dimension of data frame
# to obtain sample size
mean(VFdf4$speed)
sd(VFdf4$speed)
dim(VFdf4)

 # upper and lower Wald-type 95 percent confidence interval on average speed
mean(VFdf4$speed)/1000 + qt(0.975, dim(VFdf4)[1] - 1)*sd(VFdf4$speed)/1000/sqrt(dim(VFdf4)[1] - 1)
mean(VFdf4$speed)/1000 + qt(0.025, dim(VFdf4)[1] - 1)*sd(VFdf4$speed)/1000/sqrt(dim(VFdf4)[1] - 1)

## -----------------------------------------------------------------------------
# the hypotenuse of a right angled triangle whose vertical and horizontal sides are vectors of magnitude
# 176 (coresponding to average speed estimated in the previous R-chunk) has a length of
sqrt((176^2) + (176^2))
# (pythagorean theorem)

