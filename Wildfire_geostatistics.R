library(dplyr)
library(geoR)
library(maps)
library(gstat)
library(sp)
library(scatterplot3d)

# Read in the data
fire <- read.csv("FW_Veg_Rem_Combined.csv")

# Only fires in CA, select response and predictors
fire <- fire %>% filter(state == "CA") %>% select(longitude, latitude, fire_size, Temp_pre_7, Hum_pre_7)

# Select only fires with valid temperature and humidity
fire <- fire %>% filter(Temp_pre_7 > 0 & Hum_pre_7 > 0)

# Use only a sample of the total data set and remove duplicate points
set.seed(1208)
idx <- sample(1:2468, 1000)
fire_dat <- fire[idx,]
fire_dat <- fire_dat[!duplicated(fire_dat$latitude),]
fire_dat <- fire_dat[!duplicated(fire_dat$longitude),]

# Plot the location of the fires with the size of the point corresponding to the size of the fire
map("state", "ca")
points(fire_dat$longitude, fire_dat$latitude, cex = log(fire_dat$fire_size)/ 15)


# Histograms
par(mfrow=c(2,2))
hist(fire_dat$fire_size, main = "Histogram of Fire Size", xlab = "Fire Size")
hist(log(fire_dat$fire_size), main = "Histogram of Log(Fire Size)", xlab = "Log of Fire Size")
hist(fire_dat$Temp_pre_7, main = "Temperature 7 Days Before Fire ", xlab = "Temperature (Celsius)")
hist(fire_dat$Hum_pre_7, main = "Humidity 7 Days Before Fire", xlab = "Humidity (Percentage)")

# Box plots
par(mfrow = c(2,2))
boxplot(log(fire_dat$fire_size), main = "Log of Fire Size")
boxplot(fire_dat$Temp_pre_7, main = "Temperature")
boxplot(fire_dat$Hum_pre_7, main = "Humidity")

# ecdf
par(mfrow = c(2,2))
plot(ecdf(log(fire_dat$fire_size)), main = "ECDF of Log(Fire Size)")
plot(ecdf(fire_dat$Temp_pre_7), main = "ECDF of Temperature")
plot(ecdf(fire_dat$Hum_pre_7), main = "ECDF of Humidity")

# looking for trends visually
par(mfrow = c(2,2))
plot(fire_dat$longitude, fire_dat$latitude, cex = log(fire_dat$fire_size)/10, main = "Log(Fire Size)",
     xlab = "Longitude", ylab = "Latitude")
plot(fire_dat$longitude, fire_dat$latitude, cex = fire_dat$Temp_pre_7/40, main = "Temperature",
     xlab = "Longitude", ylab = "Latitude")
plot(fire_dat$longitude, fire_dat$latitude, cex = fire_dat$Hum_pre_7/100, main = "Humidity",
     xlab = "Longitude", ylab = "Latitude")

# Create a geodata object
new_dat <- fire_dat[,c(1,2,3)]
new_dat$fire_size <- log(new_dat$fire_size)
geo_dat <- as.geodata(new_dat)

# Create a gstat object to plot variogram
g_test <- gstat(id="fire_size", formula = log(fire_size)~1, locations = ~longitude+latitude, 
                data = fire_dat)

# h-scatterplots
set.seed(1)
idx <- sample(1:nrow(fire_dat), 200)
fire_hscat <- as.data.frame(cbind(fire_dat$longitude[idx], fire_dat$latitude[idx], 
                                  log(fire_dat$fire_size[idx])))
names(fire_hscat) <- c("x", "y", "data")

coordinates(fire_hscat) <- ~x+y

hscat(data~1, data = fire_hscat, seq(0, 12, by = 2))

# rose diagram

#Compute the variogram for the following directions:
var1 <- variog(geo_dat, dir=pi/2, tol=pi/4, estimator.type="modulus")
var2 <- variog(geo_dat, dir=pi/2.57, tol=pi/4, estimator.type="modulus")
var3 <- variog(geo_dat, dir=pi/3.6, tol=pi/4, estimator.type="modulus")
var4 <- variog(geo_dat, dir=pi/6, tol=pi/4, estimator.type="modulus")
var5 <- variog(geo_dat, dir=pi/18, tol=pi/4, estimator.type="modulus")
var6 <- variog(geo_dat, dir=0.944*pi, tol=pi/4, estimator.type="modulus")
var7 <- variog(geo_dat, dir=0.833*pi, tol=pi/4, estimator.type="modulus")
var8 <- variog(geo_dat, dir=0.722*pi, tol=pi/4, estimator.type="modulus")
var9 <- variog(geo_dat, dir=0.611*pi, tol=pi/4, estimator.type="modulus")

theta <- c(0, pi/9, pi/4.5, pi/3, pi/2.25, pi/18, pi/6, pi/3.6, pi/2.571)
range <- c(1.9, 3, 2.5, 2, 3, 2.5, 2.5, 2.5, 2)

x1 <- cos(theta[1:5])*range[1:5]
y1 <- sin(theta[1:5])*range[1:5]

x2 <- range[6:9]*sin(theta[6:9]) 
y2 <- -range[6:9]*cos(theta[6:9])

x11 <- -x1
y11 <- -y1

x22 <- -x2
y22 <- -y2

plot(x1,y1, xlim = c(-5,5), ylim = c(-5, 5), xaxt="n", yaxt="n", 
     ylab="y", xlab="x", main = "Rose Diagram")
points(x11,y11)
points(x2,y2)
points(x22,y22)

segments(x1,y1, x11, y11)
segments(x2,y2, x22, y22)

segments(0, -34.8, 0, 34.8, lty=2)
segments(-28, 0, 28, 0, lty=2)

# looking for trend/directional trend
scatterplot3d(new_dat$longitude,new_dat$latitude,new_dat$fire_size, xlab="West to East", 
              ylab="South to North", zlab="Log(Fire Size)")
dir_plot <- variog4(geo_dat)
plot(dir_plot)

# Classical and Robust Estimators
cloud1 <- variog(geo_dat, bin.cloud=T)
cloud2 <- variog(geo_dat, bin.cloud=T, estimator.type="modulus")

par(mfrow=c(1,2))
plot(cloud1, bin.cloud=T, main = "Classical Estimator")
plot(cloud2, bin.cloud=T, main = "Robust Estimator")

# Fit model to sample variogram
sample_variogram <- variog(geo_dat, estimator.type = "modulus")

fit_d <- variofit(sample_variogram, cov.model="sph", ini.cov.pars=c(8, 3), 
                  fix.nugget=FALSE, nugget=4)
fit_e <- variofit(sample_variogram, cov.model="sph", weights="cressie", ini.cov.pars=c(8, 3), 
                  fix.nugget=FALSE, nugget=4)
fit_f <- variofit(sample_variogram, cov.model="sph", weights="equal", ini.cov.pars=c(8, 3), 
                  fix.nugget=FALSE, nugget=4)

fit_d_exp <- variofit(sample_variogram, cov.model="exp", ini.cov.pars=c(8, 3), 
                      fix.nugget=FALSE, nugget=4)
fit_e_exp <- variofit(sample_variogram, cov.model="exp", weights="cressie", ini.cov.pars=c(8, 3), 
                      fix.nugget=FALSE, nugget=4)
fit_f_exp <- variofit(sample_variogram, cov.model="exp", weights="equal", ini.cov.pars=c(8, 3), 
                      fix.nugget=FALSE, nugget=4)

plot(sample_variogram, main = "Semivariogram with Spherical Models")
lines(fit_d, lty=1, col = "red")
lines(fit_e, lty=1, col="green")
lines(fit_f, lty=1, col="orange")
legend(8, 3.5, legend = c("Default Weights", "Cressie's Weights", "OLS Weights"), fill = c("red", "green", "orange"))

plot(sample_variogram, main = "Semivariogram with Exponential Models")
lines(fit_d_exp, lty=1, col = "red")
lines(fit_e_exp, lty=1, col="green")
lines(fit_f_exp, lty=1, col="orange")
legend(8, 3.5, legend = c("Default Weights", "Cressie's Weights", "OLS Weights"), fill = c("red", "green", "orange"))

### Ordinary Kriging

#Create the grid for spatial predictions:
x.range <- as.integer(range(fire_dat[,1]))
y.range <- as.integer(range(fire_dat[,2]))
grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=.05),
                   y=seq(from=y.range[1], to=y.range[2], by=.05))
names(fire_dat)[1] <- "x"
names(fire_dat)[2] <- "y"
# ordinary kriging
pr_ok <- krige(id="log(fire_size)", log(fire_size)~1, locations=~x+y, 
               model=vgm(4.7034, "Exp", 2.6253, 2.7924),
               data=fire_dat, newdata=grd)
range(pr_ok$`log(fire_size).pred`) # for contours

#We want to assign "NA" values for the region outside the observed data
#points:
X <- seq(from=x.range[1], to=x.range[2], by=.05)
Y <- seq(from=y.range[1], to=y.range[2], by=.05)
dz.mask <- function(grid, pts, x.dom, y.dom, window, mitre=2) {
  N <- length(pts[ ,1]) ; mask <- array( NA, dim(grid) )
  for(j in 1:N) {
    dx <- abs(x.dom - pts$x[j])
    dy <- abs(y.dom - pts$y[j])
    d.Mx <- tcrossprod( dx , rep(1, length(dy)) )^mitre +
      tcrossprod( rep(1, length(dx)), dy )^mitre
    mask[ d.Mx < window^mitre ] <- FALSE
  }
  return(mask+grid)
}

qqq.masked <- dz.mask(qqq, fire_dat, X, Y, 1)

image(X,Y,qqq.masked, xlab="West to East",ylab="South to North", main="Raster Map - Ordinary Kriging")
points(fire_dat$x, fire_dat$y, cex = .3*log(fire_dat$fire_size)/mean(log(fire_dat$fire_size)))
contour(seq(from=x.range[1], to=x.range[2], by=.05), 
        seq(from=y.range[1],to=y.range[2], by=.05), qqq.masked, add=TRUE, col="black", 
        levels=seq(0, 8, by=2), labcex=1)

#Collapse the variances into a matrix:
qqq1 <- matrix(pr_ok$`log(fire_size).var`, length(seq(from=x.range[1], to=x.range[2], by=.05)),
               length(seq(from=y.range[1], to=y.range[2], by=.05)))

qqq1.masked <- dz.mask(qqq1, fire_dat, X, Y, 1)

#Construct the raster map of the variances:
image(seq(from=x.range[1], to=x.range[2], by=.05),
      seq(from=y.range[1],to=y.range[2], by=.05), qqq1.masked,
      xlab="West to East",ylab="South to North", main="Raster Map - Variances")
contour(seq(from=x.range[1], to=x.range[2], by=.05), 
        seq(from=y.range[1],to=y.range[2], by=.05), qqq1.masked, add=TRUE, col="black", 
        levels=seq(2, 9, by=1), labcex=1)



### cokriging
v.fit <- fit.variogram(variogram(g_test), vgm(4, "Sph", 1, 6))

#Begin with the target variable log fire_size:
g1 <- gstat(id="log_fire_size", formula = log(fire_size)~1, locations = ~x+y, data = fire_dat)

#Append Temp:
g1 <- gstat(g1,id="temp", formula = Temp_pre_7~1, locations = ~x+y, data = fire_dat)
  
#Append Humidity:
g1 <- gstat(g1,id="hum", formula = Hum_pre_7~1, locations = ~x+y, data = fire_dat)


plot(variogram(g1))

#Fit a model variogram to all the variograms:
vm <- variogram(g1) 
vm.fit <- fit.lmc(vm, g1, model=v.fit)

#Plot the fitted variograms to all the sample variograms:
plot(vm,vm.fit)

#Cokriging predictions:
#Create the grid for predictions:
x.range <- as.integer(range(fire_dat[,1])) 
y.range <- as.integer(range(fire_dat[,2])) 
grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=.05), 
                   y=seq(from=y.range[1], to=y.range[2], by=.05)) 

#Perform co-kriging predictions:
ck <- predict(vm.fit, grd)

#Plots:
#Collapse the predicted values into a matrix:
qqq_ck <- matrix(ck$log_fire_size.pred,
              length(seq(from=x.range[1], to=x.range[2], by=.05)),
              length(seq(from=y.range[1], to=y.range[2], by=.05)))

image(seq(from=x.range[1], to=x.range[2], by=.05),
      seq(from=y.range[1], to=y.range[2], by=.05), qqq_ck)



#We want to assign "NA" values for the region outside the observed data
#points:
X <- seq(from=x.range[1], to=x.range[2], by=.05)
Y <- seq(from=y.range[1], to=y.range[2], by=.05)
dz.mask <- function(grid, pts, x.dom, y.dom, window, mitre=2) {
  N <- length(pts[ ,1]) ; mask <- array( NA, dim(grid) )
  for(j in 1:N) {
    dx <- abs(x.dom - pts$x[j])
    dy <- abs(y.dom - pts$y[j])
    d.Mx <- tcrossprod( dx , rep(1, length(dy)) )^mitre +
      tcrossprod( rep(1, length(dx)), dy )^mitre
    mask[ d.Mx < window^mitre ] <- FALSE
  }
  return(mask+grid)
}
qqq.masked_ck <- dz.mask(qqq_ck, fire_dat, X, Y, 1)

image(X,Y,qqq.masked_ck, xlab="West to East",ylab="South to North", main="Raster Map - Co-Kriging")
points(fire_dat$x, fire_dat$y, cex = .3*log(fire_dat$fire_size)/mean(log(fire_dat$fire_size)))
contour(seq(from=x.range[1], to=x.range[2], by=.05), 
        seq(from=y.range[1],to=y.range[2], by=.05), qqq.masked_ck, add=TRUE, col="black", 
        levels=seq(0, 9, by=3), labcex=1)



# compare using cross validation
cv_ck <- gstat.cv(vm.fit) #takes a while to run

sum(cv_ck$residual^2)/nrow(fire_dat)

cv_ok <- krige.cv(log(fire_size)~1, locations=~x+y, 
               model=vgm(4.7034, "Exp", 2.6253, 2.7924),
               data=fire_dat, nfold=nrow(fire_dat))

sum(cv_ok$residual^2)/nrow(fire_dat)

plot(cv_ok$var1.pred,cv_ok$observed,
     xlab="Observed values", ylab="Predicted values",
     main = "Predicted Against True Values")


cv_ok_sph <- krige.cv(log(fire_size)~1, locations=~x+y, 
                  model=vgm(5.3586, "Sph", 2.0436, 10.7177),
                  data=fire_dat, nfold=nrow(fire_dat))

sum(cv_ok_sph$residual^2)/nrow(fire_dat)

plot(cv_ok_sph$var1.pred,cv_ok_sph$observed,
     xlab="Observed values", ylab="Predicted values",
     main = "Predicted Against True Values")


cv_ok_sph_cress <- krige.cv(log(fire_size)~1, locations=~x+y, 
                      model=vgm(4.4219, "Sph", 2.3635, 3.1867),
                      data=fire_dat, nfold=nrow(fire_dat))

sum(cv_ok_sph_cress$residual^2)/nrow(fire_dat)

##### Point Pattern data

library(splancs)

pt_pat_dat <- fire_dat[,1:2]

npts <- nrow(pt_pat_dat)
aa <- as.matrix(pt_pat_dat)

lambda <- npts / ((124.3-114.5)*(42.1-32.4))

#Create the square and add the locations of the longleaf trees:
poly <- matrix(c(-124.3,32.4,-114.5,32.4,-114.5,42.1,-124.3,42.1),4,2,T)
polymap(poly)
pointmap(aa,pch=19,add=T)

#Compute the nearest distance from each longleaf tree:
d <- nndistG(aa)$dists


#
# Monte Carlo test
#
#Test based on the average nearest neighbor distances (NND):
#Compute the average NND from the actual data set:
davg <- mean(d)

#Construct the distribution of NND assuming a complete spatial  randomness (CSR).  This means that we will randomly select many samples of size 584 in the square and for each sample we will compute the average NND:

nsim <- 150
simul_davg <- rep(0, nsim)


for (isim in 1:nsim) {
  xy <- csr(poly,npts)
  di <- nndistG(xy)$dists
  simul_davg[isim] <- mean(di)
}


#Construct the historgram using the average NND from the 99 simulated data sets:
hist(simul_davg, xlim = c(.07, .18))

#Place the actual average NND on the histogram to compare:

segments(davg ,0, davg ,30, col="green")

#Compute p-value:
sum(simul_davg < davg)/150


########### now separate CA in three sections

north_dat <- fire_dat %>% filter(y<42.1 & y>39.1)

npts <- nrow(north_dat)
aa <- as.matrix(north_dat)

lambda <- npts / ((124.3-119.5)*(42.1-39.1))

#Create the square and add the locations of the longleaf trees:
poly <- matrix(c(-124.3,39.1,-119.5,39.1,-119.5,42.1,-124.3,42.1),4,2,T)
polymap(poly)
pointmap(aa,pch=19,add=T)
title(main = "Wildfires in Northern California", xlab = "Longitude", ylab = "Latitude")

#Compute the nearest distance from each longleaf tree:
d <- nndistG(aa)$dists


#
# Monte Carlo test
#
#Test based on the average nearest neighbor distances (NND):
#Compute the average NND from the actual data set:
davg <- mean(d)

#Construct the distribution of NND assuming a complete spatial  randomness (CSR).  This means that we will randomly select many samples of size 584 in the square and for each sample we will compute the average NND:

nsim <- 99
simul_davg <- rep(0, nsim)


for (isim in 1:nsim) {
  xy <- csr(poly,npts)
  di <- nndistG(xy)$dists
  simul_davg[isim] <- mean(di)
}


#Construct the historgram using the average NND from the 99 simulated data sets:
hist(simul_davg,  xlim = c(.07, .14), main = "Histogram of Simulations")

#Place the actual average NND on the histogram to compare:

segments(davg ,0, davg ,30, col="green")

#Compute p-value:
sum(simul_davg < davg)/99


## Central CA

central_dat <- fire_dat %>% filter(y>36.1 & y<=39.1)

npts <- nrow(central_dat)
aa <- as.matrix(central_dat)

#Create the square and add the locations of the longleaf trees:
poly <- matrix(c(-123.5,36.1,-118,36.1,-118,39.1,-123.5,39.1),4,2,T)
polymap(poly)
pointmap(aa,pch=19,add=T)
title(main = "Wildfires in Central California", xlab = "Longitude", ylab = "Latitude")

#Compute the nearest distance from each longleaf tree:
d <- nndistG(aa)$dists


#
# Monte Carlo test
#
#Test based on the average nearest neighbor distances (NND):
#Compute the average NND from the actual data set:
davg <- mean(d)

#Construct the distribution of NND assuming a complete spatial  randomness (CSR).  This means that we will randomly select many samples of size 584 in the square and for each sample we will compute the average NND:

nsim <- 99
simul_davg <- rep(0, nsim)


for (isim in 1:nsim) {
  xy <- csr(poly,npts)
  di <- nndistG(xy)$dists
  simul_davg[isim] <- mean(di)
}


#Construct the historgram using the average NND from the 99 simulated data sets:
hist(simul_davg,  xlim = c(.05, .14))

#Place the actual average NND on the histogram to compare:

segments(davg ,0, davg ,30, col="green")

#Compute p-value:
sum(simul_davg < davg)/99


## Southern CA

southern_dat <- fire_dat %>% filter(y<=36.1)

npts <- nrow(southern_dat)
aa <- as.matrix(southern_dat)

#Create the square and add the locations of the longleaf trees:
poly <- matrix(c(-121.5,32.4,-115,32.4,-115,36.1,-121.5,36.1),4,2,T)
polymap(poly)
pointmap(aa,pch=19,add=T)
title(main = "Wildfires in Central California", xlab = "Longitude", ylab = "Latitude")

#Compute the nearest distance from each longleaf tree:
d <- nndistG(aa)$dists


#
# Monte Carlo test
#
#Test based on the average nearest neighbor distances (NND):
#Compute the average NND from the actual data set:
davg <- mean(d)

#Construct the distribution of NND assuming a complete spatial  randomness (CSR).  This means that we will randomly select many samples of size 584 in the square and for each sample we will compute the average NND:

nsim <- 99
simul_davg <- rep(0, nsim)


for (isim in 1:nsim) {
  xy <- csr(poly,npts)
  di <- nndistG(xy)$dists
  simul_davg[isim] <- mean(di)
}


#Construct the historgram using the average NND from the 99 simulated data sets:
hist(simul_davg)

#Place the actual average NND on the histogram to compare:

segments(davg ,0, davg ,30, col="green")

#Compute p-value:
sum(simul_davg < davg)/99




