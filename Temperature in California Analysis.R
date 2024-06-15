library(sp)
library(phylin)
library(tidyverse)
library(viridis)
air <- read.csv("airqual.csv")
q3 <- as.data.frame(rowMeans(air[,c('JUL',"AUG","SEP")]))
q3df <- as.data.frame(cbind(air$NAME,air$LONG,air$LAT,q3))
colnames(q3df) <- c("name","lon","lat","q3")
hist(q3df$q3, main='Histogram of Temperature (quarter3)', xlab='Temperature(quarter3)',
     col = 'light blue')

boxplot(q3df$q3, horizontal = TRUE)
outliers <- as.data.frame(boxplot.stats(q3df$q3)$out)
#Removing the distributionl outlie
q3df <- filter(q3df , q3df$name != "CORONA")# station with temp=61.66

hist(q3df$q3, main='Histogram of Temperature (quarter3)', xlab='Temperature(quarter3)',
     col = 'light blue')

#Normality
hist(sqrt(q3df$q3),main='Histogram of SQRT(Temperature (quarter3))', xlab='SQRT(Temperature (quarter3)',
     ,col = 'light blue')
shapiro.test(sqrt(q3df$q3))


# Exploration
##bubble Plot
radius <- sqrt(q3df$q3/ pi )
symbols(q3df$lon, q3df$lat, circles=radius, inches=0.25, fg="white", bg="red",
        xlab = "Longitude",ylab = "Latitude" ,
        main = "Bubble Plot for Temperature (Quarter 3)")
## Spplot
df <- q3df
coordinates(df) <- c("lon","lat")
spplot(df, "q3", colorkey = TRUE, xlab= "Longitude", ylab= "Latitude",
       edge.col = "black",scales = list(draw = TRUE))

ggplot(q3df, aes(lon, lat))+
  geom_point(aes(color =q3),size= 3) +
  scale_color_viridis(option = "C")+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")
#################################################################
air<- as.data.frame(q3df)
colnames(air) <- c("Name","LAT","LONG","Q3")
#Desriptive Measures
#moran's I
air_q3.dists<-as.matrix(dist(cbind(air$LONG,air$LAT)))
coor_df <- as.data.frame(cbind(air$LAT,air$LONG))
library(codep) 
gcd1<-gcd.slc(coor_df, radius = 6371)
gcd1inv<-1/gcd1

gcd1inv<-as.matrix(gcd1inv)
library(ape)
Moran.I(air$Q3,gcd1inv)

# making spatial dataframe
library(sp)
coords <- air[ , c("LONG", "LAT")]   # coordinates
data   <- as.data.frame(air[ , c("Q3")] )         # data
class(data)
crs    <- CRS("+init=epsg:28992") # proj4string of coords

# make the SpatialPointsDataFrame object
##library(sp)
spdf <- SpatialPointsDataFrame(coords      = coords,
                               data        = data, 
                               proj4string = crs)
summary(spdf)
##### Moran-I plot 
library(spatialEco)
library(spdep)
knn<-knearneigh(spdf, k=3, longlat = NULL)
knn2nb<-knn2nb(knn)
mp <- moran.plot(air$Q3, nb2listw(knn2nb))
local <- localmoran(x = air$Q3, listw = nb2listw(knn2nb))
moran.map <- cbind(air, local)

##### localized 
library(lctools)
l.moran<-l.moransI(air_q3.dists,6,air$Q3, WType='Bi-square', scatter.plot = TRUE, family = "adaptive")
l.moran
####### lisa plot
library(ncf)
lisa <- lisa(air$LONG, air$LAT, air$Q3, neigh = 3, latlon=TRUE)
lisa
plot(lisa)

###Scatter 3D
library("plot3Drgl")
scatter3D(air$LONG, air$LAT,air$Q3, zcol=air$Q3,pty="g",ticktype="detailed")

plotrgl()

##fitting a trend surface model by least squares
library(spatial)
x<-air$LONG
y<-air$LAT
z<-air$Q3

fit.sfc3 <- surf.ls(3,x,y,z)
summary(fit.sfc3)
fit.sfc3$beta

#evaluate trend surface over a grid
trsurf3 <- trmat(fit.sfc3, min(x), max(x), min(y), max(y), 50)

trsurf3

scatter3D(x,y,z,surf = list(x = trsurf3$x, y = trsurf3$y, z = trsurf3$z,
                            NAcol = "grey", shade = 0.1))

plotrgl()
###################

fit.sfc1 <- surf.ls(1,x,y,z)
summary(fit.sfc1)
fit.sfc1$beta

trsurf1 <- trmat(fit.sfc1, min(x), max(x), min(y), max(y), 50)

trsurf1

scatter3D(x,y,z,surf = list(x = trsurf1$x, y = trsurf1$y, z = trsurf1$z,
                            NAcol = "grey", shade = 0.1))

##############
fit.sfc2 <- surf.ls(2,x,y,z)
summary(fit.sfc2)
fit.sfc2$beta

trsurf2 <- trmat(fit.sfc2, min(x), max(x), min(y), max(y), 50)

trsurf2

scatter3D(x,y,z,surf = list(x = trsurf2$x, y = trsurf2$y, z = trsurf2$z,
                            NAcol = "grey", shade = 0.1))
####################
library(plot3Drgl)

contour(trsurf3)
############################################################
q3df<- as.data.frame(air)
colnames(q3df) <- c("name","lon","lat","q3")

#IDW

coords <- q3df[ , c("lon", "lat")]

data1   <- q3df[ , c("q3")]
data   <- as.data.frame(q3df[ , c("q3")])          # data
crs    <- CRS("+init=epsg:28992") # proj4string of coords

# make the SpatialPointsDataFrame object
spdf <- SpatialPointsDataFrame(coords      = coords,
                               data        = data, 
                               proj4string = crs)
summary(spdf)
Long <- seq(from=-125,to=-113,by=0.1)
Lat <- seq(from=32,to=43,by=0.1)
grid<-cbind(rep(Long,length(Lat)), rep(Lat,each=length(Long)))

idw<- phylin:: idw(data1, coords, grid,p =3.951)
grid.image(idw, grid, main='IDW interpolation at P = 3.951', xlab='Longitude', 
           ylab='Latitude')
points(coords, cex=data1/6)

# for getting best p
library(spatstat)
my_owin <- owin(c(-125,-113),c(32,43))
ppp_air <- ppp(x= q3df$lon,y = q3df$lat,marks = q3df$q3, window = my_owin)
idw_air <- idw(ppp_air,power= 2,at= "pixels")
idw_points <- idw(ppp_air,power = 2, at ="points")
plot(idw_air,col= heat.colors(20))
Metrics::mse(ppp_air$marks,idw_points)

powers <- seq(0.001, 10, 0.01)
mse_result <- NULL
for(power in powers){
  CV_idw <- idw(ppp_air, power=power, at="points")
  mse_result <- c(mse_result,
                  Metrics::mse(ppp_air$marks,CV_idw))
}
optimal_power <- powers[which.min(mse_result)]
optimal_power
plot(powers, mse_result)



#####Kriging
colnames(air) <- c("Name","LAT","LONG","q3")

###Variogram (empirical)
library(gstat)
##plot
evgm <- gstat::variogram( air$q3~1,spdf)
plot(evgm, main='Semivariogram vs. distance ', ylab='semivariogram')

##fitting
fvgm1 <- fit.variogram(evgm,vgm("Sph"))#spherical
fvgm1

fvgm2 <- fit.variogram(evgm,vgm("Gau")) #Gaussian
fvgm2

fvgm3 <- fit.variogram(evgm,vgm("Exp")) #Exponential
fvgm3

#checking errors
plot(evgm,model=fvgm1,main='Empirical vs Spherical semivariogram')
plot(evgm,model=fvgm2,main='Empirical vs Gaussian semivariogram')

plot(evgm, main='Semivariogram vs. distance ', ylab='semivariogram')

plot(evgm,model=fvgm3,main='Empirical vs Exponential semivariogram')

attr(fvgm1, "SSErr")
attr(fvgm2, "SSErr")
attr(fvgm3, "SSErr")# the best one

# fitting ordinary kriging on a grid 
s.grid <- spsample(spdf, type = "regular", n = 8000)

krig.est <- krige(air$q3~1, spdf, newdata = s.grid, model = fvgm3)


#plotting using spplot
spplot(krig.est['var1.pred'], main='Kriging prediction')

spplot(krig.est['var1.var'], main='Kriging prediction error')

points(coords)

krig.grid <- SpatialPixelsDataFrame(krig.est, krig.est@data)
levs <- c(0, 5, 10, 15,20,Inf)
var.levs <- c(0, 10, 20,30 , 40,50, Inf)

##plot using tmap
library(tmap)
krig.map.est <- tm_shape(krig.grid) +
  tm_raster(col = 'var1.pred', breaks = levs, title = 'Tempreture', palette = 'Reds') +
  tm_layout(legend.bg.color = 'white', legend.frame = TRUE)

krig.map.var <- tm_shape(krig.grid) +
  tm_raster(col = 'var1.var', breaks = var.levs, title = 'Estimate Variance', palette = 'Reds') +
  tm_layout(legend.bg.color = 'white', legend.frame = TRUE)

tmap_arrange(krig.map.est, krig.map.var)

