#GEOSTAT-------------
# robe spatial
library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics
#1) parameters of the model, model assumption, estimate of tilde(range and psill), comparison between two model,  point predictions of known UTM coordinates  -----------------------------
rm(list=ls())
graphics.off()
data <- read.table('colours.txt',header=T)
head(data)
coordinates(data) <- c('x','y')
head(data)

#a)Assuming a0g = a0, find model via generalized least square
#modello a: y(si) = a0
v <- variogram( data$revenue ~ 1,data=data)  #,cutoff= ... )
plot(v, main = 'Sample Variogram', pch=19 )  
v.fit <- fit.variogram(v, vgm(40, "Sph", 100))  
v.fit
# primo: psill-> altezza asintoto
# secondo: type of model -> Sph, Exp, ... vgm()
# terzo: range-> dove asintoto raggiunge la stabilità nelle x
# quarto: nugget->  dove incorcia x=0
plot(v, v.fit, pch = 19)
#stationarity assumption is verified since the asymptot is reached before cut-off

#estimate of the range and the sill
v.fit$range
v.fit$psill #è la somma

# isotropic assumption verified since even if alpha changes the variogram does not differ too much
v.dir <- variogram(data$revenue~1,data,alpha=(0:3)*45) 
v.anis <- vgm(43, "Sph", 100, anis=c(45, 0.3)) 
print(plot(v.dir, v.anis, pch=19))

data.gstat <- gstat(id = 'revenue', formula = revenue ~ 1, 
                    data = data, model=v.fit, set = list(gls=1))
data.gstat

#estimate the parameter a0 
p1<-predict(data.gstat, data[1,] , BLUE = TRUE) # se sono diversi aumenta nmax o toglilo
p2<-predict(data.gstat, data[50,] , BLUE = TRUE)
p1
p2
#a0= p1 = p2
# a0= revenue.pred = 12.18036

#b)Assuming a0i != a0j , find model via generalized least square
#modello b: y(si) = a0g + δ(si) con g= red, yellow, orange

v2 <- variogram( data$revenue ~ colour,data=data, cutoff=7000)
plot(v2, main = 'Sample Variogram', pch=19)

v.fit2 <- fit.variogram(v2, vgm(5, "Sph", 1200))
v.fit2
plot(v2, v.fit2, pch = 19)
#stationarity assumption is verified since the asymptot is reached before cut-off

# isotropic assumption verified since even if alpha changes the variogram does not differ too much
v.dir2 <- variogram(data$revenue~colour,data,alpha=(0:3)*45) 
v.anis2 <- vgm(43, "Sph", 100, anis=c(45, 0.3)) 
print(plot(v.dir2, v.anis2, pch=19))

data.gstat2 <- gstat(id = 'revenue', formula = revenue ~ colour, 
                    data = data, model=v.fit2)
data.gstat2

#accendo red, poi yellow, poi orange
p1<-predict(data.gstat2, data[1,] , BLUE = TRUE) # red 
p2<-predict(data.gstat2, data[2,] , BLUE = TRUE) # yellow
p3<-predict(data.gstat2, data[3,] , BLUE = TRUE) # orange

a0red <- p1$revenue.pred
a0yellow <- p2$revenue.pred
a0orange <- p3$revenue.pred

a0red
a0yellow
a0orange



#c) comparison
# dici che uno converge prima del cut off l'altro no oppure verifichi le assumptions 
# altrimenti
attr(v.fit, "SSErr")
attr(v.fit2, "SSErr")  #meglio il secondo modello

# quello con valore più basso è meglio
# inoltre da plot se è spherical vuoi che vada all'asintoto con una curva, se è exp con un picco


#d)  three point predictions of the revenue (dataset parlava del guadagno)
new<- data.frame( x=514811.55,y = 5037308.54 , colours = "red" )  #color era una variabile
coordinates(new)<-c('x','y')
pnew<-predict(data.gstat2, new,BLUE=FALSE) # false se ti chiede di un nuovo dato, true se vuoi predictare tenendo la media(tipo per l'annp prox)
pnew
#  revenue.pred= 10.55575

new<- data.frame( x=514811.55,y = 5037308.54 , colour = "orange" )
coordinates(new)<-c('x','y')
pnew<-predict(data.gstat2, new,BLUE=FALSE) # false se ti chiede di un nuovo dato, true se vuoi predictare tenendo la media(tipo per l'annp prox)
pnew 
new<- data.frame( x=514811.55,y = 5037308.54 , colour = "yellow" )
coordinates(new)<-c('x','y')
pnew<-predict(data.gstat2, new,BLUE=FALSE) # false se ti chiede di un nuovo dato, true se vuoi predictare tenendo la media(tipo per l'annp prox)
pnew #25.54375
















#2) logtrasformazione del modello, modello per trovare un dato normalmente noto, variance σ2(s0) (s0 our point) is fully representative?----------------
rm(list=ls())
graphics.off()
data <- read.table('walesharks.txt',header=T)
head(data)
coordinates(data) <- c('x','y')
head(data)
log.sights <- log(data$sights)

#a)
# modello : y= a0 + a1*x
v <- variogram( log.sights ~  1+log.chlorofill , data=data, cutoff = 250000) #cutoff
plot(v, main = 'Sample Variogram', pch=19 )  
v.fit <- fit.variogram(v, vgm(0.55, "Exp", 150000))  
v.fit
# primo: psill-> altezza asintoto
# secondo: type of model -> Sph, Exp, ... vgm()
# terzo: range-> dove asintoto raggiunge la stabilità nelle x
# quarto: nugget->  dove incorcia x=0
plot(v, v.fit, pch = 19)
#stationarity assumption is verified since the asymptot is reached before cut-off

#estimate of the range and the sill
v.fit$range
v.fit$psill

# isotropic assumption verified since even if alpha changes the variogram does not differ too much
v.dir <- variogram( log.sights ~ 1 + log.chlorofill ,data,alpha=(0:3)*45) 
v.anis <- vgm(0.55, "Exp", 150000, anis=c(45, 0.3)) 
print(plot(v.dir, v.anis, pch=19))

data.gstat <- gstat(id = 'log_sights', formula = log.sights ~ 1 + log.chlorofill , 
                    data = data, model=v.fit)
data.gstat

#estimate the parameter a0 and a1
# y1 = a0 + a1*x1 
# y2 = a0 + a1*x2
# -> a1 = ( y2-y1 )/( x2-x1 )
# a0 = y1 - a1*x1
p1<-predict(data.gstat, data[1,] , BLUE = TRUE) # se sono diversi aumenta nmax o toglilo
p2<-predict(data.gstat, data[2,] , BLUE = TRUE)
a1 = ( p2$log_sights.pred - p1$log_sights.pred )/( data[2,]$log.chlorofill - data[1,]$log.chlorofill ) 
a0 = p1$log_sights.pred - a1*data[1,]$log.chlorofill 
a1  #0.7388982
a0   #2.748574




#b) prediction for log-chlorofill
# first we need to do a model for log.chlorofill:

v2 <- variogram( log.chlorofill ~ 1, data=data)
plot(v2, main = 'Sample Variogram', pch=19)

v.fit2 <- fit.variogram(v2, vgm(3.5, "Sph", 20000))
v.fit2
plot(v2, v.fit2, pch = 19)

data.gstat2 <- gstat(id = 'log.chlorofill', formula = log.chlorofill ~ 1, 
                     data = data, nmax = 50, model=v.fit2, set = list(gls=1))
data.gstat2

set.seed(10)

s0.new=data.frame(x=253844.8, y=385997.7) 
coordinates(s0.new)=c('x','y')
p<-predict(data.gstat2, s0.new) 
p #log.chlorofill.pred= 3.790642

#so from my model
new = data.frame(x=253844.8, y=385997.7, log.chlorofill= 3.79) 
coordinates(new)<-c('x','y')
pnew<-predict(data.gstat, new,BLUE=FALSE) # false se ti chiede di un nuovo dato, true se vuoi predictare tenendo la media(tipo per l'annp prox)
pnew 
# log_sights.pred = 5.596835      


#c)variance σ2(s0) (s0 our point) is fully representative of the uncertainty associated with the prediction
pnew$log_sights.var
#0.3863536

# the variance of pnew is not representative, since it does not account for the fact that sigma is unknown
# so, the uncertanty of our predictior is bigger.


















#3)factor -----------------------------------

# Convert urban to a factor
data$urban <- as.factor(data$urban)

rm(list=ls())
graphics.off()
data <- read.table('crops.txt',header=T)
head(data)
coordinates(data) <- c('x','y')
head(data)

#a)
# modello : y= b0
v1 <- variogram( yield ~ 1  , data=data) #cutoff
plot(v1, main = 'Sample Variogram', pch=19 )  
v1.fit <- fit.variogram(v1, vgm(500, "Sph", 600, 10))  
v1.fit
# primo: psill-> altezza asintoto
# secondo: type of model -> Sph, Exp, ... vgm()
# terzo: range-> dove asintoto raggiunge la stabilità nelle x
# quarto: nugget->  dove incorcia x=0
plot(v1, v1.fit, pch = 19)
#stationarity assumption is verified since the asymptot is reached before cut-off

#estimate of the range and the sill
v1.fit$range
v1.fit$psill  #somma dei due  (il primo del nuggut)

# isotropic assumption verified since even if alpha changes the variogram does not differ too much
v.dir <- variogram( yield ~1 ,data,alpha=(0:3)*45) 
v.anis <- vgm(500, "Sph", 600, 10, anis=c(45, 0.3)) 
print(plot(v.dir, v.anis, pch=19))

data.gstat1 <- gstat(id = 'yield', formula = yield ~ 1 , 
                    data = data, model=v1.fit)
data.gstat1

#estimate the parameter a0 and a1
# y1 = a0 + a1*x1 
# y2 = a0 + a1*x2
# -> a1 = ( y2-y1 )/( x2-x1 )
# a0 = y1 - a1*x1
p1<-predict(data.gstat1, data[1,] , BLUE = TRUE) # se sono diversi aumenta nmax o toglilo
p2<-predict(data.gstat1, data[2,] , BLUE = TRUE)
p1 #b0

#b)
s0.new=data.frame(x=625182.088, y=4811723.685, sandy= "no") 
coordinates(s0.new)=c('x','y')
p<-predict(data.gstat1, s0.new) 
p #yield.pred = 126.0004  


#c)
# modello2 : y= b0g + b1g*distance , g=0,1

v <- variogram( yield ~ sandy  + sandy:distance , data=data) #cutoff
plot(v, main = 'Sample Variogram', pch=19 )  
v.fit <- fit.variogram(v, vgm(270, "Sph", 600, 100))  
v.fit
# primo: psill-> altezza asintoto
# secondo: type of model -> Sph, Exp, ... vgm()
# terzo: range-> dove asintoto raggiunge la stabilità nelle x
# quarto: nugget->  dove incorcia x=0
plot(v, v.fit, pch = 19)
#stationarity assumption is verified since the asymptot is reached before cut-off

#estimate of the range and the sill
v.fit$range
v.fit$psill 

# isotropic assumption verified since even if alpha changes the variogram does not differ too much
v.dir <- variogram( yield ~ sandy  + sandy:distance ,data,alpha=(0:3)*45) 
v.anis <- vgm(270, "Sph", 600, 100, anis=c(45, 0.3)) 
print(plot(v.dir, v.anis, pch=19))


data.gstat <- gstat(id = 'yield', formula = yield ~ sandy  + sandy:distance , 
                    data = data, model=v.fit)
data.gstat

#estimate the parameter a00, a10, and a10, a11
# y1 = a00 + a10*x1 
# y6 = a00 + a10*x6
# -> a10 = ( y6-y1 )/( x6-x1 )
# a00 = y1 - a10*x1

p1<-predict(data.gstat, data[1,] , BLUE = TRUE) # se sono diversi aumenta nmax o toglilo
p6<-predict(data.gstat, data[6,] , BLUE = TRUE)
a10 = ( p6$yield.pred - p1$yield.pred )/( data[6,]$distance - data[1,]$distance   ) 
a00 = p1$yield.pred - a10*data[1,]$distance   
a10  #0.02711285
a00   #96.68945

# y2 = a01 + a11*x2
# y3 = a01 + a11*x3
# -> a11 = ( y3-y2 )/( x3-x2 )
# a01 = y2 - a10*x2
p2<-predict(data.gstat, data[2,] , BLUE = TRUE) # se sono diversi aumenta nmax o toglilo
p3<-predict(data.gstat, data[3,] , BLUE = TRUE)
a11 = ( p3$yield.pred - p2$yield.pred )/( data[3,]$distance - data[2,]$distance   ) 
a01 = p2$yield.pred - a11*data[1,]$distance   
a11  #0.01884376
a01  #88.49475

#d)

dis <- sqrt( (623299.322-625182.088)^2 + (4811132.941-4811723.685)^2 )
dis

s0.new=data.frame(x=625182.088, y=4811723.685, sandy= "no", distance= dis ) 
coordinates(s0.new)=c('x','y')
p<-predict(data.gstat, s0.new) 
p #log.chlorofill.pred= 3.790642
#147.491    



#e) comparison
# dici che uno converge prima del cut off l'altro no oppure verifichi le assumptions 
# altrimenti
attr(v1.fit, "SSErr")  
attr(v.fit, "SSErr")  #meglio il secondo modello

# quello con valore più basso è meglio
# inoltre da plot se è spherical vuoi che vada all'asintoto con una curva, se è exp con un picco

























#4) geostat con modello bastardo-------------------
rm(list=ls())
graphics.off()
data <- read.table('beverages.txt',header=T)
head(data)
coordinates(data) <- c('x','y')
head(data)

#a)Assuming a0g = a0, find model via generalized least square
#modello a:
v <- variogram( sales ~ holiday + temp ,data=data)  #,cutoff= ... )
plot(v, main = 'Sample Variogram', pch=19 )  
v.fit <- fit.variogram(v, vgm(0.45, "Sph", 2000, 0.1))  
v.fit
# primo: psill-> altezza asintoto
# secondo: type of model -> Sph, Exp, ... vgm()
# terzo: range-> dove asintoto raggiunge la stabilità nelle x
# quarto: nugget->  dove incorcia x=0
plot(v, v.fit, pch = 19)
#stationarity assumption is verified since the asymptot is reached before cut-off

#estimate of the range and the sill
v.fit$range
v.fit$psill #è la somma


data.gstat <- gstat(id = 'sales', formula = sales ~ holiday + temp , 
                    data = data, model=v.fit)
data.gstat

#b)
#y1 = b00 + b1*x1
#y2 = b00 + b1*x2
#y5 = b01 + b1*x5

## -> b1 = ( y2-y1 )/( x2-x1 )
## b00 = y1 - b1*x1
## b01 = y5 - b1*x5 

p1<-predict(data.gstat, data[1,] , BLUE = TRUE) # se sono diversi aumenta nmax o toglilo
p2<-predict(data.gstat, data[2,] , BLUE = TRUE)
p5<-predict(data.gstat, data[5,] , BLUE = TRUE) 
b1 = ( p2$sales.pred - p1$sales.pred )/( data[2,]$temp - data[1,]$temp ) 
b00 = p1$sales.pred - b1*data[1,]$temp 
b01 = p5$sales.pred - b1*data[5,]$temp 
b00
b01
b1



#c)
s0.new=data.frame(x=0, y=0, holiday=TRUE, temp= 30) 
coordinates(s0.new)=c('x','y')
p<-predict(data.gstat, s0.new) 
p #sales.pred= 2.181444




#d)


v2 <- variogram( sales ~ central:holiday + central:temp ,data=data)  #,cutoff= ... )
plot(v2, main = 'Sample Variogram', pch=19 )  
v2.fit <- fit.variogram(v2, vgm(0.45, "Sph", 2000))  
v2.fit
# primo: psill-> altezza asintoto
# secondo: type of model -> Sph, Exp, ... vgm()
# terzo: range-> dove asintoto raggiunge la stabilità nelle x
# quarto: nugget->  dove incorcia x=0
plot(v2, v2.fit, pch = 19)


data.gstat2 <- gstat(id = 'sales', formula = sales ~ central:holiday + central:temp , 
                     data = data, model=v2.fit)
data.gstat2

#y3 = b000 + b10*x3
#y91 = b000 + b10*x91
#y1 = b001 + b11*x1
#y2 = b001 + b11*x2

## -> b10 = ( y91-y3 )/( x91-x3 )
## b000 = y3 - b10*x3


data

p1<-predict(data.gstat2, data[1,] , BLUE = TRUE) # se sono diversi aumenta nmax o toglilo
p2<-predict(data.gstat2, data[2,] , BLUE = TRUE)
p3<-predict(data.gstat2, data[3,] , BLUE = TRUE)
p91<-predict(data.gstat2, data[91,] , BLUE = TRUE) 

b10 = ( p91$sales.pred - p3$sales.pred )/( data[91,]$temp - data[3,]$temp ) 
b000 = p3$sales.pred - b10*data[3,]$temp 

b11 = ( p2$sales.pred - p1$sales.pred )/( data[2,]$temp - data[1,]$temp ) 
b001 = p1$sales.pred - b11*data[1,]$temp 

b01 = p5$sales.pred - b1*data[1,]$temp 
b000
b001
b11
b10














