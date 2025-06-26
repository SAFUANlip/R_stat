library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat) 


# With over 280 cinemas, Paris boasts the most extensive cinema scene in the world. A study is conducted to analyze
# the impact of several factors on the daily sales of movie tickets in Paris. The dataset moviesales.txt includes the
# UTM geographical coordinates si of 100 cinemas in Paris, the standard ticket price in the cinema concerned (ine), 
# and the number of tickets y(si) (in thousands) sold during one day (which is not necessarily the same between
# the cinemas) of May 2024. Additionally, the boolean variables rainy and cloudy
# indicate whether the day duringwhich the ticket sales were counted was cloudy and/or rainy.

# Ticket sales are first modeled based on price using the following relationship:
# y(si) = b0 + b1price + ω(si) 

moviesales <- read.table("20250206/moviesales.txt", h=TRUE)
coordinates(moviesales) <- c('x', 'y')

# where ω(si) represents a stationary residual modeled with a spherical variogram with nugget.

# a) Report a plot of the fitted variogram. Indicate the estimate of the range and the sill.
v <- variogram(sales ~ 1 + price, moviesales)
plot(v)

v.fit <- fit.variogram(v, vgm(3, "Sph", 2000, 1), fit.method = 7) # 7 - by default (Weighted least squares) 
# I did't indicate nugget, so model don't use it
v.fit # here we see psill and range (estimated)
plot(v, v.fit)

# model    psill    range
# 1   Nug 1.451283    0.000
# 2   Sph 1.550279 2001.374
# range = 2001.374
# psill = 1.451283 + 1.550279 = 3.001562

# b) Estimate the parameters b0 and b1 using the generalized least squares method.
g.tr <- gstat(formula = sales ~ 1 + price, data = moviesales, model = v.fit)
summary(g.tr)

s0.new=data.frame(x=0.0, y=0.0, price=0.0) # UTM coordinates
coordinates(s0.new)=c('x','y')

predict(g.tr, s0.new, BLUE = TRUE) # var1.pred = 4.323927 = b_0

s1.new=data.frame(x=0.0, y=0.0, price=1.0) # UTM coordinates
coordinates(s1.new)=c('x','y')

predict(g.tr, s1.new, BLUE = TRUE) # b_1 = 4.35331 - b_0 = 0.029383

# c) Provide an estimate of the number of tickets that will be sold during a day of May 2025 (assuming spatial
# correlation) for the new cinema Les Visionnaires, located next to the Basilique du Sacr´e-Coeur de Montmartre
# (located at x=514712, y=5033903), with a ticket price of 8.50 e.

sc.new=data.frame(x=514712, y=5033903, price=8.50) # UTM coordinates
coordinates(sc.new)=c('x','y')

predict(g.tr, sc.new, BLUE = FALSE) # var1.pred = 4.910301 - b_0 # FALSE - because (assuming spatial correlation)

# d) Modify the model in Eq. (3) as follows:
#       y(si) = b0,j + b1,k price + ω(si) 

# where j represents the grouping induced by the variable cloudy 
# and k the grouping induced by the variable rainy.

# Provide an estimate of the parameters b0,0, b0,1, b1,0 and b1,1. 
# How would you describe the e!ect of the rainy variable on the daily ticket sales?

summary(lm(sales ~ -1 + as.factor(cloudy) + price:as.factor(rainy), moviesales))
v2 <- variogram(sales ~ -1 + as.factor(cloudy) + price:as.factor(rainy), moviesales)
plot(v2)

v2.fit <- fit.variogram(v2, vgm(1, "Sph", 2000, 0.2), fit.method = 7) # 7 - by default (Weighted least squares) 
v2.fit # here we see psill and range (estimated)
plot(v2, v2.fit)

g2.tr <- gstat(formula = sales ~ -1 + as.factor(cloudy) + price:as.factor(rainy), data = moviesales, model = v2.fit)
summary(g2.tr)

s2.new=data.frame(x=0.0, y=0.0, price=0.0, cloudy=FALSE, rainy=FALSE) # UTM coordinates
coordinates(s2.new)=c('x','y')
predict(g2.tr, s2.new, BLUE = TRUE) # var1.pred = 3.402779 = b_0_0

s3.new=data.frame(x=0.0, y=0.0, price=0.0, cloudy=TRUE, rainy=FALSE) # UTM coordinates
coordinates(s3.new)=c('x','y')
predict(g2.tr, s3.new, BLUE = TRUE) # var1.pred = 4.802318 = b_0_1

s4.new=data.frame(x=0.0, y=0.0, price=1.0, cloudy=FALSE, rainy=FALSE) # UTM coordinates
coordinates(s4.new)=c('x','y')
predict(g2.tr, s4.new, BLUE = TRUE) # var1.pred = 3.34435 = b_0_0 + b_1_0
# b_1_0 = 3.34435 - 3.402779 = -0.058429

s5.new=data.frame(x=0.0, y=0.0, price=1.0, cloudy=TRUE, rainy=FALSE) # UTM coordinates
coordinates(s5.new)=c('x','y')
predict(g2.tr, s5.new, BLUE = TRUE) # var1.pred = 4.743889 = b_0_1 + b_1_0
# b_1_0 = 4.743889 - 4.802318 = -0.058429

s6.new=data.frame(x=0.0, y=0.0, price=1.0, cloudy=FALSE, rainy=TRUE) # UTM coordinates
coordinates(s6.new)=c('x','y')
predict(g2.tr, s6.new, BLUE = TRUE) # var1.pred = 3.650044 = b_0_0 + b_1_1
# b_1_1 = 3.650044 - 3.402779 = 0.247265

s7.new=data.frame(x=0.0, y=0.0, price=1.0, cloudy=TRUE, rainy=TRUE) # UTM coordinates
coordinates(s7.new)=c('x','y')
predict(g2.tr, s7.new, BLUE = TRUE) # var1.pred = 5.049583 = b_0_1 + b_1_1
# b_1_1 = 5.049583 - b_0_1 = 5.049583 - 4.802318 = 0.247265

# as coefficents of rainy is positive - rainy days increases number of sold tickets 
# and when there are no rain - we have negative effect on sold tickets 
mean(moviesales$sales)
mean(moviesales[moviesales$rainy == FALSE,]$sales)
mean(moviesales[moviesales$rainy == TRUE,]$sales)
