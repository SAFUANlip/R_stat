library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)

# A study is conducted to analyze the impact of weather conditions, holiday periods, and geographic location on the
# sales of cold beverages by bars from May to July 2023 in Milan. The dataset beverages.txt reports the sales of
# cold beverages of various bars of Milan at di↵erent days in the considered period. It includes the UTM geographical
# coordinates si of the bars, a categorical variable holiday indicating if the considered day is a holiday (Saturday,                                                                                                        Sunday or bank holiday) or not, the recorded average daily temperature tempi (in degrees Celsius) during this
# period and the sales of cold beverages during that day y(si) [k e/day]. Consider the following model:

beverages <- read.table("2024_01_17/beverages.txt", h=TRUE)
coordinates(beverages) <- c('x', 'y')

# y(si) = b0,j + b1tempi + delta(si) 
# where delta(si) represents stationary residuals
# with spherical variogram with nugget and j = 0, 1 the grouping induced
# by the variable holiday (j = 0 for holiday= FALSE, j = 1 for holiday= TRUE).

# a) Report a plot of the fitted variogram. Indicate the estimate of the range and the sill.

v <- variogram(sales ~ -1 + as.factor(holiday) + temp, beverages)
plot(v)
# params: psill = NA, model, range = NA, nugget
v.fit <- fit.variogram(v, vgm(0.45, "Sph", 2000, 0.15), fit.method = 7) # 7 - by default (Weighted least squares) 
v.fit # here we see psill and range (estimated)
#    model     psill    range
# 1   Nug  0.2087394    0.000
# 2   Sph 0.2388645 2141.746
# psill = 0.2087394 + 0.2388645 = 0.4476039
# range = 2141.746
plot(v, v.fit)


# b) Estimate the parameters b0,0, b0,1 and b1 using the generalized least squares method.
g.tr <- gstat(formula = sales ~  -1 + as.factor(holiday) + temp, data = beverages, model = v.fit)
g.tr

s0.new=data.frame(x=0.0, y=0.0, holiday=FALSE, temp = 0, central = TRUE) # UTM coordinates
coordinates(s0.new)=c('x','y')
predict(g.tr, s0.new, BLUE = TRUE) # 1.149987 = betta_0_0 

s1.new=data.frame(x=0.0, y=0.0, holiday=TRUE, temp = 0, central = TRUE) # UTM coordinates
coordinates(s1.new)=c('x','y')
predict(g.tr, s1.new, BLUE = TRUE) # 1.579688 = betta_0_1

s2.new=data.frame(x=0.0, y=0.0, holiday=TRUE, temp = 1, central = TRUE) # UTM coordinates
coordinates(s2.new)=c('x','y')
predict(g.tr, s2.new, BLUE = TRUE) # 1.599747 = betta_0_1 + betta_1
# betta_1 = 1.599747 - 1.579688 = 0.020059

s3.new=data.frame(x=0.0, y=0.0, holiday=FALSE, temp = 1, central = TRUE) # UTM coordinates
coordinates(s3.new)=c('x','y')
predict(g.tr, s3.new, BLUE = TRUE) # 1.170046 = betta_0_0 + betta_1
# betta_1 = 1.170046 - 1.149987 = 0.020059

# c) Using the model, provide an estimate of the total sales that will be realised by La Spritzeria in the month of
# July 2024 considering a constant temperature of 30°C.

# 31 day in July, 8 holidays, 23 working days
s4.new=data.frame(x=0.0, y=0.0, holiday=FALSE, temp = 30, central = TRUE) # UTM coordinates
coordinates(s4.new)=c('x','y')
predict(g.tr, s4.new, BLUE = TRUE) # 1.751743 => 1.751743 * 23 = 40.29009

s5.new=data.frame(x=0.0, y=0.0, holiday=TRUE, temp = 30, central = TRUE) # UTM coordinates
coordinates(s5.new)=c('x','y')
predict(g.tr, s5.new, BLUE = TRUE) # 2.181444 => 2.181444 * 8 = 17.45155
# Total: 40.29009 + 17.45155 = 57.74164 sales

# d) Due to the geographical location of the bars, they can be categorized into central (central=TRUE) or peripheral
# (central=FALSE). Modify the model in Eq. (3), including this categorical e↵ect, as follows:

# where delta(si) represents stationary residuals with spherical variogram without nugget and k the grouping induced
# by the variable central.

summary(lm(sales ~ -1 + as.factor(holiday):as.factor(central) + temp:as.factor(central), beverages))
v2 <- variogram(sales ~ -1 + as.factor(holiday):as.factor(central) + temp:as.factor(central), beverages)
plot(v2)
# params: psill = NA, model, range = NA, nugget
v2.fit <- fit.variogram(v2, vgm(0.25, "Sph", 2000), fit.method = 7) # 7 - by default (Weighted least squares) 
v2.fit # here we see psill and range (estimated)
#    model     psill    range
# 1   Sph 0.26918 1815.396
# range = 1815.396
plot(v2, v2.fit)

# Provide an estimate of the parameters b0,0,0, b0,0,1, b1,0 and b1,1.
g2.tr <- gstat(formula = sales ~ -1 + as.factor(holiday):as.factor(central) + temp:as.factor(central),
               data = beverages, model = v2.fit)
g2.tr

s6.new=data.frame(x=0.0, y=0.0, holiday=FALSE, central = FALSE, temp=0) # UTM coordinates
coordinates(s6.new)=c('x','y')
predict(g2.tr, s6.new, BLUE = TRUE) # 0.5865326 = betta_0_0_0

s7.new=data.frame(x=0.0, y=0.0, holiday=FALSE, central = TRUE, temp=0) # UTM coordinates
coordinates(s7.new)=c('x','y')
predict(g2.tr, s7.new, BLUE = TRUE) # 1.226432 = betta_0_0_1

s8.new=data.frame(x=0.0, y=0.0, holiday=FALSE, central = FALSE, temp=1) # UTM coordinates
coordinates(s8.new)=c('x','y')
predict(g2.tr, s8.new, BLUE = TRUE) # 0.6094533 = betta_0_0_0 + betta_1_0
# betta_1_0 = 0.6094533 - 0.5865326 = 0.0229207

s9.new=data.frame(x=0.0, y=0.0, holiday=FALSE, central = TRUE, temp=1) # UTM coordinates
coordinates(s9.new)=c('x','y')
predict(g2.tr, s9.new, BLUE = TRUE) # 1.264048 = betta_0_0_1 + betta_1_1
# betta_1_1 = 1.264048 - 1.226432 = 0.037616

# How would you describe the effect of the central variable (during working days)?
# Central varibel during working days corrspond to betta_0_0_1 = 1.226432
# so there are positive effect of being in city center, especially compared to betta_0_0_0
# non cetral part of city (sales in city center higher in working days, compared to non city center in working days)










