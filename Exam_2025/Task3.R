library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)


data <- read.table("treegrowth.txt", h=TRUE)
coordinates(data) <- c('x', 'y')

v <- variogram(growth ~ -1 + as.factor(season) + moisture, data)
summary(v)
plot(v)


v.fit <- fit.variogram(v, vgm(0.1, "Sph", 1000,0.1))
v.fit
plot(v, v.fit)

lm_model <- lm(growth ~ -1 + as.factor(season) + moisture, data)
summary(lm_model)

summary(v.fit)
summary(v)


s0.new=data.frame(x=0.1, y=0.2, moisture=25.0, season=TRUE) # UTM coordinates
coordinates(s0.new)=c('x','y')

g.tr <- gstat(formula = growth ~ -1 + as.factor(season), data = data, model = v.fit)
summary(g.tr)


predict(g.tr, s0.new, BLUE=TRUE)

v2 <- variogram(growth ~ -1 + as.factor(season):as.factor(canopy) + moisture, data)
summary(v2)
plot(v2)

v2.fit <- fit.variogram(v2, vgm(0.1, "Sph", 3000,0.1), fit.sills = c(FALSE, TRUE))
v2.fit

g2.tr <- gstat(formula = growth ~ -1 + as.factor(season):as.factor(canopy) + moisture, data = data, model = v2.fit)
summary(g.tr)

lm_model <- lm(growth ~ -1 + as.factor(season):as.factor(canopy) + moisture:as.factor(canopy), data)
summary(lm_model)
