
#______________ Applied Statistics 2024/2025 _________________
####         Mixed-effects Models Module - Lab 2          ####


#### RIKZ EXAMPLE ####
#_____________________

# Marine benthic data from 9 inter-tidal areas (beaches) along the Dutch coast. 
# The data were collected by the Dutch institute RIKZ.

# In each beach, 5 samples were taken, 
# the macro-fauna and abiotic variables were measured. 

RIKZ <- read.table("RIKZ.txt", header=T)
RIKZ$fBeach <- factor(RIKZ$Beach)

# - R_ij species richness at site j on beach i ;
# - NAP_ij  the height of site j on beach i compared to mean beach level ;
# - Exposure_i is the exposure on beach i 
#      [ an index composed of the following elements: 
#        wave action, length of the surf zone, slope, grain size, 
#        and the depth of the anaerobic layer ] ;


# R_ij = beta0 + beta1 * NAP_ij + beta2 * Exposure_i + eps_ij, eps_ij~N(0, sigma^2)


# Beach is a factor with 9 levels, and the 1st level is used as baseline. 
# --> including this term would cost eight regression parameters
# --> we include a beach "random" effect in the model, 
#     assuming the variation around the intercept, for each beach, 
#     is normally distributed with a certain variance. 


#________________________________________________
##### 1) Linear model with random intercept #####
library(nlme)

Mlme1 <- lme(Richness ~ NAP, 
             random = ~1 | fBeach, # specifies a random intercept model (for each beach)
             data = RIKZ)
summary(Mlme1)
# we have not only Residuals, but also Intercept
# sigma = 3.05977
# sigma*sqrt(d11) = 2.944065

plot(ranef(Mlme1))
# average around 0, but there are effect


F0 <- fitted(Mlme1, level = 0)
F1 <- fitted(Mlme1, level = 1)
I <- order(RIKZ$NAP); NAPs <- sort(RIKZ$NAP)
plot(NAPs, F0[I], lwd = 4, type = "l",
     ylim = c(0, 22), ylab = "Richness", xlab = "NAP")
for (i in 1:9){
  x1 <- RIKZ$NAP[RIKZ$Beach == i]
  y1 <- F1[RIKZ$Beach == i]
  K <- order(x1)
  lines(sort(x1), y1[K])
}

# The thick line represents the 'Population model' (6.58 – 2.56*NAP_i).
# The other lines are obtained by adding the contribution of b_i 
# for each beach i to the population fitted curve. 

# so we estimated variance of each beach 
# boold line - like pool all observations, ignoring groups 

# The random intercept model implies one average curve (the thick line) that is allowed
# to be shifted up, or down, for each beach by something that is normally distributed
# with a certain variance sigma*d. 
# --> If sigma*d11 is large, the vertical shifts will be relative large;
# --> If sigma*d11 = 0, all shifts are zero and coincide with the thick line.



# Now suppose that the relationship between species richness R and NAP is different on each beach. 
# --> we need to include a NAP–Beach interaction term to the model. 
#     Ri = factor(Beach) + NAP * factor(Beach). 
# Because beach has 9 levels and 1 level is used as the baseline, 
# --> too many model parameters

# To estimate model degrees of freedom more efficiently, we can apply the
# mixed effects model with a random intercept (as before) and a random slope.

#_________________________________________________________________
##### 2) Linear model with random intercept and random slope #####

Mlme2 <- lme(Richness ~ NAP,
             random = ~1 + NAP | fBeach, data = RIKZ)
summary(Mlme2)
# sigma = 2.7
# sigma*sqrt(d11) = 3.55
# sigma*sqrt(d22) = 1.72
# there is a rather high correlation between the random intercepts and slopes  
# indicating that beaches with a high positive intercept also have a high negative slope.
plot(ranef(Mlme2))

F0 <- fitted(Mlme2, level = 0)
F1 <- fitted(Mlme2, level = 1)
I <- order(RIKZ$NAP); NAPs <- sort(RIKZ$NAP)
plot(NAPs, F0[I], lwd = 4, type = "l",
     ylim = c(0, 22), ylab = "Richness", xlab = "NAP")
for (i in 1:9){
  x1 <- RIKZ$NAP[RIKZ$Beach == i]
  y1 <- F1[RIKZ$Beach == i]
  K <- order(x1)
  lines(sort(x1), y1[K])
}

# The thick line is the fitted population curve, 
# and the other lines the within-beach fitted curves.


