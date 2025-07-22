library(mvtnorm) # to deal with multivariate normal distributions
library(car) # "Companion to Applied Regression" for regression analysis
library(pheatmap) # pheatmap


# The file athlete stats.txt contains information about the performance metrics of 100 athletes. Each column
# represents a unitless specific attribute of the athlete, indicating their level of performance in a particular skill or
# ability. Higher values correspond to better performance. The variables include:
#  • sprint speed: Speed in short-distance sprints.
#  • endurance: Stamina for sustained activities.
#  • vertical jump: Height achieved in vertical jumps.
#  • agility: Ability to change direction quickly and e!ciently.
#  • strength: Muscular power and force.
#  • reaction time: Speed of response to a stimulus.
#  • accuracy: Precision in tasks requiring targeting or control.
#  • flexibility: Range of motion and adaptability in movements.
#  • throwing power: Strength and distance of throws.
#
# a) Perform a Principal Component Analysis (PCA) on the dataset. Decide whether to use the original variables
# or the standardized ones, and justify your choice. Is there a clear number of principal components to consider?
#   How many principal components are needed to explain at least → 80% of the total variability?

athlete_stats <- read.table("2025_01_17/athlete_stats.txt", h=TRUE) 

n <- dim(athlete_stats)[1]
p <- dim(athlete_stats)[2]

# Exploration
# I would not standartise variabels, due to each feature have almost same sample variance
boxplot(athlete_stats, col = 'gold', main = 'Original Variables')

pc.athlete_stats <- princomp(athlete_stats, scores=T)
pc.athlete_stats
summary(pc.athlete_stats)

pc.athlete_stats$loadings
head(pc.athlete_stats$scores)

# PCA on scaled data will have different result
# summary(princomp(scale(athlete_stats), scores=T))$loadings
# princomp(scale(athlete_stats), scores=T)$loadings
# head(princomp(scale(athlete_stats), scores=T)$scores)

help(princomp)

# Importance of components:
#                          Comp.1    Comp.2    Comp.3     Comp.4     Comp.5     Comp.6     Comp.7
# Standard deviation     0.7508806 0.4576503 0.4126813 0.35343030 0.30353672 0.24955419 0.22959069
# Proportion of Variance 0.4200826 0.1560488 0.1268886 0.09306803 0.06864604 0.04640051 0.03927367
# Cumulative Proportion  0.4200826 0.5761314 0.7030200 0.79608805 0.86473408 0.91113459 0.95040826
#                          Comp.8     Comp.9
# Standard deviation     0.20935160 0.15077257
# Proportion of Variance 0.03265469 0.01693705
# Cumulative Proportion  0.98306295 1.00000000

# To explain 80% of total variability, I need at least 5 components,
# And I can not say that there is clear number of components to consider, due to 
# cumulative proportion growth smoothly 

# BUT in answer is 4, but on Comp.4 we have 0.79608805 of variability in total

layout(matrix(c(2, 3, 1, 3), 2, byrow = TRUE))
barplot(pc.athlete_stats$sdev^2, las = 2, main = 'Principal Components', ylim = c(0, 1), ylab = 'Variances')
abline(h = 1, col = 'blue')
barplot(sapply(athlete_stats, sd)^2, las = 2, main = 'Original Variables', ylim = c(0, 1),
        ylab = 'Variances')
plot(cumsum(pc.athlete_stats$sdev^2) / sum(pc.athlete_stats$sde^2), type = 'b', axes = FALSE,
     xlab = 'Number of Components', ylab = 'Contribution to the Total Variance', ylim = c(0, 1))
box()
axis(2, at = 0:10/10, labels = 0:10/10)
axis(1, at = 1:ncol(athlete_stats), labels = 1:ncol(athlete_stats), las = 2)

# b) Report a plot of the loadings of the first three principal components. Provide an interpretation for each of these
# components based on the loadings.

# Loadings
n_lod_show <- 3
load.athlete_stats <- pc.athlete_stats$loadings
par(mar = c(1, n_lod_show, 0, 2), mfrow = c(n_lod_show, 1))
for (i in 1:n_lod_show) {
  barplot(load.athlete_stats[, i], ylim = c(-1, 1))
}
# - First components more expalins general performance of athlete, how goo it at all
# - second component based on comparison of 
# from one side: sprint_speed, endurance, vertical_jump, strength, throwing power (muscels strongs)
# on ther hand: reaction time, accuracy, flexibility (nervouse system ability)
# - Third component:
# more based on reaction time , speed and endurace
# In answer - highlits athelts, that have high mobility



# c) Report the biplot of the data along the second and the third principal components. How would you qualify the
# athlete labeled as 78 (row index) based on the biplot?
x_coord <- pc.athlete_stats$scores[78, 2] 
y_coord <- pc.athlete_stats$scores[78, 3] 

biplot(pc.athlete_stats, choices = c(2, 3)) # how to change Principal components, where we plot our data (PC2 and PC3)
points(x_coord, y_coord, col = "red", pch = 19, cex = 1.5)
text(x_coord, y_coord, labels = "78", pos = 4, col = "red")
# it shows right direction, but wrong point
# But observation 78 based on throwing power in PC2-PC3 dimension
# 9. How would you qualify the athlete labeled as 78
# (row index) based on the biplot? (multiple answers possible)
# Low mobility (yes, because reaction_time and sprint_speed in oppsoit directions)
# Muscle power (yes, coincide with throwing_power direction)
# Good runner (no as said previously)
# Lightweight (we don't have this feature)


# d) Based on the dimensionality reduction suggested in part (a),
# project a new athlete with characteristics provided
# in Table 1 onto the reduced space, and compute its
# coordinates in the reference system of the first three principal
# 
# components.
# sprint speed 1.85
# endurance 1.74
# vertical jump 1.92
# agility 1.89
# strength 1.78
# reaction time 1.81
# accuracy 1.69
# flexibility 1.76
# throwing power 1.84
# 
# Table 1: Performance metrics of a new athlete.

new_obs <- c(1.85, 1.74, 1.92, 1.89, 1.78, 1.81, 1.69, 1.76, 1.84)
new_obs_df <- as.data.frame(t(new_obs))
colnames(new_obs_df) <- colnames(athlete_stats)

pc.athlete_stats$loadings[,]

pc.athlete_stats$center

# what happen if don't centralise input
t(as.matrix(new_obs - colMeans(athlete_stats))) %*% as.matrix(pc.athlete_stats$loadings)
predict(pc.athlete_stats, new_obs_df)

colMeans(athlete_stats)
# sprint_speed      endurance  vertical_jump        agility       strength 
# 0.8609686      1.0029763      0.9546951      0.9374458      0.7534036 
# reaction_time       accuracy    flexibility throwing_power 
# 1.0367553      0.8944484      1.0305398      0.9493422

pc.athlete_stats$center
# sprint_speed      endurance  vertical_jump        agility       strength 
# 0.8609686      1.0029763      0.9546951      0.9374458      0.7534036 
# reaction_time       accuracy    flexibility throwing_power 
# 1.0367553      0.8944484      1.0305398      0.9493422

pc.athlete_stats$scale
# sprint_speed      endurance  vertical_jump        agility       strength 
# 1              1              1              1              1 
# reaction_time       accuracy    flexibility throwing_power 
# 1              1              1              1

diag(var(athlete_stats))
# sprint_speed      endurance  vertical_jump        agility       strength 
# 0.1130895      0.1442024      0.1400125      0.1556007      0.1119988 
# reaction_time       accuracy    flexibility throwing_power 
# 0.1849666      0.2046396      0.1557840      0.1454317

# If centralise 
t(as.matrix(new_obs - colMeans(athlete_stats))) %*% as.matrix(pc.athlete_stats$loadings)

predict(pc.athlete_stats, new_obs_df) # model knowing, that data has not zero mean, centralise input by automatically 
# predict(pc.NO.sd, new_obs_df-colMeans(NO)) # if we use pca, which trained on сentralised data - we have to centralise by ourself

# Coordinates in first 3 PC:
# 2.592293 0.2497497 0.02197077

# What if we applied pca on centralised data (not on S, but Correlation)
athlete_stats.sd <- scale(athlete_stats)
pc.athlete_stats.sd <- princomp(athlete_stats.sd, scores=T)
summary(pc.athlete_stats.sd)

# we may see that loadings changed, as scores and proportion of variance in each component
n_lod_show <- 3
load.athlete_stats.sd <- pc.athlete_stats.sd$loadings
par(mar = c(1, n_lod_show, 0, 2), mfrow = c(n_lod_show, 1))
for (i in 1:n_lod_show) {
  barplot(load.athlete_stats.sd[, i], ylim = c(-1, 1))
}

eigen(cov(athlete_stats))
eigen(cov(athlete_stats.sd))

load.athlete_stats.sd

# same vetors, but may have opposite direction
load.athlete_stats.sd[,"Comp.1"]
eigen(cov(athlete_stats.sd))$vectors[,1]


