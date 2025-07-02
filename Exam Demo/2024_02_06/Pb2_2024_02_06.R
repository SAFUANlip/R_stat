library(MASS)
library(MVN)


# A large food manufacturing company, concerned about maintaining high standards in its products, is implementing
# a new quality control measure to ensure the safety and integrity of its food items. Instead of relying solely on
# traditional laboratory testing methods, which can be time-consuming and costly, the company aims to introduce
# a preliminary sensory-based test using taste, smell and visual indicators. This sensory test will serve as an
# initial screening to identify products with abnormal taste or odor characteristics that may indicate quality issues.
# Subsequently, those flagged by the sensory test will undergo more comprehensive and accurate laboratory testing.
# The outcome of the test reports the value of the variables: taste, which quantifies the perceived intensity
# of flavors in the food; odor, representing the strength of any odors emitted by the food; smooth, indicating the
# smoothness or roughness of the food’s texture; and bright, measuring the brightness or vividness of the food’s
# color. Additionally, the variable result indicates whether these products passed or failed the laboratory quality
# tests.
# 
# The aim is to build a classifier for discriminating between high-quality and low-quality food products based on
# the four sensory parameters listed above.
# The cost associated with the laboratory quality tests is $500 per product. This cost is equivalent to the estimated
# economic benefit of correctly identifying a true low-quality product. The economic loss incurred due to failing to
# detect a low-quality product is quantified as a cost of $100,000. The company estimates that 0.1% of food products
# have quality issues.

food <- read.table("2024_02_06/food.txt", h=TRUE)
sum(food$result == "high")
high <- which(food$result == "high")
sum(food$result == "low")
low <- which(food$result == "low")

food_data <- food[,1:4]

# a) Which classification method should be used? Verify the underlying assumptions.

# a) Which classification method would you use? Verify the underlying assumptions.
mvn(food_data[high,]) # MVN
mvn(food_data[low,]) # NOT MVN

boxM(food_data, food$result) # p-value > 0.05 => same covariance structure => not QDA => LDA


S_high <-  cov(food_data[high,])
S_low <-  cov(food_data[low,])

max(abs(S_high/S_low)) # < 10
min(abs(S_high/S_low)) # > 0.1

# So as groups have same covariance structure, but not MVN, we have to use FDA

# b) Build the corresponding classifier, providing its apparent error rate and an estimate of its actual error rate
# through leave-one-out cross-validation. What do you observe? Which characteristic of the classifier you are
# using is being highlighted here?

g <- 2
n1 <- length(high)
n2 <- length(low)

n <- n1+n2

m <-  colMeans(food_data)
m1 <- colMeans(food_data[high,])
m2 <- colMeans(food_data[low,])

S1 <- cov(food_data[high,])
S2 <- cov(food_data[low,])
Sp  <- ((n1-1)*S1+(n2-1)*S2)/(n-g)

# covariance between groups (estimate) # should be here 1/(g-1)?
B <- 1/(g-1)*(cbind(m1 - m) %*% rbind(m1 - m) +
                cbind(m2 - m) %*% rbind(m2 - m))
B

# covariance within groups (estimate)
Sp

# how many coordinates?
p <- 4
s <- min(g-1,p)
s

# system on which we project the data (in PCA it's orhogonal basis, but here no)
# Matrix Sp^(-1/2)

val.Sp <- eigen(Sp)$val
vec.Sp <- eigen(Sp)$vec
invSp.2 <- 1/sqrt(val.Sp[1])*vec.Sp[,1]%*%t(vec.Sp[,1]) + 1/sqrt(val.Sp[2])*vec.Sp[,2]%*%t(vec.Sp[,2])
invSp.2

# ration variability between groups compared to variability within groups
# spectral decomposition of Sp^(-1/2) B Sp^(-1/2)
spec.dec <- eigen(invSp.2 %*% B %*% invSp.2)
spec.dec$val # s=1, so we have only one direction, where can project our data
# o

# first canonical coordinate
a1 <- invSp.2 %*% spec.dec$vec[,1] # not exact eigen vector, but multiplyed by invSp
# 6.130371e-01  3.965295e-18  2.083256e-19 -1.760117e-16
# others almost zero


### How are the data classified?
# Compute the canonical coordinates of the data
# we want project data to canonical coordinates 
cc1.food <- as.matrix(food_data)%*%a1

coord.cc <- cc1.food

# Compute the coordinates of the mean within groups along the canonical directions
# here we use our manually extracted vectors
cc.m1 <- m1%*%a1
cc.m2 <- m2%*%a1

# Assign data to groups
f.class=rep(0, n)
for(i in 1:n) # for each datum
{
  # Compute the Euclidean distance of the i-th datum from mean within the groups
  dist.m=c(d1=sqrt(sum((coord.cc[i,]-cc.m1)^2)),
           d2=sqrt(sum((coord.cc[i,]-cc.m2)^2))
           )
  # Assign the datum to the group whose mean is the nearest
  f.class[i]=which.min(dist.m)
}
f.class
table(class.true=food$result, class.assigned=f.class)

#        class.assigned
# class.true  1  2
# high       22  8
# low         9 21

APERf <- 8/30 * (1-0.001) + 9/30 * 0.001
APERf


# CV for FDA -------------------------------------------------------------------
n <- nrow(food)
f.classCV=rep(0, n)
for(i in 1:n) # for each datum
{
  g_i <- 2
  food_i <- food[-i,]
  high_i <- which(food_i$result == "high")
  low_i<- which(food_i$result == "low")
  
  food_data_i <- food_i[, 1:4]
  
  
  n1_i <- length(high_i)
  n2_i <- length(low_i)
  
  n_i <- n1_i+n2_i
  
  m_i <-  colMeans(food_data_i)
  m1_i <- colMeans(food_data_i[high_i,])
  m2_i <- colMeans(food_data_i[low_i,])
  
  S1_i <- cov(food_data_i[high_i,])
  S2_i <- cov(food_data_i[low_i,])
  Sp_i  <- ((n1_i-1)*S1_i+(n2_i-1)*S2_i)/(n_i-g_i)
  
  # covariance between groups (estimate) # should be here 1/(g-1)?
  B_i <- 1/(g_i-1)*(cbind(m1_i - m_i) %*% rbind(m1_i - m_i) +
                  cbind(m2_i - m_i) %*% rbind(m2_i - m_i))
  
  # covariance within groups (estimate)
  Sp_i
  
  # how many coordinates?
  p_i <- 4
  s_i <- min(g_i-1,p_i)
  s_i
  
  # system on which we project the data (in PCA it's orhogonal basis, but here no)
  # Matrix Sp^(-1/2)
  
  val_i.Sp <- eigen(Sp_i)$val
  vec_i.Sp <- eigen(Sp_i)$vec
  invSp_i.2 <- 1/sqrt(val_i.Sp[1])*vec_i.Sp[,1]%*%t(vec_i.Sp[,1]) + 1/sqrt(val_i.Sp[2])*vec_i.Sp[,2]%*%t(vec_i.Sp[,2])
  invSp_i.2
  
  spec_i.dec <- eigen(invSp_i.2 %*% B_i %*% invSp_i.2)
  
  a1_i <- invSp_i.2 %*% spec_i.dec$vec[,1]
  
  test_data <- as.matrix(food[i, 1:4])  # это исключённая точка
  coord_test <- test_data %*% a1_i
  
  # Compute the coordinates of the mean within groups along the canonical directions
  # here we use our manually extracted vectors
  cc_i.m1 <- m1_i%*%a1_i
  cc_i.m2 <- m2_i%*%a1_i
  
  # Compute the Euclidean distance of the i-th datum from mean within the groups
  dist_i.m=c(
    d1_i = sqrt(sum((coord_test - cc_i.m1)^2)),
    d2_i = sqrt(sum((coord_test - cc_i.m2)^2))
  )
  # Assign the datum to the group whose mean is the nearest
  f.classCV[i]=which.min(dist_i.m)
}

f.classCV
table(class.true=food$result, class.assigned=f.classCV)

AER <- 8/30 * (1-0.001) + 9/30 * 0.001
AER # so we have same result for AER and APER, it means, that our model not overfitted

# The company intends to use the classifier developed in (b) to identify potential low-quality products during the
# upcoming production batch, which will consist of 1000 food items.

# c) How much should be budgeted for the cost of the laboratory quality tests?

c_high_low <- 100000
c_low_high <- 500

1000*(0.999*500*8/30 + 0.001*100000*9/30 + 0.001*500*21/30) # 163550

# d) Compared to the previous strategy of conducting accurate laboratory tests on all products, does the new two-
# fold testing approach lead to a lower expected cost of misclassification? How much would be saved for this
# upcoming production batch?
1000*500 - 163550 # 336450

# По логие только low quality будут повторно проверены в лаборатории 



