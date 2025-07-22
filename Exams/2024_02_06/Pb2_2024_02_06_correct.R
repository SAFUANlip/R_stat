library(MASS)
library(MVN)
library(car)
library(heplots)

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

# So all groups have same covariance structure, but not MVN, -> FDA,
# BUT, LDA is robust to violations of the normality assumption -> will use LDA

c_high_low <- 100000
c_low_high <- 500

prior_low <- 0.001
prior_high <- (1-0.001)

prior_adj_low <- prior_low*c_high_low/(prior_low*c_high_low + prior_high*c_low_high)
prior_adj_high <- prior_high*c_low_high/(prior_low*c_high_low + prior_high*c_low_high)

prior_adj_low
prior_adj_high

# b) Build the corresponding classifier, providing its apparent error rate and an estimate of its actual error rate
# through leave-one-out cross-validation. What do you observe? Which characteristic of the classifier you are
# using is being highlighted here?

lda.m_cv <- lda(food_data, food$result, prior=c(prior_adj_high, prior_adj_low), CV=TRUE) # for AER
lda.m_cv
lda.m <- lda(food_data, food$result, prior=c(prior_adj_high, prior_adj_low)) # for APER
lda.m

# Prior probabilities of groups:
#   high       low 
# 0.8331943 0.1668057

# APER -------------------------------------------------------------------------
# APER - compute on training data
Lda.s <- predict(lda.m)
table(class.true=food$result, classe.assigned=Lda.s$class)

#            classe.assigned
# class.true high low
# high        28   2
# low         16  14

n_high <- sum(food$result == "high")
n_low <- sum(food$result == "low")

APER <- 2*prior_high/n_high + 16*prior_low/n_low
APER       # 0.06713333

# AER --------------------------------------------------------------------------
# AER - compute on validation data (or on Cross validation)
table(class.true=food$result, class.assignedCV=lda.m_cv$class)
#            class.assignedCV
# class.true high low
# high         28   2
# low         16  14

AER <- 2*prior_high/n_high + 16*prior_low/n_low
AER      # 0.06713333 = 0.999*2/30 + 0.001*16/30

# AER  equal to APER - it means that we are not overfitted and have good fitted model

# The company intends to use the classifier developed in (b) to identify potential low-quality products during the
# upcoming production batch, which will consist of 1000 food items.

# c) How much should be budgeted for the cost of the laboratory quality tests? (It's column "low")
t <- table(class.true=food$result, class.assignedCV=lda.m_cv$class)
t

ptd <- t['low', 'low']/sum(t['low',]) * prior_low + t['high', 'low']/sum(t['high',]) * prior_high
ptd
ptd*1000*500 # 33533.33 # - correct

1000*(500*prior_high*2/30 + 500*prior_low*14/30) # 33533.33 # - correct (just my formula)

# 1000*(500*prior_high*2/30 + 100000*prior_low*16/30 + 500*prior_low*14/30)

# d) Compared to the previous strategy of conducting accurate laboratory tests on all products, does the new two-
#   fold testing approach lead to a lower expected cost of misclassification? How much would be saved for this
# upcoming production batch?
1000*500 - 1000*(500*prior_high*2/30 + 500*prior_low*14/30) # 466466.7

