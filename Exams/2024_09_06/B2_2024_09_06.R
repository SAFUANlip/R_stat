library(MVN)
library(car)
library(heplots)


# A healthcare technology company is developing a system for monitoring patients with chronic conditions remotely.
# To provide timely interventions and prevent complications, the company is implementing a predictive model to
# assess the risk of a patient’s condition worsening. Instead of relying solely on periodic in-person check-ups, which
# can be infrequent and costly, the company is introducing continuous monitoring through wearable devices. This
# system will collect real-time data on patients’ vital signs and activity levels, allowing for early detection of potential
# health risks. Patients flagged as high-risk will receive immediate medical attention.
# The system captures several key variables: hr, representing the patient’s resting heart rate; activity, measuring
# daily physical activity; sleep, indicating the quality of sleep; and spo2, tracking blood oxygen saturation. The file
# health.txt contains the average system measurements for 60 monitored individuals over a week. Additionally,
# the variable risk indicates whether the patient required urgent medical intervention in the subsequent week (yes if medical intervention).

# The objective is to build a classifier that can predict which patients are at high risk of needing urgent care
# based on the four health indicators mentioned above.

# The cost of providing an urgent medical intervention is $500 per patient, which is equivalent to the value of
# accurately identifying a patient who needs it. If a high-risk patient is not identified and subsequently experiences
# a serious health event, the estimated economic loss is $100,000. The company estimates that 0.1% of patients
# monitored will be at significant risk. The classifier should be optimal in minimizing the expected cost of misclassification.

health <-read.table("2024_09_06/health.txt", h=TRUE)
health$risk
str(health)
sum(health$risk == "no")
sum(health$risk == "yes")

health_data <-health[,1:4]

# a) Which classification method would you use? Verify the underlying assumptions.
mvn(health_data[health$risk == "yes", ])
mvn(health_data[health$risk == "no", ])

# each group is MVN -> not FDA
boxM(health_data, health$risk) # same covariance structure => not QDA => LDA

yes_index <- which(health$risk == "yes")
no_index <- which(health$risk == "no")

S_yes <-  cov(health_data[yes_index,])
S_no <-  cov(health_data[no_index,])

# No coefficient of the covariance matrix should differ from more than a factor ~10 across groups
cat("Maximum factor of features across group", max(abs(S_yes/S_no)), "Has to be less than ~10", min(abs(S_yes/S_no)), "Has to be more than ~0.1")

# same covariance structure => not QDA => LDA

# b) Provide the costs of misclassification, the prior probabilities and the priors adjusted for considering the cost of
# misclassification.

c_no_yes <- 100000 # cost of misclassification c(no|yes) - when we said no, but it is yes (missed high-risk patients)
c_yes_no <- 500    # cost of misclassification c(yes|no) - when we said yes, but it is no (wrongly said that this patient high-risk)

p_yes <- 0.001     # "The company estimates that 0.1% of patients monitored will be at significant risk."
p_no <- 1 - p_yes  # patients not under risk

# prior adjusted
p_adj_yes <- p_yes*c_no_yes / (p_yes*c_no_yes + p_no*c_yes_no) 
p_adj_no <- p_no*c_yes_no / (p_yes*c_no_yes + p_no*c_yes_no)

# c) Build the corresponding classifier, providing its apparent error rate and an estimate of its actual error rate
# through leave-one-out cross-validation. What do you observe? Which characteristic of the classifier you are
# using is being highlighted here?

# When we build classifer, we have to use adjusted prior probailities, to take into account 
# effect of misclassification cost
# But when we will compute APER and AER we will use source priora prababilities
# NOTE if we have from text information about priorr probability - it will be source
# if no - we will use prior from provided data

lda.m_cv <- lda(health_data, health$risk, prior=c(p_adj_no, p_adj_yes), CV=TRUE) # for AER
lda.m_cv
lda.m <- lda(health_data, health$risk, prior=c(p_adj_no, p_adj_yes)) # for APER
lda.m

# Prior probabilities of groups:
#  no       yes 
# 0.8331943 0.1668057 
# 
# Group means:
#  heart_rate activity_level sleep_quality oxygen_level
# no    68.88967       5.942928      5.113091     97.99042
# yes   77.69220       4.790074      4.981370     96.92756
#
# Coefficients of linear discriminants:
#  LD1
# heart_rate      0.06751168
# activity_level -0.85152161
# sleep_quality  -0.12872378
# oxygen_level   -1.42860716

# APER -------------------------------------------------------------------------
# APER - compute on training data
Lda.s <- predict(lda.m)
table(class.true=health$risk, classe.assigned=Lda.s$class)
#          classe.assigned
# class.true   no yes
# no           28   2
# yes           4  26
n_no <- sum(health$risk == "no")
n_yes <- sum(health$risk == "yes")

APER <- 2*p_no/n_no + 4*p_yes/n_yes
APER       # 0.06673333

# AER --------------------------------------------------------------------------
# AER - compute on validation data (or on Cross validation)
table(class.true=health$risk, class.assignedCV=lda.m_cv$class)
#            class.assignedCV
# class.true no yes
# no         28   2
# yes         5  25

AER <- p_no*2/n_no + p_yes*5/n_yes
AER      # 0.06676667

# AER almost equal to APER - it means that we are not overfitted and have good fitted model

# The company plans to use the classifier developed in (b) to monitor a cohort of 1000 patients during the next
# monitoring period.

# Expected Misclassification Cost (EMC) -------------------------------------------------------
# d) How much should be budgeted for the cost of urgent medical interventions?
# generally we have to compute Expected Misclassification Cost for 1000 patients
# take misclassifications from APER table
# (c_yes_no*2*p_no/n_no + c_no_yes*4*p_yes/n_yes) * 1000 # 46633.33
# we will use table from AER 

1000*(500*p_no*2/30 + 500*p_yes*25/30) # 33716.67 - correct



1000*(0.999*500*2/30 + 0.001*100000*5/30 + 0.001*500*25/30) # 50383.33 - expected economical expenses
# (если бы пришлось учесть все затраты, даже на неправильно классифицированных)
# 0.001*500*25/30 - budget spending also for patients, who was correctly predicted as sick 

# (c_yes_no*2*p_no/n_no + c_no_yes*5*p_yes/n_yes) - EMC for one patient

# e) Compared to the previous strategy of providing interventions based on scheduled check-ups for all patients, does
# the new predictive monitoring approach lead to a lower expected cost of misclassification? How much would be
# saved for this cohort?

# "for all patients" - so everyone will get check-ups (treatment)
1000*500 - 33716.67 # 466283.3 - how much we saved 

# APER cycle computation ------------------------------------------------------------
prior <- c(p_no, p_yes)
G <- 2
misc <- table(class.true=health$risk, classe.assigned=Lda.s$class)
APER_cycle <- 0
for(g in 1:G){
  APER_cycle <- APER_cycle + sum(misc[g,-g])/sum(misc[g,]) * prior[g] 
}
APER_cycle # 0.06673333

# AER cycle computation ------------------------------------------------------------
prior <- c(p_no, p_yes)
G <- 2
misc_aer <- table(class.true=health$risk, class.assignedCV=lda.m_cv$class)
AER_cycle <- 0
for(g in 1:G){
  print(sum(misc_aer[g,-g])/sum(misc_aer[g,]) * prior[g])
  AER_cycle <- AER_cycle + sum(misc_aer[g,-g])/sum(misc_aer[g,]) * prior[g] 
}
AER_cycle # 0.0669

# When you want to calculate the expected error based on the confusion matrix,
# you should not use the modified prior probabilities anymore.
# You must use the original prior probabilities
# (i.e., the true class distribution in the real-world problem),
# because you’ve already adjusted the decision rule by using the modified priors during classification.
# If you also use those modified priors again when computing the error or risk,
# you would double-count the adjustment, 
# and so distort the evaluation with respect to the true class distribution.


