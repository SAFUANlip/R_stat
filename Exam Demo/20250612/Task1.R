library(nlmeU) ## --> for the dataset
library(nlme)  ## --> for models implementation

library(corrplot)
library(lattice)
library(plot.matrix)

library(nlmeU)
library(corrplot)
library(nlme)
library(lattice)
library(plot.matrix)
library(lme4)
library(insight)


data <- read.table("plants_mood.txt", h=TRUE)

# all groups has 100 
for (i in 1:20){
  print(sum(data["zone_id"] == 7))
}

data <- data.frame(
  mood = data["mood"],
  plants = data["plants"],
  light = data["light"],
  social = data["social"],
  zone_id = data["zone_id"],
  num_part = as.factor(rep(100, 20*100))
)

lme(mood ~ plants + light + social, 
    data = data)

lm_model <- lm(mood ~ plants + light + social, data)
summary(lm_model)



VarCorr(lm_model)

lme_model <- lme(mood ~ plants + light + social, data)
summary(lm)
