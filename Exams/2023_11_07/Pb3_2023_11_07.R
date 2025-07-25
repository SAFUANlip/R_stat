library(MASS)
library(car)
library(rgl)
library(glmnet)

students <- read.table("2023_11_07/students.txt", h=TRUE)

lm_model <- lm(
  watchtv ~ -1 + as.factor(gender) + age + height + distance + siblings + computertime + exercisehours + musiccds + playgames, 
  data=students)

summary(lm_model)

# Call:
#   lm(formula = watchtv ~ -1 + as.factor(gender) + age + height + 
#        distance + siblings + computertime + exercisehours + musiccds + 
#        playgames, data = students)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -10.1005  -3.6037  -0.2002   3.2055  12.4808 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# as.factor(gender)female  6.4329081  9.8143851   0.655 0.513265    
# as.factor(gender)male    6.8710519 10.4622929   0.657 0.512439    
# age                      0.1948575  0.1899515   1.026 0.306768    
# height                  -0.1077502  0.1337050  -0.806 0.421698    
# distance                 0.0005002  0.0001972   2.536 0.012312 *  
# siblings                 0.7107674  0.2644622   2.688 0.008082 ** 
# computertime             0.1901959  0.0562389   3.382 0.000937 ***
# exercisehours            0.0460659  0.0978123   0.471 0.638411    
# musiccds                 0.0033897  0.0025794   1.314 0.190982    
# playgames                0.1434152  0.1256614   1.141 0.255729    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.918 on 138 degrees of freedom
# Multiple R-squared:  0.7529,	Adjusted R-squared:  0.735 
# F-statistic: 42.05 on 10 and 138 DF,  p-value: < 2.2e-16


# b) Perform variable selection using the Lasso method, setting the regularization parameter to lambda = 0.3. Identify
# and report the significant coeffcients.

# Build the matrix of predictors
x <- model.matrix(watchtv ~ -1 + as.factor(gender) + age + height + distance + siblings + computertime + exercisehours + musiccds + playgames, data=students)#[,-1]
# Build the vector of response
y <- students$watchtv


fit.lasso <- glmnet(x, y, lambda = 0.3)


plot(fit.lasso, xvar='lambda',label=TRUE, col = rainbow(dim(x)[2]))
legend('topright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=1)

coef(fit.lasso) < 0.0001

# 11 x 1 Matrix of class "lgeMatrix"
# s0
# (Intercept)             FALSE
# as.factor(gender)female  TRUE
# as.factor(gender)male    TRUE
# age                     FALSE
# height                   TRUE
# distance                FALSE
# siblings                FALSE
# computertime            FALSE
# exercisehours            TRUE
# musiccds                FALSE
# playgames               FALSE

# So I would take these coefficients:
# (Intercept)             FALSE
# age                     FALSE
# distance                FALSE
# siblings                FALSE
# computertime            FALSE
# musiccds                FALSE
# playgames               FALSE

# c) For this question, the random state should be fixed 
# by inserting set.seed(20231108) at the beginning of the code.
# Optimize the regularization parameter lambda through 
# cross-validation within the interval [0.01, 10]. 
# Report the optimal lambda, the associated mean cross validation MSE (mean-squared error)
# and the value of the coeffcients for
# the optimal lambda.

grid <- seq(0.01, 10,length=1000)

lasso.mod <- glmnet(x,y,alpha=1,lambda=grid)

plot(lasso.mod, xvar='lambda',label=TRUE)

# choosing the parameter lambda
set.seed(20231108)
cv.out <- cv.glmnet(x,y,alpha=1,nfold=3,lambda=grid) 

plot(cv.out)

bestlam.lasso <- cv.out$lambda.min
bestlam.lasso
# log(bestlam.lasso)

abline(v=(bestlam.lasso))

plot(lasso.mod,xvar='lambda',label=TRUE)
abline(v=log(bestlam.lasso))

coef(cv.out, s = "lambda.min")
min(cv.out$cvm)

