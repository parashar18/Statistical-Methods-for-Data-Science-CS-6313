#getting inputs from csv file
data=read.csv('C:/Users/Parashar Parikh/Desktop/UTD/Sem3/stats/Miniprojects/Miniproject_6/prostate_cancer.csv')

#storing all data
psa=data[,2]
cancervol=data[,3]
weight=data[,4]
age=data[,5]
benpros=data[,6]
vesinv=data[,7]
capspen=data[,8]
gleason=data[,9]

#Exploratory Analysis of PSA Feature
#Histogram
hist(psa, xlab="PSA Level",main= "Histogram of PSA Level",breaks=20)

#Q-Q Plots
qqnorm(psa)
qqline(psa)

#Boxplot 
boxplot(psa)

#we can see from box plot that psa has lot of outliers so we need to tranform it using log transformation
#Boxplot of transformed psa level (log(psa))
boxplot(log(psa))

#now we are finding corrilation between features regarding our predictor
trandata=data
trandata$psa=log(psa)
cor(trandata[,2:9])

#transforming psa in log(psa)
logpsa=log(psa)

#we can visualize pair plot for linear dependencies based on good corrilation  
pairs(~logpsa + cancervol + capspen + vesinv + gleason + benpros)

# Drawing scatterplots of each variables (except categorical) with log(psa).
plot(cancervol, logpsa, xlab="Cancer Volume",ylab="Log of PSA level")
abline(lm(logpsa ~ cancervol))

plot(weight, logpsa,xlab="Weight", ylab="Log of PSA level")
abline(lm(logpsa ~ weight))

plot(age, logpsa,xlab="Age", ylab="Log of PSA level")
abline(lm(logpsa ~ age))

plot(benpros, logpsa,xlab="Benign prostatic hyperplasia", ylab="Log of PSA level")
abline(lm(logpsa~ benpros ))

plot(capspen, logpsa,xlab="Capsular penetration",ylab="Log of PSA level")
abline(lm(logpsa ~ capspen))




# cheking for all features.
fit1 <- lm(logpsa ~ cancervol + weight + age + benpros + capspen)
fit1

fit2 <- lm(logpsa ~ cancervol + benpros+ capspen)
fit2

# Compare first two models.
anova(fit1, fit2)

# Apply stepwise selection.
# Forward selection based on AIC.
fit3.forward <-step(lm(logpsa ~ 1), scope = list(upper = ~ cancervol + weight + age + benpros + capspen), direction = "forward")
fit3.forward

# Backward elimination based on AIC.
fit3.backward <- step(lm(logpsa ~ cancervol + weight + age + benpros + capspen), scope = list(lower = ~1), direction = "backward")
fit3.backward

# Both forward/backward selection.
fit3.both <- step(lm(logpsa ~ 1),scope = list(lower = ~1, upper = ~ cancervol + weight + age + benpros + capspen), direction = "both")
fit3.both

# Model selected bases on analysis
fit3 <- lm(logpsa ~ cancervol + benpros)

summary(fit3)

# Compare the model with the guess one.
anova(fit3, fit2)

#now we add qualitative (categorical) variables
fit4 <- update(fit3, . ~ . + factor(vesinv))
fit5 <- update(fit3, . ~ . + factor(gleason))

# Comparing two categorical variables.
summary(fit4)
anova(fit3, fit4)

summary(fit5)
anova(fit3, fit5)

# Finalize the model using exploratory analysis
fit6 <- update(fit3, . ~ . + factor(vesinv) + factor(gleason))
summary(fit6)


# Apply stepwise selection and comparing our model . Forward selection based on AIC for all features
fit7.forward <-step(lm(logpsa ~ 1), scope = list(upper = ~ cancervol + weight + age + benpros + capspen+ as.factor(vesinv)+ as.factor(gleason)), direction = "forward")
fit7.forward

# Backward elimination based on AIC
fit7.backward <- step(lm(logpsa ~ cancervol + weight + age + benpros + capspen+ as.factor(vesinv)+ as.factor(gleason)), scope = list(lower = ~1), direction = "backward")
fit7.backward

# Both forward/backward
fit7.both <- step(lm(logpsa ~ 1),scope = list(lower = ~1, upper = ~ cancervol + weight + age + benpros + capspen + as.factor(vesinv)+ as.factor(gleason)), direction = "both")
fit7.both


#So our model is similar as step wise model so we will continue with model fit6 as final model

# Residual plot of fit6.
plot(fitted(fit6), resid(fit6))
abline(h = 0)

# Plot the absolute residual of fit3.
plot(fitted(fit6), abs(resid(fit6)))

# Plot the times series plot of residuals
plot(resid(fit6), type="l")
abline(h = 0)

# Normal QQ plot of fit6
qqnorm(resid(fit6))
qqline(resid(fit6))

# functino to get mode 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Predict the PSA level for predictors having valuesof sample means and categorical predictors at their most frequent label
prediction=predict(fit6, data.frame(cancervol = mean(cancervol),
                           benpros   = mean(benpros),
                           vesinv    = getmode(vesinv),
                           gleason   = getmode(gleason)))

# since our respnse variable is log(psa)
exp(prediction)
