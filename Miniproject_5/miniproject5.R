#First question

#getting data from csv file
data = read.csv('C:/Users/Parashar Parikh/Desktop/UTD/Sem3/stats/Miniprojects/Miniproject_5/bodytemp-heartrate.csv')

#Body Temperature male and female
body_temp=data["body_temperature"]
heart_rate=data["heart_rate"]
body_temp_male = data[which(gender == 1),"body_temperature"]
body_temp_female = data[which(gender == 2), "body_temperature"]

#summary statistics for male
summary(body_temp_male)

#summary statistics for female
summary(body_temp_female)

#boxplot comparitions for male and female body_temp
boxplot(body_temp_male,body_temp_female,main="Body Temperatures", names=c('male','female'), ylab="Temperature" )

#qq plot for male body temperature
qqnorm(body_temp_male, main = "QQPlot for body temperarture for male")
qqline(body_temp_male)

#qq plot for female body temperature
qqnorm(body_temp_female, main = "QQPlot for body temperarture for female")
qqline(body_temp_female)

#t test
t.test(body_temp_male, body_temp_female, alternative = "two.sided", conf.level = 0.95,var.equal = FALSE)

#Heart Rate male and female
heart_rate_male = data[which(gender == 1), "heart_rate"]
heart_rate_female =data[which(gender == 2), "heart_rate"]

#summary statistics for male heartrate
summary(heart_rate_male)
#summary statistics for female heartrate
summary(heart_rate_female)

#boxplot comparitions for male and female heart rate
boxplot(heart_rate_male,heart_rate_female,main="Heart Rates", names=c('male','female'), ylab="Heart Rate" )

#qq plot for male heart rate
qqnorm(heart_rate_male, main = "QQPlot for body temperarture for male")
qqline(heart_rate_male)

#qq plot for female heart rate
qqnorm(heart_rate_female, main = "QQPlot for body temperarture for female")
qqline(heart_rate_female)

#t test
t.test(heart_rate_male, heart_rate_female, alternative = "two.sided", conf.level = 0.95,var.equal = FALSE)

#correlation coefficient between body_temperature and heart_rate for both genders
cor(body_temp, heart_rate)

# Scatter plots for heart rate and body temperature for both genders
plot(data$body_temperature~data$heart_rate, xlab="Body temperature",ylab="Heart rate", main= "Both genders")
abline(lm(data$body_temperature~data$heart_rate))

# correlation coefficient between body_temperature and heart_rate for male
cor(body_temp_male,heart_rate_male)

# Scatter plots for heart rate and body temperature for male
plot(body_temp_male~heart_rate_male, xlab="Body temperature",ylab="Heart rate", main="Male")
abline(lm(body_temp_male~heart_rate_male))

# correlation coefficient between body_temperature and heart_rate for female
cor(body_temp_female,heart_rate_female)

# Scatter plots for heart rate and body temperature for female
plot(body_temp_female~heart_rate_female, xlab="Body temperature",ylab="Heart rate", main="Female")
abline(lm(body_temp_female~heart_rate_female))


#Second question


# For z-interval estimation
z_conf_interval = function(n,lambda){
  d = rexp(n,lambda)
  conf_int=mean(d) +c(-1, 1)*qnorm(1 - (0.05/2))*sd(d)/sqrt(n)
  return (conf_int)
}
n=5
lambda=0.01
z_conf_int=z_conf_interval(n,lambda)
z_conf_int

#Confidence interval using parametric bootstrap percentile interval

boot_conf_interval = function(d) {
  n =length(d)
  xbar = mean(d)
  xstar =rexp(n, 1/xbar)
  conf_int = mean(xstar)
  return(conf_int)
}
n=5
lambda=0.01
d = rexp(n,lambda)
num_sim = 1000
#Distribution of sample mean using parametric bootstrap
boot_strap = replicate(num_sim, boot_conf_interval(d))
#using parametric percentile bootstrap 95 % CI 
boot_conf_int = sort(boot_strap)[c(25, 975)]
boot_conf_int

#Estimate of coverage probability

conf_intervals = function(n, lambda) {
  d = rexp(n, lambda)
  #z confidence interval:
  z_conf_int =mean(d) + c(-1, 1)*qnorm(1 - (0.05/2))*sd(d)/sqrt(n)
  #confidence interval using parametric percentile bootstrap interval
  num_sim = 1000
  boot_strap = replicate(num_sim, boot_conf_interval(d))
  boot_conf_int=sort(boot_strap)[c(25, 975)]
  return(c(z_conf_int, boot_conf_int))
}
n = 5
lambda = 0.01
mean_exp = 1/lambda
#repeating the process 5000 times
combine = replicate(5000, conf_intervals(n,lambda))
coverage_prob_z_conf_int = mean( (mean_exp >= combine[1,])*(mean_exp <= combine[2,]) )
coverage_prob_z_conf_int

coverage_prob_boot_conf_int = mean( (mean_exp >= combine[3,])*(mean_exp <= combine[4,]) )
coverage_prob_boot_conf_int


#Number of simulations
num_sim = 5000
#Sample size
n = c(5,10,30,100)
#Lambda values
lambda = c(0.01,0.1,1,10)

combination = expand.grid(n1,lambda)
for (i in n){
  for (j in lambda){
    mean_exp = 1/j
    combine = replicate(num_sim, conf_intervals(i,j))
    coverage_prob_z_conf_int = mean( (mean_exp >= combine[1,])*(mean_exp <= combine[2,]) )
    coverage_prob_boot_conf_int = mean( (mean_exp >= combine[3,])*(mean_exp <= combine[4,]) )
    result = ((c(i,j,coverage_prob_z_conf_int,coverage_prob_boot_conf_int)))
    print(result)
  }
}


#Number of simulations
num_sim = 5000
#Sample size
n = c(5)
#Lambda values
lambda = c(0.01,0.1,1,10)

combination = expand.grid(n1,lambda)
for (i in n){
  for (j in lambda){
    mean_exp = 1/j
    combine = replicate(num_sim, conf_intervals(i,j))
    coverage_prob_z_conf_int = mean( (mean_exp >= combine[1,])*(mean_exp <= combine[2,]) )
    coverage_prob_boot_conf_int = mean( (mean_exp >= combine[3,])*(mean_exp <= combine[4,]) )
    result = ((c(i,j,coverage_prob_z_conf_int,coverage_prob_boot_conf_int)))
    print(result)
  }
}



