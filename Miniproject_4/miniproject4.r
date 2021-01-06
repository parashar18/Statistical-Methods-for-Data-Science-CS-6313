# Reading data
data <- read.csv(file="C:/Users/Parashar Parikh/Desktop/UTD/Sem3/stats/Miniprojects/Miniproject_4/gpa.csv")
# gpa and act scatterplot
plot(data$gpa,data$act,xlab="GPA",ylab="ACT",main="Scatterplot of gpa and act")

# Correlation Coefficient for gpa and act 
cor(data$gpa,data$act)

# Bootstrap estimate of bias and standard error
library(boot)
func <- function(data, index){
  dt <- data[index,]
  return (cor(dt$gpa,dt$act))
}
BootStrap <- boot(data, func, R=999)
BootStrap
  

#Plotting bootstrap estimation
plot(BootStrap)

# 95% confidence interval for percentile bootstrap
ci <- boot.ci(BootStrap,index=1,type="perc")
ci


# Reading data
data = read.csv(file="C:/Users/Parashar Parikh/Desktop/UTD/Sem3/stats/Miniprojects/Miniproject_4/VOLTAGE.csv")
remote = subset(data, location == "0")
local = subset(data, location == "1")
remote = as.numeric(remote$voltage)
local = as.numeric(local$voltage)

#Histograms for data
hist(remote, main='Voltage of remote', xlab = 'Remote')
hist(local, main='Voltage of local', xlab = 'Local')

#Side by side boxplot of Remote and local locations
boxplot(remote,local,horizontal=TRUE,names=c("Remote","Local"),xlab="Locations")

#Length of remote and local
r_length = length(remote)
l_length = length(local)

#Minimum of remote and local
r_min = min(remote)
l_min= min(local)
r_min
l_min

#Maximum of remote and local
r_max = max(remote)
l_max = max(local)
r_max
l_max

#Mean of remote and local
r_mean = mean(remote)
l_mean = mean(local)
r_mean
l_mean

#Standard Deviation of remote and local
r_sd = sd(remote)
l_sd = sd(local)
r_sd
l_sd

sdofdifference = sqrt( (sd(remote)**2/length(remote)) + (sd(local)**2/length(local)) )
sdofdifference

#90% CI.(alpha=0.10, alpha/2=0.05, qnorm(1-0.05)
z = qnorm(.95)
lowerBound = mean(remote)-mean(local)- z*sdofdifference
upperBound = mean(remote)-mean(local)+ z*sdofdifference
lowerBound
upperBound  



# Read the data from csv
data = read.csv('C:/Users/Parashar Parikh/Desktop/UTD/Sem3/stats/Miniprojects/Miniproject_4/VAPOR.csv')
theoretical = as.numeric(data$theoretical)
experimental = as.numeric(data$experimental)

#Length of theoretical and experimental values
t_length = length(theoretical)
e_length = length(experimental)
t_length
e_length

#Minimum of theoretical and experimental values
t_min = min(theoretical)
e_min = min(experimental)
t_min
e_min

#Maximum of theoretical and experimental values
t_max = max(theoretical)
e_max = max(experimental)
t_max
e_max

#Mean of theoretical and experimental values
t_mean = mean(theoretical)
e_mean = mean(experimental)
t_mean
e_mean

#Standard deviation of theoretical and experimental values
t_sd = sd(theoretical)
e_sd = sd(experimental)
t_sd
e_sd


#Histogram of theoretical and experimental values
hist(theoretical, main='Theoretical vapor pressure', xlab='Vapor Pressures')
hist(experimental, main='Experimental vapor pressure', xlab='Vapor Pressures')

#Side by side boxplot of theoretical and experimental values
boxplot(theoretical, experimental,horizontal=TRUE,names=c("Theoretical","Experimental"))


sdofdifference = sqrt( ((t_sd**2)/length(theoretical)) + ((e_sd**2)/length(experimental)) )

#95% CI (alpha=0.05, alpha/2=0.025, qnorm(1-0.025))
z = qnorm(.975)
lowerBound = mean(theoretical)-mean(experimental)- z*sdofdifference
upperBound = mean(theoretical)-mean(experimental)+ z*sdofdifference
lowerBound
upperBound
