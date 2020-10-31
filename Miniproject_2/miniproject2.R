data = read.csv('C:/Users/Parashar Parikh/Desktop/UTD/Sem3/stats/Miniprojects/Miniproject_2/roadrace.csv')
barplot(table(data$Maine),main="Road Race",ylab="Number of participants",col=c("darkblue","red"))
summary(data$Maine)

#selecting maine runners
maine = subset(data, Maine == "Maine")

#histogram
hist(maine[,12],xlab = "Runners time", main="Runners time of Maine",col=c("red"))

#Summary statistics of runtimes of Runners from Maine
summary(maine[,12])

#Inter quatertile range of runtime of runners from Maine
IQR(maine[,12])

#Standard Deviation of runtime of runners from Maine
sd(maine[,12])

#Range of the runtimes of runners from Maine
range = max(maine[,12])-min(maine[,12])


#selecting away candidates
away = subset(data, Maine == "Away")

#histogram
hist(away[,12],xlab = "Runners time", main="Runners time of Away",col=c("darkblue"))

#Summary Statistics of runtimes of runners for away runners
summary(away[,12])

#Inter quatertile range of runtime of runners for away runners
IQR(away[,12])

##Standard Deviation of runtime of runners for away runners
sd(away[,12])

##Range of the runtimes of runners foraway runners
range = max(away[,12])-min(away[,12])

#box plot for Maine and Away side by side
boxplot(maine[,12],away[,12],names=c("Maine","Away"),col=c("red","darkblue"),horizontal=TRUE,
        main="Box Plot of Runners from Maine and Away",xlab="Run time in minutes")

#selecting male  and female runners
male = subset(data, Sex== "M")
female = subset(data, Sex== "F")

#histogram
boxplot(as.numeric(male[,5]),as.numeric(female[,5]),names=c("Male","Female"),col=c("lightblue","pink"),horizontal=TRUE,
        main="Box Plot of Male and Female Runners",xlab="Age")

#Summary statistics of Male runner
summary(as.numeric(male[,5]))

#Inter quatertile range of Male runners 
IQR(as.numeric(male[,5]))

#Standard Deviation of Male runners 
sd(as.numeric(male[,5]))

#Range of the runtimes male
range = max(as.numeric(male[,5]))-min(as.numeric(male[,5]))
range
#Summary statistics of female runner
summary(as.numeric(female[,5]))

#Inter quatertile range of female runners 
IQR(as.numeric(female[,5]))

#Standard Deviation of female runners 
sd(as.numeric(female[,5]))

#Range of the runtimes female
range = max(as.numeric(female[,5]))-min(as.numeric(female[,5]))
range


data = read.csv('C:/Users/Parashar Parikh/Desktop/UTD/Sem3/stats/Miniprojects/Miniproject_2/motorcycle.csv')
#Summary statistics of fatal number of accidents
summary(data[,2])

#Plotting box plot for Fatal number of accidents
boxplot(data[,2],horizontal=TRUE,xlab="Number of Fatal Accidents",main="Box plot of
        Fatal Accidents in South Carolina",col='red')

#Summary statistics of fatal number of accidents
summary = fivenum(data[,2])
summary
IQR=IQR(as.numeric(data[,2]))
#Calculating the outlier range
outlierLower = summary[2] - 1.5 * IQR
outlierHigher = summary[4] + 1.5 * IQR
##Finding the outlier
outlier = subset(data, data[,2] < outlierLower | data[,2] > outlierHigher)
outlier
