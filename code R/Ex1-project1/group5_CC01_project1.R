#Project 1, ex1
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(readxl)
data1 <- read_excel("C:/Users/HP/Desktop/ass_Prob&Stat/code R/Ex1-project1/data1.xlsx")
View(data1)
data1.lm <- lm(Y ~ X, data = data1)
summary(data1.lm)
data1.graph <- ggplot(data1, aes(x=X, y=Y)) + geom_point()
data1.graph <- data1.graph + geom_smooth(formula = y ~ x, method="lm", col="red")
data1.graph <- data1.graph + stat_regline_equation(label.x = 150, label.y = 350)
data1.graph + theme_bw() + labs(title = "Report", x = "X", y = "Y")


#Project 1, ex2
library(readxl)
data <- read_excel("C:/Users/HP/Desktop/ass_Prob&Stat/code R/Ex2-project1/data.xlsx")
View(data)
  #Sound
m1Sound <- data$Sound...2
m2Sound <- data$Sound...6
sd1Sound <- sd(data$Sound...2)
sd2Sound <- sd(data$Sound...6)
num1 = 5
num2 = 5
seSound = sqrt(sd1Sound*sd1Sound/num1 + sd2Sound*sd2Sound/num2)
t_Sound = (m1Sound - m2Sound)/seSound
averageP_Sound <- pt(t_Sound, df = pmin(num1, num2) - 1)
averageP_Sound

mean(averageP_Sound)

#Light
m1Light <- data$Light...3
m2Light <- data$Light...7
sd1Light <- sd(data$Light...3)
sd2Light <- sd(data$Light...7)
seLight = sqrt(sd1Light*sd1Light/num1 + sd2Light*sd2Light/num2)
t_Light = (m1Light - m2Light)/seLight
averageP_Light <- pt(t_Light, df = pmin(num1, num2) - 1)
averageP_Light

mean(averageP_Light)

#Pulse
m1Pulse <- data$Pulse...4
m2Pulse <- data$Pulse...8
sd1Pulse <- sd(data$Pulse...4)
sd2Pulse <- sd(data$Pulse...8)
sePulse <- sqrt(sd1Pulse*sd1Pulse/num1 + sd2Pulse*sd2Pulse/num2)
t_Pulse = (m1Pulse - m2Pulse)/sePulse
averageP_Pulse <- pt(t_Pulse, df = pmin(num1, num2) - 1)
averageP_Pulse

mean(averageP_Pulse)



#Project 1, ex3
no.company = matrix(c(20,52,32,53,47,28,67,32,25), ncol = 3 , byrow = TRUE)
colnames(no.company) = c("High","Moderate","Low")
rownames(no.company) = c("Small","Medium","Large")
no.company = as.table(no.company)
Chi_test = chisq.test(no.company, simulate.p.value = TRUE)
print(Chi_test)

#Project 1, ex4
monday = c(5,4,5,7)
tuesday = c(4,5,3,2)
wednesday = c(4,3,4,5)
thursday = c(4,4,3,2)
x =c(monday,tuesday,wednesday,thursday)
group = c(rep("monday",4) ,rep("tuesday",4) ,rep("wednesday",4) ,rep("thursday",4))
dat = data.frame(x,group)
print(dat)
av = aov(x~as.factor(group))
print(summary(av))

