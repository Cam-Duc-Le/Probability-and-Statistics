library(readxl)
data <- read_excel("C:/Users/ASUS/Desktop/THIEN/Studying/Statistics/data.xlsx")

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

