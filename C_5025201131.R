############ Nomor 1
#a
Responden = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
x = c(78, 75, 67, 77, 70, 72, 78, 74, 77)
y = c(100, 95, 70, 90, 90, 90, 89, 90, 100)

Data_Frame = data.frame(Responden, x, y)

sdev = sd(Data_Frame$x-Data_Frame$y)
sdev

#b
h = Data_Frame$y - Data_Frame$x
t_mu = mean(h)
t_zbar = mean(h[1:6])
t_n = 6
t_s = sd(h[1:6])
t = (t_zbar - t_mu) / (t_s/sqrt(t_n))
t
pval = 2 * pt(-abs(t), df = t_n - 1)
pval

#c
install.packages("BSDA")
library(BSDA)
var.test(Data_Frame$x, Data_Frame$y)
t.test(Data_Frame$x, Data_Frame$y, var.equal = TRUE)

############ Nomor 2
#a & b
tsum.test(mean.x=23500, sd(3900), n.x=100)

#c
xbar2c = 23500
mu2c = 20000
sdev2c = 3900
n2c = 100
zval2c = (xbar2c - mu2c) / (sdev2c / sqrt(n2c))
zval2c
pval2c = 2 * pnorm(-abs(zval2c))
pval2c

############ Nomor 3
#a
xbarh0 = 3.64
sdevh0 = 1.67
n_h0 = 19
xbarh1 = 2.79
sdevh1 = 1.32
n_h1 = 27

z_h0 = (xbarh0 - 0) / (sdevh0 / sqrt(n_h0))
z_h0
z_h1 = (xbarh1 - 0) / (sdevh1 / sqrt(n_h1))
z_h1

#b
tsum.test(mean.x=3.64, s.x = 1.67, n.x = 19, mean.y =2.79 , s.y = 1.32, n.y = 27, alternative = "greater", var.equal = TRUE)

#c
curve(dt(x, df=2), from=-4, to=4)

#d
qt(p = 0.05, df = 2, lower.tail = FALSE)
#lower.tail = FALSE karena kalau TRUE minus

#e & f ada di readme

############ Nomor 4
#a

myFile  <- read.table(url("https://rstatisticsandresearch.weebly.com/uploads/1/0/2/6/1026585/onewayanova.txt"))
dim(myFile)
head(myFile)
attach(myFile)

myFile$V1 <- as.factor(myFile$V1)
myFile$V1 = factor(myFile$V1,labels = c("Kucing Oren","Kucing Hitam","Kucing Putih","Kucing Oren"))

class(myFile$V1)

group1 <- subset(myFile, V1=="Kucing Oren")
group2 <- subset(myFile, V1=="Kucing Hitam")
group3 <- subset(myFile, V1=="Kucing Putih")

#b
bartlett.test(Length~V1, data=dataoneway)

#c
qqnorm(group1$Length)
qqline(group1$Length)

#d di README

#e
model1 <- lm(Length~Group, data=myFile)

anova(model1)

TukeyHSD(aov(model1))

#f
library(ggplot2)
ggplot(dataoneway, aes(x = Group, y = Length)) + geom_boxplot(fill = "grey80", colour = "black") + 
  scale_x_discrete() + xlab("Treatment Group") +  ylab("Length (cm)")


############ Nomor 5
#a
install.packages("multcompView")
library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)

GTL <- read_csv("GTL.csv")
head(GTL)

str(GTL)

qplot(x = Temp, y = Light, geom = "point", data = GTL) +
  facet_grid(.~Glass, labeller = label_both)

#b
GTL$Glass <- as.factor(GTL$Glass)
GTL$Temp_Factor <- as.factor(GTL$Temp)
str(GTL)

anova <- aov(Light ~ Glass*Temp_Factor, data = GTL)
summary(anova)

#c
data_summary <- group_by(GTL, Glass, Temp) %>%
  summarise(mean=mean(Light), sd=sd(Light)) %>%
  arrange(desc(mean))
print(data_summary)

#d
tukey <- TukeyHSD(anova)
print(tukey)

#e
tukey.cld <- multcompLetters4(anova, tukey)
print(tukey.cld)

cld <- as.data.frame.list(tukey.cld$`Glass:Temp_Factor`)
data_summary$Tukey <- cld$Letters
print(data_summary)

write.csv("GTL_summary.csv")