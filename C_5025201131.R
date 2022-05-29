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
2 * pnorm(-abs(zval2c))

############ Nomor 3