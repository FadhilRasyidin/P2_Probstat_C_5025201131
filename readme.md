## Praktikum Probstat Modul 2 [2022 / Modul 2]
- Mohammad Fadhil Rasyidin Parinduri
- 5025201131
- Probstat C // Dr. Bilqis Amaliah, S.Kom.,M.Kom.
----------------
#### Nomor 1
- Carilah Standar Deviasi dari data selisih pasangan pengamatan tabel diatas
```r
Responden = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
x = c(78, 75, 67, 77, 70, 72, 78, 74, 77)
y = c(100, 95, 70, 90, 90, 90, 89, 90, 100)

Data_Frame = data.frame(Responden, x, y)

sdev = sd(Data_Frame$x-Data_Frame$y)
sdev
```
- carilah nilai t (p-value)
```r
h = Data_Frame$y - Data_Frame$x
t_mu = mean(h)
t_zbar = mean(h[1:6])
t_n = 6
t_s = sd(h[1:6])
t = (t_zbar - t_mu) / (t_s/sqrt(t_n))
t
pval = 2 * pt(-abs(t), df = t_n - 1)
pval
```
- tentukanlah apakah terdapat pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan aktivitas 𝐴 jika diketahui tingkat signifikansi 𝛼 = 5% serta H0 : “tidak ada pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan aktivitas 𝐴”
```r
install.packages("BSDA")
library(BSDA)
var.test(Data_Frame$x, Data_Frame$y)
t.test(Data_Frame$x, Data_Frame$y, var.equal = TRUE)
```
#### Nomor 2
- Apakah Anda setuju dengan klaim tersebut? Jelaskan maksud dari output yang dihasilkan!
```r
tsum.test(mean.x=23500, sd(3900), n.x=100)
```
- Buatlah kesimpulan berdasarkan P-Value yang dihasilkan!
```r
xbar2c = 23500
mu2c = 20000
sdev2c = 3900
n2c = 100
zval2c = (xbar2c - mu2c) / (sdev2c / sqrt(n2c))
zval2c
pval2c = 2 * pnorm(-abs(zval2c))
pval2c
```

#### Nomor 3
- H0 dan H1
```r
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
```
- Hitung Sampel Statistik
```r
tsum.test(mean.x=3.64, s.x = 1.67, n.x = 19, mean.y =2.79 , s.y = 1.32, n.y = 27, alternative = "greater", var.equal = TRUE)
```
- Lakukan Uji Statistik (df =2)
```r
curve(dt(x, df=2), from=-4, to=4)
```
![image](https://user-images.githubusercontent.com/73109893/170881878-46981ea6-a580-47b7-acb0-4ac2775e4b01.png)

- Nilai Kritikal
```r
qt(p = 0.05, df = 2, lower.tail = FALSE)
#lower.tail = FALSE karena kalau TRUE minus
```
- Keputusan
analisis probabilitas adalah suatu penerapan teori pengambilan keputusan statistik, yang dalam kondisi ketidak pastian, mengarah kepada keputusan yang lebih konsisten dan dapat diandalkan dibandingkan dengan satu tebakan paling baik
- Kesimpulan
