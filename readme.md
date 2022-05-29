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
untuk memvisualisasikan data yang dilakukan adalah memasukan data x dan y ke dalam data frame. `R languange` mempunyai function untuk menghitung standar deviasi, `sd()`. standar deviasi selisih data x dan y yang dihitung.
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
**p-value dengan distribusi t** dihitung dengan rumus berikut dengan **s** merupakan standar deviasi sampel dan sampel yang diambil adalah data pertama sampai ke-enam <br/>
![image](https://user-images.githubusercontent.com/73109893/170882712-0b11b8bf-29c7-40fd-9303-42618977db6f.png)

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

#### Nomor 4
- Buatlah masing masing jenis spesies menjadi 3 subjek "Grup" (grup 1,grup 2,grup 3). Lalu Gambarkan plot kuantil normal untuk setiap kelompok dan lihat apakah ada outlier utama dalam homogenitas varians.
</br>
Langkah pertama mengambil data dari link yang telah disediadakan

```
myFile  <- read.table(url("https://rstatisticsandresearch.weebly.com/uploads/1/0/2/6/1026585/onewayanova.txt")) 
dim(myFile)
head(myFile)
```

Selanjutnya membuat myFile menjadi group 
```
myFile$Group <- as.factor(myFile$Group)
myFile$Group = factor(myFile$Group,labels = c("Kucing Oren","Kucing Hitam","Kucing Putih"))
```

Setelah itu, dicek apakah dia menyimpan nilai di groupnya
```
class(myFile$Group)
```

Lalu bagi tiap valuer menjadi 3 bagian ke 3 grup
```
group1 <- subset(myFile, Group=="Kucing Oren")
group2 <- subset(myFile, Group=="Kucing Hitam")
group3 <- subset(myFile, Group=="Kucing Putih")
```
- carilah atau periksalah Homogeneity of variances nya , Berapa nilai p yang didapatkan? , Apa hipotesis dan kesimpulan yang dapat diambil ?

Mencari Homogeneity of variances bisa menggunakan command sebagai berikut
```
bartlett.test(Length~Group, data=dataoneway)
```
Setelah di jalankan maka nilai p-value = 0.8054. 
Kesimpulan yang didapatkan yaitu Bartlett's K-squared memiliki nilai sebesar 0.43292 dan df bernilai 2
-  Untuk uji ANOVA (satu arah), buatlah model linier dengan Panjang versus Grup dan beri nama model tersebut model 1.

```
qqnorm(group1$Length)
qqline(group1$Length)
```

![image](https://user-images.githubusercontent.com/70510279/170848819-3b70668f-ba55-4d57-b297-a14cb7d7218a.png)

- Dari Hasil Poin C, Berapakah nilai-p ? , Apa yang dapat Anda simpulkan dari H0?
Setelah di jalankan maka nilai p-value = 0.8054. 

- Verifikasilah jawaban model 1 dengan Post-hoc test Tukey HSD, dari nilai p yang didapatkan apakah satu jenis kucing lebih panjang dari yang lain? 3 Jelaskan.
Langkah pertama adalah menggunakan command ANOVA
```
model1 <- lm(Length~Group, data=myFile)
```
Selanjutnya menggunakan command 
```
anova(model1)
```
Lalu menggunakan model Post-hoc Tukey HSD sebagai berikut
```
TukeyHSD(aov(model1))
```

- Visualisasikan data dengan ggplot2
```
library(ggplot2)
ggplot(dataoneway, aes(x = Group, y = Length)) + geom_boxplot(fill = "grey80", colour = "black") + scale_x_discrete() + xlab("Treatment Group") +  ylab("Length (cm)")
```


### Nomor 5
- Buatlah plot sederhana untuk visualisasi data

Run semua library yang diperlukan
```
install.packages("multcompView")
library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)
```

Selanjutnya membaca file GTL.csv dari documents
```
GTL <- read_csv("GTL.csv")
head(GTL)
```
![image](https://user-images.githubusercontent.com/70510279/170851339-6020c531-8d07-4a20-a9ab-4db04a1110e0.png)

</br>

Lakukan observasi pada data
```
str(GTL)
```
![image](https://user-images.githubusercontent.com/70510279/170851373-4512e70e-81ed-4a12-bf5e-5408d4403678.png)
</br>

Selanjutnya lakukan viasualisasi menggunakan simple plot yaitu sebagai berikut
```
qplot(x = Temp, y = Light, geom = "point", data = GTL) +
  facet_grid(.~Glass, labeller = label_both)
```
![image](https://user-images.githubusercontent.com/70510279/170851403-3b91fe4f-ab41-4b3e-8aca-066a27607971.png)

- Lakukan uji ANOVA dua arah
Langkah pertama adalah membuat variabel as factor sebagai ANOVA
```
GTL$Glass <- as.factor(GTL$Glass)
GTL$Temp_Factor <- as.factor(GTL$Temp)
str(GTL)
```
![image](https://user-images.githubusercontent.com/70510279/170851438-509ae870-a9a1-420e-adb9-3239f6a6dfb6.png)

</br>

Selanjutnya melakukan analisis of variance (aov) yaitu sebagai berikut 
```
anova <- aov(Light ~ Glass*Temp_Factor, data = GTL)
summary(anova)
```
![image](https://user-images.githubusercontent.com/70510279/170851507-b318c577-8c71-4a3c-b391-1c406e364abb.png)

- Tampilkan tabel dengan mean dan standar deviasi keluaran cahaya untuk setiap perlakuan (kombinasi kaca pelat muka dan suhu operasi)

Menggunakan `group_by` lalu melakukan `summarise` sesuai mean dan standar deviasi yang berlaku sehingga scriptnya adalah sebagai berikut
```
data_summary <- group_by(GTL, Glass, Temp) %>%
  summarise(mean=mean(Light), sd=sd(Light)) %>%
  arrange(desc(mean))
print(data_summary)
```
![image](https://user-images.githubusercontent.com/70510279/170851578-fee77749-6fff-4abf-ad36-62ef2ec84c3d.png)

</br>

- Lakukan uji Tukey

Menggunakan fungsi `TukeyHSD` sebagai berikut
```
tukey <- TukeyHSD(anova)
print(tukey)
```
![image](https://user-images.githubusercontent.com/70510279/170851658-f097be04-5017-404e-99b6-0ebdebb284d9.png)
![image](https://user-images.githubusercontent.com/70510279/170851669-260742aa-75b0-47e2-9d8a-dabf318b5082.png)

- Gunakan compact letter display untuk menunjukkan perbedaan signifikan antara uji Anova dan uji Tukey

Awalnya yaitu membuat compact letter display sebagai berikut
```
tukey.cld <- multcompLetters4(anova, tukey)
print(tukey.cld)
```
![image](https://user-images.githubusercontent.com/70510279/170851727-729875df-5aaf-4897-b97f-08b91127347e.png)

</br>
Tambahkan compact letter display tersebut ke tabel dengan means(rata-rata) dan sd

```
cld <- as.data.frame.list(tukey.cld$`Glass:Temp_Factor`)
data_summary$Tukey <- cld$Letters
print(data_summary)
```
![image](https://user-images.githubusercontent.com/70510279/170851749-d1e4fd97-1020-4f52-bb1a-7d020a508093.png)
