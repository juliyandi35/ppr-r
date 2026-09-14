# Install and load the ppr package
library(stats)
library(readxl)
library(dplyr)
library(ggplot2)
library(Metrics)

RSquare = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}

# Generate synthetic data
data <- read_excel('Book1 Lingga Lingga.xlsx')
View(data)
data$Tanggal <- as.Date(data$Tanggal)
str(data)
data <- na.omit(data)

# Rata-rata, standar deviasi dan koefisien variasi
sapply(data[,-1], function(x) mean(x)) # Rata-rata
sapply(data[,-1], function(x) sd(x)) # Standar deviasi
sapply(data[,-1], function(x) sd(x) / mean(x) * 100) #Koefisien variasi

# Pembuatan boxplot
boxplot(data[,2:5],xlab="Variable")

# Plot time series
ggplot(data, aes(x=Tanggal, y=suhu)) +
  geom_line()
ggplot(data, aes(x=Tanggal, y=kelembapan)) +
  geom_line()
ggplot(data, aes(x=Tanggal, y=`curah hujan`)) +
  geom_line()
ggplot(data, aes(x=Tanggal, y=`kecepatan angin harian`)) +
  geom_line()

# Scatter plot
ggplot(data, aes(x=suhu, y=`kecepatan angin harian`)) +
  geom_point()
ggplot(data, aes(x=kelembapan, y=`kecepatan angin harian`)) +
  geom_point()
ggplot(data, aes(x=`curah hujan`, y=`kecepatan angin harian`)) +
  geom_point()

# Membagi data menjadi inSample dan outSample
set.seed(123)  # Ini digunakan untuk membuat pembagian menjadi acak tetap

# Buat indeks acak untuk data Anda
index_acak <- sample(1:nrow(data), nrow(data))

# Hitung jumlah baris yang akan menjadi bagian dari training set (80%)
jumlah_inSample <- round(0.8 * nrow(data))

# Bagi data menjadi training set dan testing set
inSample <- data[index_acak[1:jumlah_inSample], ]
outSample <- data[index_acak[(jumlah_inSample + 1):nrow(data)], ]
outSamplePredictor <- outSample[,-5]
# Standarisasi respon
data <- data %>% mutate_at(c('kecepatan angin harian'), ~(scale(.) %>% as.vector))
View(data)


# Fit a Projection Pursuit Regression model
# m = 1
model.ppr1 <- ppr(`kecepatan angin harian`~suhu+kelembapan+`curah hujan`,data = inSample,nterms=1)
summary(model.ppr1)
model.ppr1$alpha
model.ppr1$beta

# Prediksi
forecastedData1 <- predict(model.ppr1,newdata = outSamplePredictor)
forecastedData1 <- round(forecastedData1,digits = 0)
forecastedData1
write.csv(forecastedData1,"forecastedData1.csv")

# m = 2
model.ppr2 <- ppr(`kecepatan angin harian`~suhu+kelembapan+`curah hujan`,data = inSample,nterms=2)
summary(model.ppr2)
model.ppr2$alpha
model.ppr2$beta

# Prediksi
forecastedData2 <- predict(model.ppr2,newdata = outSamplePredictor)
forecastedData2 <- round(forecastedData2,digits = 0)
forecastedData2
write.csv(forecastedData2,"forecastedData2.csv")

# m = 3
model.ppr3 <- ppr(`kecepatan angin harian`~suhu+kelembapan+`curah hujan`,data = inSample,nterms=3)
summary(model.ppr3)
model.ppr3$alpha
model.ppr3$beta

# Prediksi
forecastedData3 <- predict(model.ppr3,newdata = outSamplePredictor)
forecastedData3 <- round(forecastedData3,digits = 0)
forecastedData3
write.csv(forecastedData3,"forecastedData3.csv")

# m = 4
model.ppr4 <- ppr(`kecepatan angin harian`~suhu+kelembapan+`curah hujan`,data = inSample,nterms=4)
summary(model.ppr4)
model.ppr4$alpha
model.ppr4$beta

# Prediksi
forecastedData4 <- predict(model.ppr4,newdata = outSamplePredictor)
forecastedData4 <- round(forecastedData4,digits = 0)
forecastedData4
write.csv(forecastedData4,"forecastedData4.csv")

# m = 5
model.ppr5 <- ppr(`kecepatan angin harian`~suhu+kelembapan+`curah hujan`,data = inSample,nterms=5)
summary(model.ppr5)
model.ppr5$alpha
model.ppr5$beta

# Prediksi
forecastedData5 <- predict(model.ppr5,newdata = outSamplePredictor)
forecastedData5 <- round(forecastedData5,digits = 0)
forecastedData5
write.csv(forecastedData5,"forecastedData5.csv")

rmse1 <- rmse(outSample$`kecepatan angin harian`, forecastedData1)
rmse2 <- rmse(outSample$`kecepatan angin harian`, forecastedData2)
rmse3 <- rmse(outSample$`kecepatan angin harian`, forecastedData3)
rmse4 <- rmse(outSample$`kecepatan angin harian`, forecastedData4)
rmse5 <- rmse(outSample$`kecepatan angin harian`, forecastedData5)

RMSE <-  data.frame(rmse1,rmse2,rmse3,rmse4,rmse5)
RMSE
write.csv(RMSE,"RMSE.csv")

R1 <- RSquare(outSample$`kecepatan angin harian`, forecastedData1)
R2 <- RSquare(outSample$`kecepatan angin harian`, forecastedData2)
R3 <- RSquare(outSample$`kecepatan angin harian`, forecastedData3)
R4 <- RSquare(outSample$`kecepatan angin harian`, forecastedData4)
R5 <- RSquare(outSample$`kecepatan angin harian`, forecastedData5)

RSQUARE <-  data.frame(R1,R2,R3,R4,R5)
RSQUARE
write.csv(RSQUARE,"RSquare.csv")

# Alpha
Alpha <- data.frame(model.ppr1$alpha,model.ppr2$alpha,model.ppr3$alpha,model.ppr4$alpha,model.ppr5$alpha)
Alpha
write.csv(Alpha,"Alpha.csv")

# Beta
Beta <- c(model.ppr1$beta,model.ppr2$beta,model.ppr3$beta,model.ppr4$beta,model.ppr5$beta)
Beta
write.csv(Beta,"Beta.csv")

# Plot perbandingan
data.forecast <- data.frame(outSample,forecastedData1,forecastedData2,forecastedData3,forecastedData4,forecastedData5)
ggplot(data.forecast, aes(x = Tanggal)) +
  geom_line(aes(y = kecepatan.angin.harian, color = "Kecepatan Angin Harian")) +
  geom_line(aes(y = forecastedData1, color = "Model 1")) +
  geom_line(aes(y = forecastedData2, color = "Model 2")) +
  geom_line(aes(y = forecastedData3, color = "Model 3")) +
  geom_line(aes(y = forecastedData4, color = "Model 4")) +
  geom_line(aes(y = forecastedData5, color = "Model 5")) +
  labs(title = "Perbandingan Hasil Forecast") +
  scale_y_continuous(limits = c(0, max(data.forecast$forecastedData5))) +
  scale_color_manual(values = c("Kecepatan Angin Harian" = "blue", "Model 1" = "red", "Model 2" = "green", "Model 3" = "purple", "Model 4" = "orange", "Model 5" = "pink")) +
  theme_minimal()
