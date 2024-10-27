### IMPORT LIBRARIES
library(tseries)
library(forecast)
library(readxl)
library(car)
library(nortest)
library(lmtest)
library(stats)
library(Metrics)


### IMPORT DATASET
data <- read_excel("gisel/BINUS/Internship/Portfolio/Time Series Analysis/data.xlsx")
head(data)


### CEK POLA DATASET
plot(ts(data$percentage))

### DATA CROSS SECTION TRAIN TEST SPLIT (90% - 10%)
train_data <- data$percentage[1:21]
(train_data <- data.frame(train_data))
test_data <- data$percentage[22:23]
(test_data <- data.frame(test_data))


### TIME SERIES REGRESSION
# Kolom t (periode) untuk data cross section
(n <- nrow(train_data))
train_data$t <- seq(1:n)
test_data$t <- seq(22:23)

# Model Regresi Linear: Yt = a + bt
# Estimasi Parameter
lm1 <- lm(train_data~t, data=train_data)

# Uji Signifikansi Parameter
summary(lm1) # signifikan
e1 <- residuals(lm1)

# Uji Asumsi Independen
dwtest(lm1, alternative="two.sided") # tidak memenuhi

# Uji Asumsi Identik
bptest(lm1) # memenuhi

# Uji Asumsi Distribusi Normal
lillie.test(e1) # memenuhi

# Evaluasi model
(rmse1 <- sqrt(mean(e1^2)))
(mae1 <- mean(abs(e1)))
(mape1 <- mean(abs(e1/train_data$train_data))*100)

# Prediksi Data Testing & Evaluasi model
t_new = seq(n+1, n+2)
predict_modellm <- predict(lm1, newdata = data.frame(t=t_new))
predict_modellm

(error1 <- (test_data$test_data-predict_modellm))

(rmse1 <- sqrt(mean(error1^2)))
(mae1 <- mean(abs(error1)))
(mape1 <- mean(abs(error1/test_data$test_data))*100)

# Model Regresi Eksponensial: Yt = ae ^ bt
# Estimasi Parameter
train_data$lny <- log(train_data$train_data)
lm2 <- lm(lny~t, data=train_data)

# Uji Signifikansi Parameter
summary(lm2) # signifikan

(a <- exp(lm2$coefficients[1]))
yhat <- exp(fitted.values(lm2))
e2 <- train_data$train_data-yhat

# Uji Asumsi Independen
dwtest(lm2, alternative="two.sided") # tidak memenuhi

# Uji Asumsi Identik
bptest(lm2) # memenuhi

# Uji Asumsi Distribusi Normal
lillie.test(e2) # memenuhi

# Evaluasi model
(rmse2 <- sqrt(mean(e2^2)))
(mae2 <- mean(abs(e2)))
(mape2 <- mean(abs(e2/train_data$train_data))*100)

# Prediksi Data Testing & Evaluasi model
predict_ln <- predict(lm2, newdata = data.frame(t=t_new))
(predict_exp <- exp(predict_ln))

(error2 <- (test_data$test_data-predict_exp))

(rmse2 <- sqrt(mean(error2^2)))
(mae2 <- mean(abs(error2)))
(mape2 <- mean(abs(error2/test_data$test_data))*100)

# Model Regresi Kuadratik: Yt = a + bt + ct^2
# Estimasi Parameter
train_data$t2 <- (train_data$t)^2
head(train_data)
lm3 <- lm(train_data~t+t2, data=train_data)

# Uji Signifikansi Parameter
summary(lm3) # signifikan
e3 <- residuals(lm3)

# Uji Asumsi Independen
dwtest(lm3, alternative="two.sided") # tidak memenuhi

# Uji Asumsi Identik
bptest(lm3) # tidak memenuhi

# Uji Asumsi Distribusi Normal
lillie.test(e3) # memenuhi

# Evaluasi model
(rmse3 <- sqrt(mean(e3^2)))
(mae3 <- mean(abs(e3)))
(mape3 <- mean(abs(e3/train_data$train_data))*100)

# Prediksi Data Testing & Evaluasi model
(t_new = seq(n+1, n+2))
(t_new2 = c((n+1)^2, (n+2)^2))

predict_modelquad <- predict(lm3, newdata = data.frame(t=t_new, t2=t_new2))
predict_modelquad

(error3 <- (test_data$test_data-predict_modelquad))

(rmse3 <- sqrt(mean(error3^2)))
(mae3 <- mean(abs(error3)))
(mape3 <- mean(abs(error3/test_data$test_data))*100)

# Model Regresi Lag Pertama: Yt = a + bt + cYt-1
# Estimasi Parameter
yt <- train_data$train_data[2:n]
length(yt)
t <- train_data$t[2:n]
length(t)
yt1 <- train_data$train_data[1:(n-1)]
lm4 <- lm(yt~t+yt1)

# Uji Signifikansi Parameter
summary(lm4) # t tidak signifikan

# Modifikasi model, hapus yang tidak signifikan
lm4 <- lm(yt~yt1)
summary(lm4)
e4 <- residuals(lm4)

# Uji Asumsi Independen
dwtest(lm4, alternative="two.sided") # tidak memenuhi

# Uji Asumsi Identik
bptest(lm4) # memenuhi

# Uji Asumsi Distribusi Normal
lillie.test(e4) # memenuhi

# Evaluasi model
(rmse4 <- sqrt(mean(e4^2)))
(mae4 <- mean(abs(e4)))
(mape4 <- mean(abs(e4/train_data$train_data[2:21]))*100)

# Prediksi Data Testing & Evaluasi model
p4 <- predict(lm4, newdata = data.frame(yt1 = train_data$train_data[21]))
predict_modellag1 <- c(p4, p4)
predict_modellag1

(error4 <- (test_data$test_data-predict_modellag1))

(rmse4 <- sqrt(mean(error4^2)))
(mae4 <- mean(abs(error4)))
(mape4 <- mean(abs(error4/test_data$test_data))*100)

# Model Regresi Lag Kedua: Yt = a + bt + cYt-1 + dYt-2
# Estimasi Parameter
yt <- train_data$train_data[3:n]
length(yt)
t <- train_data$t[3:n]
length(t)
yt1 <- train_data$train_data[2:(n-1)]
yt2 <- train_data$train_data[1:(n-2)]
lm5 <- lm(yt~t+yt1+yt2)

# Uji Signifikansi Parameter
summary(lm5) # signifikan
e5 <- residuals(lm5)

# Uji Asumsi Independen
dwtest(lm5, alternative="two.sided") # memenuhi

# Uji Asumsi Identik
bptest(lm5) # memenuhi

# Uji Asumsi Distribusi Normal
lillie.test(e5) # memenuhi

# Evaluasi model
(rmse5 <- sqrt(mean(e5^2)))
(mae5 <- mean(abs(e5)))
(mape5 <- mean(abs(e5/train_data$train_data[3:21]))*100)

# Prediksi Data Testing & Evaluasi model
p5 <- predict(lm5, newdata = data.frame(t = 22, yt1 = train_data$train_data[21], yt2 = train_data$train_data[20]))
predict_modellag2 <- c(p5, p5)
predict_modellag2

(error5 <- (test_data$test_data-predict_modellag2))

(rmse5 <- sqrt(mean(error5^2)))
(mae5 <- mean(abs(error5)))
(mape5 <- mean(abs(error5/test_data$test_data))*100)

# Model Regresi Lag Ketiga: Yt = a + bt + cYt-1 + dYt-2 + eYt-3
# Estimasi Parameter
yt <- train_data$train_data[4:n]
length(yt)
t <- train_data$t[4:n]
length(t)
yt1 <- train_data$train_data[3:(n-1)]
yt2 <- train_data$train_data[2:(n-2)]
yt3 <- train_data$train_data[1:(n-3)]
lm6 <- lm(yt~t+yt1+yt2+yt3)

# Uji Signifikansi Parameter
summary(lm6) # tidak signifikan


### TIME SERIES DATASET
(tsdata <- ts(data=data$percentage, start=2000, end=2022, frequency=1))


### TRAIN TEST SPLIT (90% - 10%)
(train_data <- ts(data=data$percentage[1:21], start=2000, end=2020, frequency=1))
(test_data <- ts(data=data$percentage[22:23], start=2021, end=2022, frequency=1))


### DOUBLE EXPONENTIAL SMOOTHING
# Identifikasi parameter alpha beta & Evaluasi model
DES1 <- ets(train_data, model="AAN", alpha=0.5, beta=0.5)
summary(DES1)

DES2 <- ets(train_data, model="AAN", alpha=0.7, beta=0.5)
summary(DES2)

DES3 <- ets(train_data, model="AAN", alpha=0.9, beta=0.5)
summary(DES3)

DES4 <- ets(train_data, model="AAN", alpha=0.9, beta=0.7)
summary(DES4)

DES5 <- ets(train_data, model="AAN", alpha=0.9, beta=0.9)
summary(DES5)

# Prediksi Data Testing & Evaluasi model
(yhat1 <- forecast(DES1, h=2)$mean)
(e1 <- yhat1-test_data)
(rmse1 <- sqrt(mean(e1^2)))
(mae1 <- mean(abs(e1)))
(mape1 <- mean(abs(e1/test_data))*100)

(yhat2 <- forecast(DES2, h=2)$mean)
(e2 <- yhat2-test_data)
(rmse2 <- sqrt(mean(e2^2)))
(mae2 <- mean(abs(e2)))
(mape2 <- mean(abs(e2/test_data))*100)

(yhat3 <- forecast(DES3, h=2)$mean)
(e3 <- yhat3-test_data)
(rmse3 <- sqrt(mean(e3^2)))
(mae3 <- mean(abs(e3)))
(mape3 <- mean(abs(e3/test_data))*100)

(yhat4 <- forecast(DES4, h=2)$mean)
(e4 <- yhat4-test_data)
(rmse4 <- sqrt(mean(e4^2)))
(mae4 <- mean(abs(e4)))
(mape4 <- mean(abs(e4/test_data))*100)

(yhat5 <- forecast(DES5, h=2)$mean)
(e5 <- yhat5-test_data)
(rmse5 <- sqrt(mean(e5^2)))
(mae5 <- mean(abs(e5)))
(mape5 <- mean(abs(e5/test_data))*100)


### ARIMA
# Uji Stasioneritas terhadap variance
summary(powerTransform(train_data)) # stasioner

# Uji Stasioneritas terhadap mean
adf.test(train_data) # tidak stasioner
diff_data = diff(train_data, differences=9) # differencing 9x
adf.test(diff_data) # stasioner

# Identifikasi ordo p & q (Plot ACF & PACF)
acf(diff_data, lag.max=20) # ordo q signifikan sampai ordo 1
pacf(diff_data*9, lag.max=20) # ordo p signifikan sampai ordo 2

# Estimasi Parameter & Uji Signifikansi Parameter
ARIMA1 = arima(train_data,order=c(0,0,1))
coeftest(ARIMA1) # signifikan

ARIMA2 = arima(train_data,order=c(1,0,0))
coeftest(ARIMA2) # signifikan

ARIMA3 = arima(train_data,order=c(0,1,1))
coeftest(ARIMA3) # signifikan

ARIMA4 = arima(train_data,order=c(1,1,0))
coeftest(ARIMA4) # signifikan

ARIMA5 = arima(train_data,order=c(2,1,0))
coeftest(ARIMA5) # signifikan

ARIMA6 = arima(train_data,order=c(1,1,1))
coeftest(ARIMA6) # signifikan

ARIMA7 = arima(train_data,order=c(0,2,1))
coeftest(ARIMA7) # signifikan

ARIMA8 = arima(train_data,order=c(1,2,0))
coeftest(ARIMA8) # signifikan

ARIMA9 = arima(train_data,order=c(2,2,0))
coeftest(ARIMA9)

ARIMA10 = arima(train_data,order=c(1,2,1))
coeftest(ARIMA10) # signifikan

ARIMA11 = arima(train_data,order=c(2,2,1))
coeftest(ARIMA11)

ARIMA12 = arima(train_data,order=c(0,3,1))
coeftest(ARIMA12) # signifikan

ARIMA13 = arima(train_data,order=c(1,3,0))
coeftest(ARIMA13)

ARIMA14 = arima(train_data,order=c(2,3,0))
coeftest(ARIMA14)

ARIMA15 = arima(train_data,order=c(1,3,1))
coeftest(ARIMA15)

ARIMA16 = arima(train_data,order=c(2,3,1))
coeftest(ARIMA16)

ARIMA17 = arima(train_data,order=c(0,4,1))
coeftest(ARIMA17)

ARIMA18 = arima(train_data,order=c(1,4,0))
coeftest(ARIMA18)

ARIMA19 = arima(train_data,order=c(2,4,0))
coeftest(ARIMA19)

ARIMA20 = arima(train_data,order=c(1,4,1))
coeftest(ARIMA20)

ARIMA21 = arima(train_data,order=c(2,4,1))
coeftest(ARIMA21)

ARIMA22 = arima(train_data,order=c(0,5,1))
coeftest(ARIMA22) # signifikan

ARIMA23 = arima(train_data,order=c(1,5,0))
coeftest(ARIMA23)

ARIMA24 = arima(train_data,order=c(2,5,0))
coeftest(ARIMA24)

ARIMA25 = arima(train_data,order=c(1,5,1))
coeftest(ARIMA25)

ARIMA26 = arima(train_data,order=c(2,5,1))
coeftest(ARIMA26)

ARIMA27 = arima(train_data,order=c(0,6,1))
coeftest(ARIMA27) # signifikan

ARIMA28 = arima(train_data,order=c(1,6,0))
coeftest(ARIMA28) # signifikan

ARIMA29 = arima(train_data,order=c(2,6,0))
coeftest(ARIMA29) # signifikan

ARIMA30 = arima(train_data,order=c(1,6,1))
coeftest(ARIMA30)

ARIMA31 = arima(train_data,order=c(2,6,1))
coeftest(ARIMA31)

ARIMA32 = arima(train_data,order=c(0,7,1))
coeftest(ARIMA32) # signifikan

ARIMA33 = arima(train_data,order=c(1,7,0))
coeftest(ARIMA33) # signifikan

ARIMA34 = arima(train_data,order=c(1,7,1))
coeftest(ARIMA34) # signifikan

ARIMA35 = arima(train_data,order=c(2,7,1))
coeftest(ARIMA35)

ARIMA36 = arima(train_data,order=c(0,8,1))
coeftest(ARIMA36) # signifikan

ARIMA37 = arima(train_data,order=c(1,8,0))
coeftest(ARIMA37) # signifikan

ARIMA38 = arima(train_data,order=c(2,8,0))
coeftest(ARIMA38) # signifikan

ARIMA39 = arima(train_data,order=c(1,8,1))
coeftest(ARIMA39) # signifikan

ARIMA40 = arima(train_data,order=c(0,9,1))
coeftest(ARIMA40) # signifikan

ARIMA41 = arima(train_data,order=c(1,9,0))
coeftest(ARIMA41) # signifikan

ARIMA42 = arima(train_data,order=c(2,9,0))
coeftest(ARIMA42) # signifikan

ARIMA43 = arima(train_data,order=c(1,9,1))
coeftest(ARIMA43) # signifikan

ARIMA44 = arima(train_data,order=c(2,9,1))
coeftest(ARIMA44) # signifikan

# Uji Asumsi White Noise untuk model signifikan
e1 = residuals(ARIMA1)
Box.test(e1, type="Ljung-Box") # tidak memenuhi

e2 = residuals(ARIMA2)
Box.test(e2, type="Ljung-Box") # memenuhi

e3 = residuals(ARIMA3)
Box.test(e3, type="Ljung-Box") # tidak memenuhi

e4 = residuals(ARIMA4)
Box.test(e4, type="Ljung-Box") # memenuhi

e5 = residuals(ARIMA5)
Box.test(e5, type="Ljung-Box") # memenuhi

e6 = residuals(ARIMA6)
Box.test(e6, type="Ljung-Box") # memenuhi

e7 = residuals(ARIMA7)
Box.test(e7, type="Ljung-Box") # memenuhi

e8 = residuals(ARIMA8)
Box.test(e8, type="Ljung-Box") # memenuhi

e10 = residuals(ARIMA10)
Box.test(e10, type="Ljung-Box") # memenuhi

e12 = residuals(ARIMA12)
Box.test(e12, type="Ljung-Box") # tidak memenuhi

e22 = residuals(ARIMA22)
Box.test(e22, type="Ljung-Box") # tidak memenuhi

e27 = residuals(ARIMA27)
Box.test(e27, type="Ljung-Box") # tidak memenuhi

e28 = residuals(ARIMA28)
Box.test(e28, type="Ljung-Box") # tidak memenuhi

e29 = residuals(ARIMA29)
Box.test(e29, type="Ljung-Box") # tidak memenuhi

e32 = residuals(ARIMA32)
Box.test(e32, type="Ljung-Box") # tidak memenuhi

e33 = residuals(ARIMA33)
Box.test(e33, type="Ljung-Box") # tidak memenuhi

e34 = residuals(ARIMA34)
Box.test(e34, type="Ljung-Box") # tidak memenuhi

e36 = residuals(ARIMA36)
Box.test(e36, type="Ljung-Box") # tidak memenuhi

e37 = residuals(ARIMA37)
Box.test(e37, type="Ljung-Box") # tidak memenuhi

e38 = residuals(ARIMA38)
Box.test(e38, type="Ljung-Box") # tidak memenuhi

e39 = residuals(ARIMA39)
Box.test(e39, type="Ljung-Box") # tidak memenuhi

e40 = residuals(ARIMA40)
Box.test(e40, type="Ljung-Box") # tidak memenuhi

e41 = residuals(ARIMA41)
Box.test(e41, type="Ljung-Box") # tidak memenuhi

e42 = residuals(ARIMA42)
Box.test(e42, type="Ljung-Box") # tidak memenuhi

e43 = residuals(ARIMA43)
Box.test(e43, type="Ljung-Box") # tidak memenuhi

e44 = residuals(ARIMA44)
Box.test(e44, type="Ljung-Box") # tidak memenuhi

# Uji Asumsi Distribusi Normal untuk model yang memenuhi
lillie.test(e2) # tidak memenuhi
lillie.test(e4) # tidak memenuhi
lillie.test(e5) # tidak memenuhi
lillie.test(e6) # tidak memenuhi
lillie.test(e7) # tidak memenuhi
lillie.test(e8) # tidak memenuhi
lillie.test(e10) # tidak memenuhi


### NEURAL NETWORK
# Inisialisasi list untuk menyimpan hasil evaluasi model
model_list <- list()
results <- data.frame(p = integer(), size = integer(), train_rmse = numeric(), train_mae = numeric(), train_mape = numeric(), test_rmse = numeric(), test_mae = numeric(), test_mape = numeric(), stringsAsFactors = FALSE)

# Identifikasi parameter p dan size (looping)
for (p in 1:5) {
  for (size in 1:100) {
    # Melatih model
    NN <- nnetar(train_data, p = p, size = size)
    
    # Menghitung error pada training data
    train_accuracy <- forecast::accuracy(NN)
    train_rmse <- train_accuracy["Training set", "RMSE"]
    train_mae <- train_accuracy["Training set", "MAE"]
    train_mape <- train_accuracy["Training set", "MAPE"]
    
    # Menghitung error pada testing data
    yhat1 <- forecast(NN, h = length(test_data))$mean
    e1 <- test_data - yhat1
    test_rmse <- sqrt(mean(e1^2))
    test_mae <- mean(abs(e1))
    test_mape <- mean(abs(e1 / test_data)) * 100
    
    # Menyimpan hasil evaluasi model
    results <- rbind(results, data.frame(p = p, size = size, train_rmse = train_rmse, train_mae = train_mae, train_mape = train_mape, test_rmse = test_rmse, test_mae = test_mae, test_mape = test_mape))
  }
}

# Memilih 5 model terbaik berdasarkan evaluasi model
(top5_rmse <- head(results[order(results$test_rmse), ], 5))
(top5_mae <- head(results[order(results$test_mae), ], 5))
(top5_mape <- head(results[order(results$test_mape), ], 5))

(top5_models <- top5_rmse)

# Menyimpan model terbaik ke dalam variabel
for (i in 1:5) {
  assign(paste0("NN", 6-i), nnetar(train_data, p = top5_models$p[i], size = top5_models$size[i]))
  print(paste("NN", 6-i, ": p =", top5_models$p[i], ", size =", top5_models$size[i]))
  print(top5_models[i, ])
}

NN5 


# FORECAST MASA DEPAN (BEST MODEL)
(forecast_percentage <- forecast(NN5, h=14)$mean)
