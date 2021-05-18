install.packages("nortest")
install.packages("olsrr")
library(nortest)
library(olsrr)


# read data
library(readr)
data <- read_table2("C:/Users/balug/OneDrive/Masaüstü/Regression Example/data.txt", 
                    col_types = cols(x4 = col_factor(levels = c("1", 
                                                                "2", "3"))))


names(data)<-c("tuketim","ofis1","ofis2","ofis3","birim")
attach(data)


# descriptive-statistics
print(summary(data))


# Normality test
lillieTest(tuketim)
ad.test(tuketim)
qqnorm(tuketim)
qqline(tuketim)
# Doğrusallik Testi
pairs(data)
# Model kurma
result = lm(tuketim~ofis1+ofis2+ofis3+birim)
summary(result)

confint(result, level = .95) # “Güven aralığı”


# Aykırı değer analizi

inf<-ls.diag(result) # “Aykırı değerler ile ilgili kod”

inf

par(mfrow=c(2,2)) # “Değişen varyanslılık için grafik çizimi ile ilgili kod”

plot(predict(sonuc), inf$stud.res, ylab="Studentized Residuals", xlab="Predicted Value")


library(zoo) #index fonksiyonu için paket

cooksd <- cooks.distance(result)

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (length(data$y)>50) 4/length(data$y) else 4/(length(data$y)-(length(data)-1)-1) , col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (length(data$y)>50) 4/length(data$y) else 4/(length(data$y)-(length(data)-1)-1),names(cooksd),""), col="red")

which(cooksd > 0.04) # 63 72 88 satırlar etkin değerler çıkartılacak


hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*length(data)/length(tuketim) , col="red")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*length(data)/length(tuketim),index(hat),""), col="red")
which( hat > (2*6)/100) # 5 18 35 satırlar uç değerler çıkartılacak


stud<-inf$stud.res
plot(stud, pch="*", cex=2, main="Outlier by Studentized residuals",ylab="Studentized Residuals", xlab="Index")
abline(h = c(-3,3) ,col="red")
text(x=1:length(stud)+1, y=stud, labels=ifelse(stud<-3 & stud>3,index(stud),""), col="red")
which(stud > 3) # 63 72 88 Student artıkları çıkarılacak


std<-inf$std.res
plot(std, pch="*", cex=2, main="Outlier by Standardized residuals",ylab="Standardized Residuals", xlab="Index")
abline(h = c(-2,2) ,col="red")
text(x=1:length(std)+1, y=std, labels=ifelse(std<-2 & std>2,index(std),""), col="red")
which(std > 2) # 63 72 88 standarlaşmış artıkları çıkarılacak

# 63 72 88 5 18 35   Çıkartılacak gözlemler 
datanew <- data[-c(63,72, 88,5,18 ,35),]
str(datanew)

detach(data)
attach(datanew)

# Yeni model 

resnew = lm(tuketim~ofis1+ofis2+ofis3+birim)
summary(resnew)
# ofis 3 hariç didğer ozniteliklerın katsayıları anlamlı

####### YENI DATA İLE VARSAYIMLAR

ad.test(tuketim)
qqnorm(tuketim)
qqline(tuketim)
# Doğrusallik Testi
pairs(datanew)
"""
     
     library(lmtest)
     
     bptest(sonuc) #« Breusch ve Pagan testi » Değişen varyanslılık testi
     
     Özilişki sorunu
     
     dwtest(sonuc) #« Durbin-watson testi »
     
     Çoklu bağlantı sorunu
     
     “car package yükle”
     
     library(car)
     
     vif(sonuc)
     
     “perturb paketini yükleyin”
     
     library(perturb)
     
     colldiag(sonuc) # “ Koşul sayıları ve varyans bozulum oranları”
     
     Özdeğer ve özvektörlerin bulunması
     
     ort1<-mean(x1)
     
     kt1<-sum((x1-ort1)^2)
     
     skx1<-(x1-ort1)/(kt1^0.5)
     
     ort2<-mean(x2)
     
     kt2<-sum((x2-ort2)^2)
     
     skx2<-(x2-ort2)/(kt2^0.5)
     
     ort3<-mean(x3)
     
     kt3<-sum((x3-ort3)^2)
     
     skx3<-(x3-ort3)/(kt3^0.5)
     
     ort4<-mean(x4)
     
     kt4<-sum((x4-ort4)^2)
     
     skx4<-(x4-ort4)/(kt4^0.5)
     
     x<-cbind(skx1,skx2,skx3,skx4)
     
     sm<- eigen (t(x)%*%x)
     
     signif(sm$values,3)
     
     signif(sm$vectors,3)
     
     V<-sm$vectors
     
     t(V)%*%V
     
     V %*% diag(sm$values) %*% t(V
"""

