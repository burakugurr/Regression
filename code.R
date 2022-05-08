#install.packages("nortest")
#install.packages("olsrr")
#install.packages('perturb'==2.05)

library(nortest)
library(olsrr)
library(readr)
library(lmtest)
library(car)

library(zoo) #index fonksiyonu için paket


packageurl <- "https://rdocumentation.org/packages/perturb/versions/2.01"
install.packages(packageurl, repos=NULL, type="source")
library(perturb)
# read data

data <- read_table2("./data.txt", 
                    col_types = cols(x4 = col_factor(levels = c("1","2", "3"))))
head(data)
names(data)<-c("tuketim","ofis1","ofis2","ofis3","birim")
attach(data)


# descriptive-statistics
print(summary(data))


# Normality test

ols_test_normality(tuketim)
qqnorm(tuketim)
qqline(tuketim)

pairs(data)

# Model kurma

result = lm(tuketim~ofis1+ofis2+ofis3+birim)
summary(result)

confint(result, level = .95) # “Güven aralığı??


# Aykırı değer analizi

inf<-ls.diag(result)

inf

par(mfrow=c(1,1)) 

plot(predict(result), inf$stud.res, ylab="Studentized Residuals", xlab="Predicted Value")



cooksd <- cooks.distance(result)

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (length(data$y)>50) 4/length(data$y) else 4/(length(data$y)-(length(data)-1)-1) , col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (length(data$y)>50) 4/length(data$y) else 4/(length(data$y)-(length(data)-1)-1),names(cooksd),""), col="red")

which(cooksd > 0.04) # 63 72 88 satırlar etkin değerler çıkartılacak


hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*length(data)/length(tuketim) , col="red")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*length(data)/length(tuketim),index(hat),""), col="red")
which( hat > (2*6)/100) # 5 18 35 54 satırlar uç değerler çıkartılacak


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

# 63 72 88 5 18 35  54 Çıkartılacak gözlemler 
datanew <- data[-c(63,72, 88,5,18,54 ,35),]
str(datanew)

detach(data)
attach(datanew)

# Yeni model 

resnew = lm(tuketim~ofis1+ofis2+ofis3+birim)
summary(resnew)
# ofis 3 hariç didğer ozniteliklerın katsayıları anlamlı


# Yeni data seti ile varsayım kontroolu


data = datanew


cooksd <- cooks.distance(resnew)

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (length(data$y)>50) 4/length(data$y) else 4/(length(data$y)-(length(data)-1)-1) , col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (length(data$y)>50) 4/length(data$y) else 4/(length(data$y)-(length(data)-1)-1),names(cooksd),""), col="red")

which(cooksd > 0.04)


hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*length(data)/length(tuketim) , col="red")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*length(data)/length(tuketim),index(hat),""), col="red")
which( hat > (2*6)/100)

stud<-inf$stud.res
plot(stud, pch="*", cex=2, main="Outlier by Studentized residuals",ylab="Studentized Residuals", xlab="Index")
abline(h = c(-3,3) ,col="red")
text(x=1:length(stud)+1, y=stud, labels=ifelse(stud<-3 & stud>3,index(stud),""), col="red")
which(stud > 3) 


std<-inf$std.res
plot(std, pch="*", cex=2, main="Outlier by Standardized residuals",ylab="Standardized Residuals", xlab="Index")
abline(h = c(-2,2) ,col="red")
text(x=1:length(std)+1, y=std, labels=ifelse(std<-2 & std>2,index(std),""), col="red")
which(std > 2) 







####### YENI DATA İLE VARSAYIMLAR
confint(resnew, level = .95) # “Güven aralığı”

dwtest(resnew)

ad.test(tuketim)
qqnorm(tuketim)
qqline(tuketim)
# Doğrusallik Testi
pairs(datanew)


# değişken varyanslılık
library(lmtest)
bptest(resnew)

# Öz ilişki sorunu
dwtest(resnew)

# çoklu bağlantı

ols_coll_diag(resnew)

plot(predict(resnew), resnew$stud.res, ylab="Studentized Residuals", xlab="Predicted Value")



# Uyum Kestirimi

#Creating a data frame

x = data.frame(ofis1=8.512642, ofis2=0.63350773,ofis3=4.921705,birim="1")

#predicts the future values
predict(resnew,newdata = x,interval = 'confidence')



# Ön Kestirimi
x = data.frame(ofis1=10, ofis2=5,ofis3=2,birim=factor(1))

distPred <- predict(resnew, x,interval = 'confidence')  # predict distance
distPred


# En İYİ model


library(stats)

lm.null <- lm(tuketim ~ 1)

forward <- step(lm.null,tuketim~ofis1+ofis2+ofis3+birim, direction = "forward")

forward

summary(forward)
ols_step_all_possible(resnew)



## Geriye Doğru Seçim (Backward elimination)


backward<-step(resnew,direction="backward")

summary(backward)

ols_step_backward_p(resnew)
resnew

## Adımsal Seçim Yöntemi (Stepwise elimination)

library(MASS)

step.model <- stepAIC(resnew, direction = "both", trace = FALSE)

summary(step.model)


# Ridge Regresyon

ridge <- lm.ridge(tuketim~ofis1+ofis2+ofis3+birim ,lambda = seq(0,1,0.05))

matplot(ridge$lambda,t(ridge$coef),type="l",xlab=expression(lambda),
        
        ylab=expression(hat(beta)))

abline(h=0,lwd=2)

ridge$coef

select(ridge)

ridge$coef[,ridge$lam == 0.8]

# RESULT

wh <- data$tuketim 
x  <- data$ofis1
y  <- data$ofis2
z  <- data$ofis3
df <- data.frame(x, y, z)


scatter3d(x , y , z, groups = data$birim,grid = FALSE)


# SAVE DATA
write.table(datanew,"newdata.txt",sep="\t",row.names=TRUE)










