setwd("/Users/apple/Desktop/回归/回归大作业1")
data0 <- read.csv("mydata.csv", header = T, sep= "," ,encoding = 'UTF-8')
dim(data0)
names(data0)
data0=data0[,-1]
str(data0)

data0$subway <- as.factor(data0$subway)
data0$school <- as.factor(data0$school)
attach(data0)
####show Chinese on MAC
library(sysfonts)
library(showtext)
dev.new() 
showtext.begin()

##############Descriptive analysis
#1.Variable description
summary(data0)
data0[data0[,8]==min(data0[,8]),]
data0[data0[,8]==max(data0[,8]),]
#2.Correlation coefficient graph
dat <- data0
dat$CATE<- as.numeric(CATE)
dat$bedrooms<- as.numeric(bedrooms)
dat$halls<- as.numeric(halls)
dat$floor<- as.numeric(floor)
dat$subway<- as.numeric(subway)
dat$school<- as.numeric(school)
dat$price<- as.numeric(price)
library(corrplot)
cormatrix <- cor(dat[,c(1,2,3,4,5,6,7,8,9,10)],use = "everything")
corrplot(cormatrix)
corrplot(cormatrix,method = "shade",addCoef.col = "black",tl.col = "black",tl.srt = 45)
#3.Histogram
library(ggplot2)
ggplot(data0,aes(x= halls))+ geom_bar(fill="pink")
ggplot(data0,aes(x= bedrooms))+ geom_bar(fill="pink")
ggplot(data0,aes(x= CATE,fill=subway))+ geom_bar(position="dodge")
ggplot(data0,aes(x= CATE,fill=subway))+ geom_bar(position="fill")
ggplot(data0,aes(x= CATE,fill=school))+ geom_bar(position="dodge")
ggplot(data0,aes(x= CATE,fill=school))+ geom_bar(position="fill")

hist(data0$price,xlab="单位面积房价/(元/平方米)",ylab="频数/(个)",main="",col = "pink")

data0$CATE=factor(data0$CATE,levels=c("shijingshan","fengtai","chaoyang","haidian","dongcheng","xicheng"))
boxplot(price~CATE,data=data0,col="pink",ylab="单位面积房价(元/平方米)")
par(mfrow=c(1,2))
boxplot(price~subway,data=data0,col="pink",names=c("地铁房","非地铁房"),ylab="单位面积房价/(元/平方米)")
boxplot(price~school,data=data0,col="pink",names=c("学区房","非学区房"),ylab="单位面积房价/(元/平方米)")
par(mfrow=c(1,3))
boxplot(price~bedrooms,data=data0,col="pink",xlab="卧室数",ylab="单位面积房价/(元/平方米)")
boxplot(price~halls,data=data0,col="pink",xlab="厅数",ylab="单位面积房价/(元/平方米)")
boxplot(price~floor,data=data0,col="pink",names=c("高","中","低"),xlab="楼层",ylab="单位面积房价/(元/平方米)")

##############Model
###1.Simple linear regression model
lm1 <- lm(price~CATE+halls+AREA+floor+subway+school,data=data0)
summary(lm1)
#Simplify model hypothesis testing
lm1_RM1 <- lm(price~CATE+halls+floor+subway+school,data=data0)
lm1_RM2 <- lm(price~CATE+halls+AREA+subway+school,data=data0)
anova(lm1_RM1,lm1)
anova(lm1_RM2,lm1)
#Model diagnosis
#Check linearity and normality
dev.new() 
showtext.begin()
par(mfrow=c(1,1))
#1. Standardized residual plot
standard_residuals<- rstandard(lm1)
plot(standard_residuals)
#2. QQ plot of standardized residuals
qqnorm(standard_residuals)
qqline(standard_residuals)
#3. Scatter plot of standardized residuals with respect to fitted values
ggplot(data0,aes(x=lm1$fitted.values,y=standard_residuals))+geom_point()#存在异方差现象,对因变量取对数
#4.cook distance: outliers
plot(lm1,which=4)

###2. Regression model after logarithmic transformation
lm2=lm(log(price)~CATE+halls+AREA+floor+subway+school,data=data0)
summary(lm2)
#Model diagnosis
#Check linearity and normality
par(mfrow=c(1,2))
#1. Standardized residual plot
standard_residuals2 <- rstandard(lm2)
plot(standard_residuals2)
#2. QQ plot of standardized residuals
qqnorm(standard_residuals2)
qqline(standard_residuals2)
#3. Scatter plot of standardized residuals with respect to fitted values
ggplot(data0,aes(x=lm2$fitted.values,y=standard_residuals2))+geom_point()#没有异方差现象
#4.cook distance: outliers
plot(lm2,which=4)

###3. Interactive linear regression model
lm3=lm(log(price)~CATE*school+halls+AREA+floor+subway,data=data0)
summary(lm3)
#Fengtai and Shijingshan are grouped together
dat2 <-data0
levels(dat2$CATE)=list(shifeng=c("shijingshan","fengtai"),chaoyang="chaoyang",dongcheng="dongcheng",
                       xicheng="xicheng",haidian="haidian")
levels(dat2$CATE)
lm3_shi=lm(log(price)~CATE*school+halls+AREA+floor+subway,data=dat2)
summary(lm3_shi)
#Box diagrams of school districts in different urban areas
par(mfrow=c(2,3))
boxplot(price~school,data=data0[data0$CATE=="shijingshan",],main="石景山区",col="pink")
boxplot(price~school,data=data0[data0$CATE=="fengtai",],main="丰台",col="pink")
boxplot(price~school,data=data0[data0$CATE=="chaoyang",],main="朝阳",col="pink")
boxplot(price~school,data=data0[data0$CATE=="dongcheng",],main="东城",col="pink")
boxplot(price~school,data=data0[data0$CATE=="xicheng",],main="西城",col="pink")
boxplot(price~school,data=data0[data0$CATE=="haidian",],main="海淀",col="pink")
#Fengtai and Chaoyang are grouped together
dat2 <-data0
levels(dat2$CATE)=list(fengchao=c("fengtai","chaoyang"),shijingshan="shijingshan",
                       dongcheng="dongcheng",xicheng="xicheng",haidian="haidian")
levels(dat2$CATE)
lm3_chao=lm(log(price)~CATE*school+halls+AREA+floor+subway,data=dat2)
summary(lm3_chao)
#Fengtai and Dongcheng are grouped together
dat2 <-data0
levels(dat2$CATE)=list(fengdong=c("dongcheng","fengtai"),chaoyang="chaoyang",
                       shijingshan="shijingshan",xicheng="xicheng",haidian="haidian")
levels(dat2$CATE)
lm3_dong=lm(log(price)~CATE*school+halls+AREA+floor+subway,data=dat2)
summary(lm3_dong)
#Fengtai and Xicheng are grouped together
dat2 <-data0
levels(dat2$CATE)=list(fengxi=c("xicheng","fengtai"),chaoyang="chaoyang",dongcheng="dongcheng",
                       shijingshan="shijingshan",haidian="haidian")
levels(dat2$CATE)
lm3_xi=lm(log(price)~CATE*school+halls+AREA+floor+subway,data=dat2)
summary(lm3_xi)
#Fengtai and Haidian are grouped together
dat2 <-data0
levels(dat2$CATE)=list(fenghai=c("haidian","fengtai"),chaoyang="chaoyang",dongcheng="dongcheng",
                       shijingshan="shijingshan",xicheng="xicheng")
levels(dat2$CATE)
lm3_hai=lm(log(price)~CATE*school+halls+AREA+floor+subway,data=dat2)
summary(lm3_hai)

#Model diagnosis
#Check linearity and normality
par(mfrow=c(1,2))
#1. Standardized residual plot
standard_residuals3 <- rstandard(lm3_dong)
plot(standard_residuals3)
#2. QQ plot of standardized residuals
qqnorm(standard_residuals3)
qqline(standard_residuals3)
#3. Scatter plot of standardized residuals with respect to fitted values
ggplot(dat2,aes(x=lm3_dong$fitted.values,y=standard_residuals3))+geom_point()#没有异方差现象
#4.cook distance: outliers
plot(lm3_dong,which=4)
#
library(lmtest)
dw_test <- dwtest(lm1)
dw_test
dw_test$statistic
rho_hat <- 1- dw_test$statistic/2
rho_hat

#Multicollinearity test
library(car)
vif(lm3_dong)

###predict
new <- data.frame(CATE="xicheng",halls=1,AREA=79,
                  floor="middle",subway=factor(0),school=factor(1))
lm3.pre <- predict(lm3_dong,new,interval = "prediction", level = 0.95)
exp(lm3.pre)
