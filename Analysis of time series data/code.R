
setwd("/Users/apple/Desktop/时间序列/期末大作业")

rm(list = ls()) 
library(tseries)
library(ggplot2)
library(forecast)
library(zoo)
library(lmtest)
library(tseries)
library(FinTS)
library(mice)
library(fGarch)
library(rugarch)
# Data ---------------------------------------------------------------------

data0 <- read.csv("pm2.csv", header = TRUE, sep = ",")
head(data0)
str(data0)
data0[,1] <- as.factor(data0[,1])
data0[,2] <- as.factor(data0[,2])
data0[,3] <- as.factor(data0[,3])
n <- dim(data0)[1]
dat <- data0[,5]
date1 <- 1:n

#Descriptive statistical analysis ----------------------------------------------------------------
library(sysfonts)
library(showtext)
dev.new() 
showtext.begin()

## Scatter plot
ggplot(data.frame(date1,dat),aes(date1,dat)) + geom_point(size=1,shape=21, colour= "black")

## Time series plot
ggplot(data.frame(date1,dat),aes(date1,dat)) + geom_line()

## Histogram
ggplot(data.frame(date1,dat), aes(x=dat)) + geom_histogram(aes(y=..density..),  binwidth=50,colour="black", fill="white") +geom_density(alpha=.2, fill="#FF6666")  

## Box plot
ggplot(data.frame(data0$year,dat), aes(x=data0$year,y=dat))+geom_boxplot(fill=rainbow(5))+labs(title = '箱线图-年度')
data0[data0$pm2.5>800,]
ggplot(data.frame(data0$month,dat), aes(x=data0$month,y=dat))+geom_boxplot(fill=rainbow(12))+labs(title = '箱线图-月度')

## Descriptive Statistics
mean(dat); max(dat); min(dat); median(dat);sd(dat);var(dat);quantile(dat)

#Analysis and modeling ---------------------------------------------------------------

## Smoothing ---------------------------------------------------------------

#log diff
x <- log(dat)
x <- diff(x)

#Sequence diagram after sequence conversion
date2 <- 1:(n-1)
ggplot(data.frame(date2,x),aes(date2,x)) + geom_line(colour='black')

### ADF test after logarithmic transformation
adf.test(x)

## White noise test ---------------------------------------------------------------
max_m <- 10
p_value_vec <- rep(NA, max_m)
for(i in 1:max_m)
{
  p_value_vec[i] <- Box.test(x, lag = i, type = 'Ljung-Box')$p.value
}
print(p_value_vec) 
plot(p_value_vec)  
plot(p_value_vec, ylim = c(0,1)) 
abline(h = 0.05, col = 'red')  

## Model identification and ordering -----------------------------------------------------------------

### The first type of method: information criterion
auto.arima(x) 
auto.arima(x, ic = 'aic')  # aic
auto.arima(x, ic = 'bic')  # bic
auto.arima(x, ic = 'aic', stepwise = F)  # Change to traverse search
auto.arima(x, ic = 'bic', stepwise = F)
auto.arima(x, ic = 'aic', max.order = 15, stepwise = F)#increase max.order
auto.arima(x, ic = 'bic', max.order = 15, stepwise = F)
auto.arima(x, ic = 'aicc', max.order = 15, stepwise = F)

### The second type of method: correlation function
TSA::acf(x) 
pacf(x) 
TSA::eacf(x)

#12 52 19

#The final order is as follows:
p <- 5
d <- 1
q <- 2

## Parameter Estimation -----------------------------------------------------------------

x.fit<-arima(x,order = c(p, d, q))
x.fit

### Parameter significance analysis
arima.coef <- function(object){
  
  coef_est <- coef(object)
  coef_std <- sqrt( diag( vcov(object) ) )
  coef_est <- coef_est[names(coef_std)]
  coef_t_value <- coef_est / coef_std
  coef_p_value <- pnorm( - abs(coef_t_value) ) * 2
  coef.stat <- data.frame(coef_est, coef_std,
                          coef_t_value, coef_p_value)
                          # Merged into regression coefficient analysis table
  return(coef.stat)
}
arima.coef(x.fit)

### Eliminate insignificant parameters
sig_level <- 0.05
my_fixed_coef <- arima.coef(x.fit)$coef_p_value
my_fixed_coef[my_fixed_coef <= sig_level] <- NA
my_fixed_coef[my_fixed_coef >  sig_level] <- 0

x.fit <- arima(x, order = c(p, d, q), fixed = my_fixed_coef, transform.pars = F)
print(x.fit)
arima.coef(x.fit)

## Model diagnosis -----------------------------------------------------------------

### White noise test on residual sequence
max_m <- 10
p_value_vec <- rep(NA, max_m)
for(i in 1:max_m)
{
  p_value_vec[i] <- Box.test(x.fit$residual, lag = i, type = 'Ljung-Box')$p.value
}
plot(p_value_vec)  
plot(p_value_vec, ylim = c(0,1)) 
abline(h = 0.05, col = 'red')  

### Autocorrelation of residuals
tsdiag(x.fit, type = 'Ljung-Box', fitdf = 2)

### Normality of residuals
qqnorm(x.fit$residual)
qqline(x.fit$residual, col = 'red', lwd = 2)

# After passing the test, determine the final model
model_final <- x.fit

# Model statistics (goodness of fit) -------------------------------------------------------------

# Sample size
Num_obs <- nobs(model_final)

# Number of parameters
Num_para <- ncol(vcov(model_final))
print(Num_para)

# R^2
R2 <- 1 - sum((model_final$residuals)^2) / sum((x- mean(x) )^2)
print(R2)

# adjusted R^2
R2_adj <- 1 - model_final$sigma2 / var(x)
print(R2_adj)

# Signal to noise ratio
SNR <- var(x) / model_final$sigma2 - 1
print(SNR)

# log
logL <- model_final$loglik
print(logL)

# Model deviation
Dev <-  - 2 * logL
print(Dev)

# AIC
Dev + 2 * length(coef(model_final))#未剔除零参数
model_final$aic
AIC(model_final)
Dev + 2 * Num_para#剔除了零参数

# BIC
Dev + length(coef(model_final)) * log(Num_obs)
BIC(model_final)
Dev + Num_para * log(Num_obs)

# AICc
Dev + 2 * (Num_para + 1) *  (Num_para + 2) / (Num_obs - Num_para -2)

# Prediction --------------------------------------------------------------------

x.fore <- forecast(x.fit,h = 50)
plot(x.fore)

# Homogeneity test of variance -----------------------------------------------------------

## Residual plot
res <- model_final$residuals
plot(res)
ggplot(data.frame(date2,res),aes(x=date2,y=res)) + geom_line(colour='black')

## Residual square plot
plot(res^2)
ggplot(data.frame(date2,res),aes(x=date2,y=res^2)) + geom_line(colour='black')

## Portmanteau Q test
for (i in 1:5) print(Box.test(x.fit$residual^2,type = "Ljung-Box",lag=i))

# GARCH -----------------------------------------------------------
garch11.spec = ugarchspec(mean.model = list(armaOrder = c(0,3)), 
                          variance.model = list(garchOrder = c(1,1), 
                                                model = "sGARCH"), distribution.model = "norm")

## Fit the model
garch.fit = ugarchfit(garch11.spec, data = x, fit.control=list(scale=TRUE))
plot(garch.fit)

##Prediction

fore=ugarchforecast(garch.fit,n.ahead=10)
fore
plot(fore)

## GARCH（1,1）prediction

gar11=garchFit(~arma(2,2)+aparch(1,1),data = x,cond.dist="sged")
summary(gar11)
plot(gar11)
report(gar11)
predict(gar11,50)
plot(predict(gar11,50))
