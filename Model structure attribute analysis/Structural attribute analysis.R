setwd("/Users/apple/Desktop/毕设")

######################MODEL_1 MODEL_sap
data0=read.csv("sap.csv",sep = ",", head=T)
data1=data0[which(data0$orginal.model=="true"),]   ##original data1
data2=data0[which(data0$orginal.model=="false"),]  ##synthetic data2
str(data0)
par(mar=c(2.5,2,2,2)+2, pty = "s")
options(digits=4)
###########Principal component analysis
data11=data1[,-(1:3)]
q = scale(data11) #Eliminate difference in magnitude and dimension between variables
cor(q) #many correlation coefficients, so principal component analysis is effective
library(caret)
library(corrplot)
corrplot(cor(q),order="hclust",tl.cex=0.7)
PCA <- princomp(q,cor=T) #Principal component analysis

###Interpretation of results
par(mar=c(4,4,2,1),cex=0.75)
#Crushed stone diagram, decide the number of selected principal components
screeplot(PCA,type="lines")  
summary(PCA, loadings = T)
PCA$loadings

# biplot
biplot(PCA) #scatter plot of principal component 1 and 2
biplot(PCA, choices = 2:3) #scatter plot of principal component 2 and 3
PCA$scores[,1:3]
PCs <- as.matrix(PCA$scores[,1:3]) 
plot(PCs)
abline(h=0,v=0,lty=3) #0,0 means the average value of y1, y2, the farther the position is from 0, the farther away from the average level
text(PCs,label=data1[,1],pos=1.8,adj=0.5,cex=0.85) 

# Sort by principal component score
rank_PC <- cbind(PCs,'rank1'=rank(PCA$scores[,1]), 'rank2'=rank(PCA$scores[,2]),'rank3'=rank(PCA$scores[,3])) 
colnames(rank_PC)
rownames(rank_PC) <- data1[,1]
sort(rank_PC[,"rank1"])
sort(rank_PC[,"rank2"])
sort(rank_PC[,"rank3"])

#####Analysis of plots
library(ggplot2)
ggplot(data1,aes(x=nodes))+geom_histogram(fill="pink",colour="orange")
ggplot(data2,aes(x=nodes))+geom_histogram(fill="pink",colour="orange")

summary(data1)
summary(data2)

sd(data2$task)
sd(data0$label.occurrences)
sd(data0$nodes)
sd(data0$edges)
##### t test
m = data1[,16]
n = data2[,16]
t.test(m,n,paired = FALSE)

######################MODEL_2 MODEL_bpmn
data1=read.csv("bpmn1.csv",sep = ",", head=T)
data0=read.csv("bpmn0.csv",sep = ",", head=T)

str(data1)
par(mar=c(2.5,2,2,2)+2, pty = "s")
options(digits=4)
###########Principal component analysis
data11=data1[,-1]
q = scale(data11[,-(11:14)])
cor(q) #many correlation coefficients, so principal component analysis is effective
library(caret)
library(corrplot)
corrplot(cor(q),order="hclust",tl.cex=0.7)
PCA <- princomp(q,cor=T)

###Interpretation of results
par(mar=c(4,4,2,1),cex=0.75)
#Crushed stone diagram, decide the number of selected principal components
screeplot(PCA,type="lines")  

summary(PCA, loadings = T)
PCA$loadings

#####Analysis of plots
ggplot(data0,aes(x=nodes))+geom_histogram(fill="pink",colour="orange")
ggplot(data1,aes(x=nodes))+geom_histogram(fill="pink",colour="orange")
summary(data0)
summary(data1)

sd(data1)
sd(data1$label.occurrences)
sd(data1$event)
sd(data1$edges)

######################MODEL_3 MODEL_bpmn
data0=read.csv("bpmn3.csv",sep = ",", head=T)
data1=read.csv("bpmn2.csv",sep = ",", head=T)
str(data1)
par(mar=c(2.5,2,2,2)+2, pty = "s")
options(digits=4)
###########Principal component analysis
data11=data1[,-1]
data11=data11[,-(11:13)]
q = scale(data11)
cor(q)
library(caret)
library(corrplot)
corrplot(cor(q),order="hclust",tl.cex=0.7)
PCA <- princomp(q,cor=T)
###Interpretation of results
par(mar=c(4,4,2,1),cex=0.75)
#Crushed stone diagram, decide the number of selected principal components
screeplot(PCA,type="lines")  

summary(PCA, loadings = T)
PCA$loadings

#####Analysis of plots
summary(data0)
summary(data1)




