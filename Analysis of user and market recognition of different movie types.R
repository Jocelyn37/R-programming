library(ggplot2)
library(sysfonts)
library(showtext)

setwd("/Users/apple/Desktop")
data0<-read.table("电影评分数据.txt",header =T,stringsAsFactors =F,encoding = 'UTF-8')
head(data0)
dim(data0)
str(data0)

######## Data is converted to matrix #######

user <- unique(data0$用户);user
movie <- unique(data0$电影);movie
tag <- unique(data0$标签);tag

#Generate matrix
data1 <- matrix(nrow=length(movie), ncol = length(user),dimnames = list(movie,user))
data1<-cbind(data1,"flag"=rep(NA,length(movie)))

#Adding movie rating
for(i in 1:dim(data0)[1]){
  a <- data0$电影[i]
  b <- data0$用户[i]
  data1[a,b]<-data0$评分[i]
}

#Adding the most frequent movie tags
for(i in movie){
  x <- data0[data0$电影== i,]
  t <- names(which.max(table(x$标签)))
  data1[i,"flag"] <- t
}

as.data.frame(table(data1[,"flag"]))
tag
data0[data0$标签 =="恐怖",]
data1["夜半歌声",]

######## Descriptive analysis of ratings of different types of movies #######

data2 <- as.data.frame(data1) 
x <- dim(data2)[1]
y <- dim(data2)[2]-1

for(i in 1:y){
  data2[,i] <- as.numeric(data2[,i])
}

#1.drama
feature <- data2[data2$flag =="剧情",-497]
res1 <- feature[!is.na(feature)]

#Number of rated users
number1 <- 0
for(i in 1:496){
  if(sum(!is.na(feature[,i]))!= 0)
    number1 <- number1+1
}
number1;

#mean,variation,derivation
mean_feature <- mean(res1);mean_feature
sd_feature <- sd(res1);sd_feature
hist(res1,breaks=10,col="pink",border="black")

#2.action
action <- data2[data2$flag =="动作",-497]
res2 <- action[!is.na(action)]

#Number of rated users
number2 <- 0
for(i in 1:496){
  if(sum(!is.na(action[,i]))!= 0)
    number2 <- number2+1
}
number2;

#mean,variation,derivation
mean_action <- mean(res2);mean_action
sd_action <- sd(res2);sd_action
hist(res2,breaks=10,col="pink",border="black")

#3.cartoon
cartoon <- data2[data2$flag =="动画",-497]
res3 <- cartoon[!is.na(cartoon)]

#Number of rated users
number3 <- 0
for(i in 1:496){
  if(sum(!is.na(cartoon[,i]))!= 0)
    number3 <- number3+1
}
number3;

#mean,variation,derivation
mean_cartoon <- mean(res3);mean_cartoon
sd_cartoon <- sd(res3);sd_cartoon
hist(res3,breaks=10,col="pink",border="black")

#4.comedy
comedy <- data2[data2$flag =="喜剧",-497]
res4 <- comedy[!is.na(comedy)]

#Number of rated users
number4 <- 0
for(i in 1:496){
  if(sum(!is.na(comedy[,i]))!= 0)
    number4 <- number4+1
}
number4;

#mean,variation,derivation
mean_comedy <- mean(res4);mean_comedy
sd_comedy <- sd(res4);sd_comedy
hist(res4,breaks=10,col="pink",border="black")

#5.family
family <- data2[data2$flag =="剧情",-497]
res5 <- family[!is.na(family)]

#Number of rated users
number5 <- 0
for(i in 1:496){
  if(sum(!is.na(family[,i]))!= 0)
    number5 <- number5+1
}
number5;

#mean,variation,derivation
mean_family <- mean(res5);mean_family
sd_family <- sd(res5);sd_family
hist(res5,breaks=10,col="pink",border="black")

#6.Suspense film
suspense <- data2[data2$flag =="悬疑",-497]
res6 <- suspense[!is.na(suspense)]

#Number of rated users
number6 <- 0
for(i in 1:496){
  if(sum(!is.na(suspense[,i]))!= 0)
    number6 <- number6+1
}
number6;

#mean,variation,derivation
mean_suspense <- mean(res6);mean_suspense
sd_suspense <- sd(res6);sd_suspense
hist(res6,breaks=10,col="pink",border="black")

#7.soft porn
horny <- data2[data2$flag =="情色",-497]
res7 <- horny[!is.na(horny)]

#Number of rated users
number7 <- 0
for(i in 1:496){
  if(sum(!is.na(horny[,i]))!= 0)
    number7 <- number7+1
}
number7;

#mean,variation,derivation
mean_horny <- mean(res7);mean_horny
sd_horny <- sd(res7);sd_horny
hist(res7,breaks=10,col="pink",border="black")

#8.scary
horror <- data2[data2$flag =="惊悚",-497]
res8 <- horror[!is.na(horror)]

#Number of rated users
number8 <- 0
for(i in 1:496){
  if(sum(!is.na(horror[,i]))!= 0)
    number8 <- number8+1
}
number8;

#mean,variation,derivation
mean_horror <- mean(res8);mean_horror
sd_horror <- sd(res8);sd_horror
hist(res8,breaks=10,col="pink",border="black")

#9.war
war <- data2[data2$flag =="战争",-497]
res9 <- war[!is.na(war)]

#Number of rated users
number9 <- 0
for(i in 1:496){
  if(sum(!is.na(war[,i]))!= 0)
    number9 <- number9+1
}
number9;

#mean,variation,derivation
mean_war <- mean(res9);mean_war
sd_war <- sd(res9);sd_war
hist(res9,breaks=10,col="pink",border="black")

#10.romantic
love <- data2[data2$flag =="爱情",-497]
res10 <- love[!is.na(love)]

#Number of rated users
number10 <- 0
for(i in 1:496){
  if(sum(!is.na(love[,i]))!= 0)
    number10 <- number10+1
}
number10;

#mean,variation,derivation
mean_love <- mean(res10);mean_love
sd_love <- sd(res10);sd_love
hist(res10,breaks=10,col="pink",border="black")

#11.sci-fi
science <- data2[data2$flag =="科幻",-497]
res11 <- science[!is.na(science)]

#Number of rated users
number11 <- 0
for(i in 1:496){
  if(sum(!is.na(science[,i]))!= 0)
    number11 <- number11+1
}
number11;

#mean,variation,derivation
mean_science <- mean(res2);mean_science
sd_science <- sd(res2);sd_science
hist(res11,breaks=10,col="pink",border="black")

#12.document
documentary <- data2[data2$flag =="纪录片",-497]
res12 <- documentary[!is.na(documentary)]

#Number of rated users
number12 <- 0
for(i in 1:496){
  if(sum(!is.na(documentary[,i]))!= 0)
    number12 <- number12+1
}
number12;

#mean,variation,derivation
mean_documentary <- mean(res12);mean_documentary
sd_documentary <- sd(res12);sd_documentary
hist(res12,breaks=10,col="pink",border="black")

######## Descriptive analysis of overall data #######

data0$标签 <- as.factor(data0$标签)
data0$评分 <- as.factor(data0$评分)
table(data0[,c(3,4)])
 
dev.new() 
showtext.begin()

#Bar graph
ggplot(data0,aes(x=标签))+geom_bar(fill="pink")
ggplot(data0,aes(x=评分))+geom_bar(fill="pink")
ggplot(data0,aes(x=标签,fill=评分))+geom_bar(position="fill")
ggplot(data0,aes(x=标签,fill=评分))+geom_bar(position = "dodge")

#Pie chart
ggplot(data0,aes(x=评分,fill=标签))+geom_bar()+coord_polar(theta="y")

#Boxplot
data0$评分 <- as.numeric(data0$评分)
ggplot(data0, aes(x=标签,y=评分))+geom_boxplot(fill=rainbow(16))+labs(title = '箱线图')

######## variance analysis #######
x <- data0$评分
A <- data0$标签
movie <- data.frame(x,A)
movie.aov <- aov(x~A,data = movie)
summary(movie.aov)

