setwd("~/Desktop/ECON613")
library(dplyr)
#library(plyr)
library(data.table)
datsss = read.csv("datsss.csv")
datjss = read.csv("datjss.csv")
datstu = read.csv("datstu.csv")


#Ex1
#number of students(340823)
dim(datstu)[[1]]
#numb of schools(898)
n_distinct(datsss$schoolcode)
#numbers of programs(270)
n_distinct(datstu$choicepgm1:datstu$choicepgm6)
#Number of Choices(338936)
datstu = datstu %>% mutate(choice1=paste0(schoolcode1,choicepgm1),
                           choice2=paste0(schoolcode2,choicepgm2),
                           choice3=paste0(schoolcode3,choicepgm3),
                           choice4=paste0(schoolcode4,choicepgm4),
                           choice5=paste0(schoolcode5,choicepgm5),
                           choice6=paste0(schoolcode6,choicepgm6))
dat = select(datstu,choice1,choice2,choice3,choice4,choice5,choice6)
n_distinct(dat$choice1,dat$choice2,dat$choice3,dat$choice4,dat$choice5,dat$choice6)
#Missing test score(179887)
sum(is.na(datstu$score))
#Apply to less than 6 school choices(17088)
subset=select(datstu,schoolcode1,schoolcode2,schoolcode3,schoolcode4,schoolcode5,schoolcode6)
list<-which(rowSums(is.na(subset))>0)
#Apply to less than 6 program choice
subset1<-select(datstu,choicepgm1,choicepgm2,choicepgm3,choicepgm4,choicepgm5,choicepgm6)
list<-which(rowSums(is.na(subset))>0)
#Apply to same school(different programs)



#Ex2

#remove students with na score
datstu=datstu[which(!is.na(datstu$score)),]
#remove students with na rankplace
datstu=datstu[which(!is.na(datstu$rankplace)),]
#remove students with rankplace=99
datstu<- datstu[!(datstu$rankplace== 99),]
#groupby
row_name=c("X","score","age","male","schoolcode","choicepgm","jssdistrict","rankplace")
datstu1=datstu[,c(1:4,5,11,17,18)]
groupby_set=datstu1%>%group_by(datstu1$schoolcode)%>%summarise(cutoff=min(score),size=length(X),quality=mean(score))

#remove missing values and repeated rows
datss_2=datsss[which(!is.na(datsss$ssslong)),]
datss_2=datss_2[which(!duplicated(datss_2$schoolcode)),]
#merge datss_2 with groupby_set
dataset_2=merge(groupby_set,datss_2[,c(3:6)],by.x="datstu1$schoolcode",by.y="schoolcode",all.x = TRUE)
#we save information of column variables in dataset_2
#create rows corresponds with school and program
datstu = datstu %>% mutate(choice1=paste0(schoolcode1,choicepgm1),
                           choice2=paste0(schoolcode2,choicepgm2),
                           choice3=paste0(schoolcode3,choicepgm3),
                           choice4=paste0(schoolcode4,choicepgm4),
                           choice5=paste0(schoolcode5,choicepgm5),
                           choice6=paste0(schoolcode6,choicepgm6))
dat = select(datstu,choice1,choice2,choice3,choice4,choice5,choice6)
dat_long = gather(dat,'key','value')
#combine dataset_2 with dat_long
dat_long=merge(dat_long,dataset_2[,c(1:7)],by.x ="value",by.y="refined_data$schoolcode",all.x = TRUE)



#exercise 3

sssjss=left_join(datsss,datjss)
z=sqrt((69.172 * (sssjss$ssslong-sssjss$point_x) * cos(sssjss$point_y/57.3))^2+(69.172 * (sssjss$ssslat-sssjss$point_y))^2)

#EX4
#GROUPBY rankplace
groupby_set_2=datstu1%>%group_by(datstu1$rankplace)%>%summarise(cutoff=min(score),size=length(X),quality=mean(score))
#average of cutoff/quaility/distance
summary(groupby_set_2) 
#sd of cutoff/quaility/distance
sd(groupby_set_2$cutoff) 
sd(groupby_set_2$size)
sd(groupby_set_2$quality)
#average of distance
summary(z)
#sd of distance
sd(z)
#redo table, diffrentiating by student test score quantiles
quantile(datstu$score,na.rm=TRUE,probs = seq(0,1, 0.25))
datstu$quantile[datstu$score<=256]="0%~25%"
datstu$quantile[(datstu$score>256) & (datstu$score<=289)]="25%~50%"
datstu$quantile[(datstu$score>289) & (datstu$score<=330)]="50%~75%"
datstu$quantile[(datstu$score>330) & (datstu$score<=469)]="75%~100%"

#EX5
x1=runif(10000, min = 1, max = 3)
x2=rgamma (10000,shape=3,scale=2)
x3=rbinom(10000,size=1,prob=0.3)
error=rnorm(10000,mean=2,sd=1)
cal_df=data.frame(x1,x2,x3,error)
cal_df$y=0.5+1.2*cal_df$x1-0.9*cal_df$x2+0.1*cal_df$x3+cal_df$error
y_mean=mean(cal_df$y)
cal_df$ydum[cal_df$y>y_mean]=1 
cal_df$ydum[is.na(cal_df$ydum)]=0 
#Exercise 6 
#calculate the correlation between Y and X1
cor(cal_df$ydum,x1)
#coefficient is 0.1724073
#outcome of the regression 
X= c(1,x1,x2,x3)
#coefficients of the regression
cal_df$constant=1
X=as.matrix(cal_df[,c(7,1,2,3)])
solve(t(X)%*%X)%*%t(X)%*%as.matrix(cal_df$y)
#coeffiicent 
constant= 2.50382938
beta1= 1.19739383
beta2=-0.90149371
beta3=0.08932417







