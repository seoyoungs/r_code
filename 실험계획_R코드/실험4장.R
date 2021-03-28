#4장연습
trt=c(1,1,1,1, 2,2,2,2, 3,3,3,3, 4,4,4,4) #가로세로 다 이름있는경우
block=c(1,2,3,4, 1,2,3,4, 1,2,3,4, 1,2,3,4)
y=c(15,20,13,16,
11,20,15,18,
10,22,17,15,
12,19,13,17)
trt=as.factor(trt)
block=as.factor(block)
tapply(y,block,mean)
tapply(y,block,sd)

par(mfrow=c(1,2)) #종류별 상자그림
plot(y~trt);plot(y~block)

aov.out=aov(y~trt+block) #분산분석표
summary(aov.out)

pairwise.t.test(y,block,p.adjust="none") #배합비율에 대한 LSD

#4.3번
b1=read.csv(file="C:/Users/김서영/Desktop/문서/2020년 1학기/실험계획/ch4.csv",header=TRUE)
b1
attach(b1)
additive=as.factor(additive)
car=as.factor(car)
par(mfrow=c(1,2))
plot(y~additive);plot(y~car) #상자그림
tapply(y,car,mean) #과자재료평균
tapply(y,car,sd)

summary(aov(y~as.factor(additive)+as.factor(car),b1))#분산분석
pairwise.t.test(y, car, p.adjust="none") #block과자재료 LSD


#4.7 연습문제
b2=read.csv(file="C:/Users/김서영/Documents/r프로그램/tree.csv", header=TRUE)
b2
attach(b2)
trt=as.factor(trt)
exp=as.factor(exp)
tapply(y, trt, mean) #농도에 따른 종이의 강도 평균과 표준편차
tapply(y, trt, sd)
boxplot(y~trt) #농도에 따른 상자그림
fit<-lm(y~trt+exp) #농도에 따른 분산분석
fit.b<-lm(y~exp+trt) #실험일에 따른 분산분석
anova(fit)
anova(fit.b)







 