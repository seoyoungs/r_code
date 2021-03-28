#2017중간고사
group<-c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5))
y<-c(55, 55, 57, 54, 54,
     60, 61, 60, 60, 60,
     70, 72, 73, 68, 77,
     72, 72, 72, 70, 68,
     65, 66, 60, 64, 65)
sol<- cbind(group,y)
group<-as.factor(group)
tapply(y, group, mean)
tapply(y, group, sd)

#분산분석
aov2<-aov(y~group)
summary(aov2)
#LSD
pairwise.t.test(y, group, p.adjust="none")

predict(aov2) #Y 반응 추정값
yhat<-aov2$fitted.value
yhat #Y 반은 추정값 구하는 방법 2
e11=y-predict(aov2) 
aov2$residuals #잔차

tapply(y, group, mean)-mean(y) #타오 햇 구하기 1번에10번

#13번 교과서 96p
k<-5
n<-length(y)
m3<-tapply(y, group, mean);m
ni3<-tapply(y, group, length); ni3
c4<-c(2,2,-3,-3,2)
sum(c4^2/ni3)
mse<-4.38
f<-sum(c4*m3)^2/(mse*sum(c4^2/ni3))
f
1-pf(f,1,n-k)


#직교대비 처리간 대비 1번에 14,15
k<-5
n<-25
mse<-4.38
t=(55-72)/sqrt(mse*sum(1/5+1/5))
t
2*pt(t,df=n-k)

lo=(55-72)-2*sqrt(mse*sum(1/5+1/5))
lo
up=(55-72)+2*sqrt(mse*sum(1/5+1/5))
up

 
#3번
b2=read.csv(file="C:/Users/김서영/Documents/r프로그램/time.csv", header=TRUE)
b2
attach(b2)
block<-as.factor(block)
m<-as.factor(m)
tapply(y, m, mean)
tapply(y, m, sd)

fit<-aov(y~m+block)
summary(fit)


trt<-c(rep(1,3),rep(1.5,3),rep(2,3),rep(2.5,3))
block<-c(1,2,3,1,2,3,1,2,3,1,2,3)
y<-c(10,6,6,
7,6,6,
5,3,4,
6,4,3)
x1<-cbind(trt, block, y)
trt<-as.factor(trt)
block<-as.factor(block)
x1
aov5.5<-aov(y~block+trt)
summary(aov5.5)

#여기까지가 분산분석 만드는 두가지 방법이다. factor지정시 그냥 data지정안하고 위에꺼처럼 하기

pairwise.t.test(y, m, p.adjust="none") #거리 LSD
x1<-cbind(trt, block, y)
trt<-as.factor(trt)
block<-as.factor(block)
x1

#3번에 10직교대비
contrasts(m)<-contr.poly(levels(m))
contrasts(m)
round(contrasts(m),digits=2)


#4
b3=read.csv(file="C:/Users/김서영/Documents/r프로그램/2017.6.csv", header=TRUE)
b3
attach(b3)
oven<-as.factor(oven)
snack<-as.factor(snack)

tapply(y, snack, mean)
tapply(y, snack, sd)
fit<-lm(y~oven+snack)
anova(fit)
pairwise.t.test(y, snack, p.adjust="none") #과자 LSD















