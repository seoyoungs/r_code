#2019년 기출
#4번
bb=read.csv(file="C:/Users/김서영/Documents/r프로그램/2019exam.csv", header=TRUE)
attach(bb)
a<-as.factor(a)
b<-as.factor(b)
c<-as.factor(c)
rep<-as.factor(rep)

tapply(y,a:b,mean)

plot(x=c(1,2), y=c(12.375,11.250), type="l", sub="c") #3번

tapply(y,a,mean)
tapply(y,b,mean)
tapply(y,c,mean)
tapply(y,a,var)
tapply(y,b,var)
tapply(y,c,var)

interaction.plot(a,b,y,bty="l") #5번
tapply(y,a:b,mean) #6번
model.tables(aov2,type="effects") #7번

aov2<-aov(y~a*b*c) #8번
cbind(y,a,b,c,rep,aov2$fitted.values)

anova(aov2) #11번 

L<-aov(y~a+b+c+a:c+b:c) #14번 
anova(L)

tapply(y,a:b:c,mean) #18번

#5번
aa=read.csv(file="C:/Users/김서영/Documents/r프로그램/2019.5.csv", header=TRUE)
attach(aa)
head(aa)
ap<-as.factor(ap)
on<-as.factor(on)
rep<-as.factor(rep)

interaction.plot(on,ap,y,bty="l") #2번
L1<-aov(y~ap*on) #3번
anova(L1)

fit1<-model.tables(L1, type="effects")
fit1





