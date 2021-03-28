ma=c("a1","a2","a3", "a1","a2","a3", "a1","a2","a3", "a1","a2","a3")
ma=c(1,2,3, 1,2,3, 1,2,3, 1,2,3)
on=c("b1","b1","b1", "b2","b2","b2", "b3","b3","b3", "b4","b4","b4")
ma=
on=c(200,200,200, 250,250,250, 300,300,300, 350,350,350)
y=c(30,28,38, 35,32,39, 37,40,41, 36,41,40)
ma<-as.factor(ma)
on<-as.factor(on)
L<-aov(y~ma+on+ma:on)
summary(L)

L<-aov(y~ma*on)  #2번
anova(L)

#3번
bb=read.csv(file="C:/Users/김서영/Documents/r프로그램/ex3.csv", header=TRUE)
attach(bb)
a<-as.factor(a)
b<-as.factor(b)
c<-as.factor(c)
rep<-as.factor(rep)
tapply(y,a,mean);tapply(y,a,var)
tapply(y,b,mean);tapply(y,b,var)
tapply(y,c,mean);tapply(y,c,var)

tapply(y,c,mean) #3번

aov2<-aov(y~a*b*c)
model.tables(aov2,type="effects")
anova(aov2) #5번

interaction.plot(c,a,y,bty="l")

tapply(y,a:c,mean)#8

cbind(y,a,b,c,rep,aov2$fitted.values) #9
cbind(y,a,b,c,rep,aov2$residual)

aov3<-aov(y~a+b+c)
anova(aov3)

par(mfrow=c(2,2))
plot(aov2)












