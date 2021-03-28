#1번
a<-c(1,1,2,2, 1,1,2,2)
b<-c(1,1,1,1, 2,2,2,2)
y<-c(0,2,4,8, 4,6,8,10)
a<-as.factor(a)
b<-as.factor(b)
tapply(y,a,mean);tapply(y,a,sd)
tapply(y,b,mean);tapply(y,b,sd)
interaction.plot(a,b,y,bty="l") #3번
tapply(y,a,mean) #4번
tapply(y,b,mean) 
tapply(y,a:b,mean) #5번
L<-aov(y~a*b)  #6번
anova(L) #8번
fit1<-model.tables(L, type="effects")
fit1
predict(L) #11번
cbind(y,a,b,L$residual)


#2번
a<-c(1,1,2,2, 1,1,2,2)
b<-c(1,1,1,1, 2,2,2,2)
y<-c(35,39,101,95, 67,66,34,31)

a<-as.factor(a)
b<-as.factor(b)
tapply(y,a,mean);tapply(y,a,sd)
tapply(y,b,mean);tapply(y,b,sd)

interaction.plot(a,b,y,bty="l")
tapply(y,a,mean) #4번
tapply(y,b,mean) #5번
tapply(y,a:b,mean) #6번

L=aov(y~a*b)
model.tables(L,type="effects") #이게 모수추정
#추정값은 위의 내용에서 추가로 계산 하여야한다.

summary(L)

#7.4
dd1=read.csv(file="C:/Users/김서영/Documents/r프로그램/7.4.csv", header=TRUE)
dd1
attach(dd1)
a<-as.factor(a)
b<-as.factor(b)
rep<-as.factor(rep)

tapply(y,a,mean)
tapply(y,b,mean)
tapply(y,a:b,mean)

#7.5
d2=read.csv(file="C:/Users/김서영/Documents/r프로그램/ex7.5.csv", header=TRUE)
d2
attach(d2)
a<-as.factor(a)
b<-as.factor(b)
c<-as.factor(c)
rep<-as.factor(rep)
#각 요인별 수준별 평균과 표준편차
tapply(y,a,mean);tapply(y,a,sd)
tapply(y,b,mean);tapply(y,b,sd)
tapply(y,c,mean);tapply(y,c,sd)
#요인별 상자그림
par(mfrow=c(1,3))
boxplot(y~a, sub="factora")
boxplot(y~b, sub="factorb")
boxplot(y~c, sub="factorc")
#주효과 abc
z2<-aov(y~a*b*c)
model.tables(z2,type="means")
tapply(y,a:b:c,mean)#이거로 쓰는게 더 쉬움 번갈아가면서 +-하기

#이차 상호작용효과 a:b:c는 제외
L2<-aov(y~a*b*c*a:b*b:c*a:c*a:b:c)
model.tables(L2,type="means")
#모든 효과를 포함한 완전모형에 대한 분산분석표
anova(z2)
#유의하지 않은 상호작용효과 합해 축수모형
L2_2<-aov(y~a+b+c+b:c)
anova(L2_2)

#7장 연습 7.6
a<-c(1,1,1,1, 2,2,2,2, 1,1,1,1, 2,2,2,2)
b<-c(1,1,2,2, 1,1,2,2, 1,1,2,2, 1,1,2,2)
c<-c(1,2,1,2, 1,2,1,2, 1,2,1,2, 1,2,1,2)
y<-c(1595,1745,1835,1838,1573,2185,1700,1717,
1578,1689,1823,1614,1592,1685,1815,1814)

a<-as.factor(a)
b<-as.factor(b)
c<-as.factor(c)

#각 요인에 대해 수준별 평균과 표준편차
tapply(y,a,mean);tapply(y,a,sd)
tapply(y,b,mean);tapply(y,b,sd)
tapply(y,c,mean);tapply(y,c,sd)

#요인 a에 대한 요인 b의 상호작용 그림
interaction.plot(a,b,y,bty="l")
#각 요인에 대한 주효과
z3<-aov(y~a*b*c)
model.tables(z3,type="effects")

L1<-aov(y~a+b+c+ a:c+b:c+a:c +a:b:c)
anova(L1)

#모수추정
model.tables(zz)


#7.7
d1=read.csv(file="C:/Users/김서영/Documents/r프로그램/7.7.csv", header=TRUE)
d1
attach(d1)
a<-as.factor(a)
b<-as.factor(b)
c<-as.factor(c)
d<-as.factor(d)
dd<-aov(y~a+b+c+d+a:b+a:c+a:d+b:c+b:d+c:d+a:b:c+a:b:d+a:c:d+b:c:d)
anova(dd)
d3<-aov(y~a+b+c+d)
anova(d3)







