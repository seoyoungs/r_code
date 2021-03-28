#1번
data2=c(rep(1,4), rep(2,4), rep(3,4), rep(4,4)) #1=a, 2=b, 3=c, 4=d
Z=c(55, 55, 57, 54,
 60, 61, 62, 64,
 70, 72, 73, 77,
 41, 42, 43, 48)
sol1=cbind(data2, Z)
data2<-as.factor(data2)
tapply(Z, data2, mean) #표준과 표준편차
tapply(Z, data2, sd)

model.tables(aov1) #코딩 방법의 효과 (4번)
stripchart(Z ~ data2, vertical=TRUE) #strip chart

aov1=aov(Z~data2) #anova분석(8,9번 답)
summary(aov1)

predict(aov1) #10번 반응 추정값
eij=Z-predict(aov1) #11번 잔차
j=aov1$residuals
round(j, digit=2) #11번 잔차 소수둘째자리 

#12번 
k<-4
n<-length(Z)
m<-tapply(Z, data2, mean); m
ni<-tapply(Z,data2,length);ni 
c1<-c(1,1,-1,-1)
sum((c1^2)/ni)
mse<-5.7
f<-sum(c1*m)/sqrt(mse*sum(c1^2/ni))
f 
1-pf(f,length(data2)-1,n-k) #또는 pf(f,length(data2)-1,n-k, lower.tail=F) 쓰기

#LSD 16번
pairwise.t.test(Z,data2,p.adjust="none",pool.sd=TRUE)


#5번 4장 문제 확률화완전블록설계
sam<-c(1,1,1, 2,2,2, 3,3,3, 4,4,4)
lab<-c(1,2,3, 1,2,3, 1,2,3, 1,2,3)
y<-c(5.4, 6.1, 6.2,
   5.3, 6.4, 6.4,
   5.4, 6.5, 6.7,
   5.5, 6.5, 6.3)
sam<-as.factor(sam)
lab<-as.factor(lab)

aov3<-aov(y~lab+sam)
summary(aov3) #(4)번

#LSD (8)번
tapply(y,lab, mean)
pairwise.t.test(y, lab, p.adjust="none")



#6번
oven<-c(1,1,1, 2,2,2, 3,3,3, 4,4,4)
snack<-c(1,2,3, 1,2,4, 1,3,4, 2,3,4)
y<-c(15, 20, 13,
    11, 20, 12,
   10, 17, 12,
   21, 13, 17)

b1=read.csv(file="C:/Users/김서영/Documents/r프로그램/num6.csv", header=TRUE)
b1
attach(b1)
oven<-as.factor(oven) #처리
snack<-as.factor(snack) #블록

tapply(y, snack, mean) #과자재료(블록) 맛점수 평균 1=a, 2=b, 3=c, 4=d
tapply(y, snack, sd)

#분산분석(과자재료에 따른 과자맛차이) 
fit.b<-lm(y~oven+snack)
anova(fit.b)





















