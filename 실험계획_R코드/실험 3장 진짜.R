life=c(rep(1,6), rep(2,6), rep(3,6))
y=c(100,96,98,96,92,95,
76,80,84,84,78,82,
108,100,101,98,102,100)
sol=cbind(life,y)
life<-as.factor(life)

sol2=data.frame(sol)

tapply(y, life, mean) #3.1번에 2번 평균
tapply(y, life, sd) #3.1번에 2번 표준편차
boxplot(y~life) #3.1번에 3번 상자그림

aov1=aov(y~life) #avona분산분석
summary(aov1)

par(mfrow=c(2,2))
plot(aov1)
pairwise.t.test(y,life,p.adjust="none",pool.sd=TRUE) #LSD
a.tukey = TukeyHSD(aov1, ordered=TRUE) #tukey
a.tukey

y.lm=lm(y~life)
y.lm$residuals #잔차 3.1번
y.lm$fitted.values #추정값

#13번 문제
k<-3
n<-length(y)
m<-tapply(y, life, mean);m 
ni<-tapply(y, life, length);ni 
c1<-c(1,1,-2) 
sum((c1^2)/ni) 
mse<-10 
f<-sum(c1*m)^2/(mse*sum(c1^2/ni)) 
1-pf(f,length(life)-1,n-k)

#3.2번
#2번에 3
n<-27
c2=c(1,-1,0)
ni2<-c(9,9,9)
m2<-c(240/9,320/9,180/9)
k<-3
mse2=1440/24;mse2
f2=sum(c2*m2)/sqrt((mse2*sum(c2^2/ni2)))
f2
2*pt(f2,df=n-k)

#2번에 4
c3=c(1,-2,1)
f3=sum(c3*m2)/sqrt((mse2*sum(c3^2/ni2)))
f3
2*pt(f3, df=n-k)


#3.3번
data1=c(rep("a",6), rep("b",6), rep("c",6), rep("d",6), rep("e",6))
z=c(55,55,57,54,54,56,
60,61,60,60,60,60,
70,72,73,68,77,77,
72,72,72,70,68,60,
65,66,60,64,65,65)
sol1=cbind(data1,z)
sol2=data.frame(sol1)

tapply(z,data1, mean)
tapply(z, data1, sd)
boxplot(z~data1)

aov1=aov(z~data1) #avona분산분석
summary(aov1)
pairwise.t.test(z,data1, p.adjust="none",pool.sd=TRUE)
z.tukey = TukeyHSD(aov1, ordered=TRUE)
z.tukey

z.lm=lm(z~data1)
z.lm$residuals
z.lm$fitted.values

par(mfrow=c(2,2))
plot(aov1)

#연습문제 3.9
display<-c(rep(1,5), rep(2,5), rep(3,5))
y<-c(31, 30, 23, 12, 3,
62, 40, 24, 30, 35,
53, 27, 60, 90, 70)
sol<-cbind(display, y); head(sol)
sol2<-data.frame(sol); head(sol2)
tapply(y, display, mean) #방법별 평균
tapply(y, display, sd)    #방법별 표준편차
boxplot(y~display)   #방법별 상자그림
aov1<-aov(y~display)
summary(aov1) #분산분석표










