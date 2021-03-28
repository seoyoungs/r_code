b1=read.csv(file="C:/Users/김서영/Documents/r프로그램/kk2020.csv", header=TRUE)
b1
attach(b1)
k<-as.factor(k)
levels(k)
round(tapply(y,k,mean), digits=2)
round(tapply(y,k,sd), digits=2)

aov1<-aov(y~k)
summary(aov1)

round(tapply(y,k,mean), digits=2)
pairwise.t.test(y,k,p.adjust="none",pool.sd=TRUE)

k2<-3
n<-length(y)
m<-tapply(y, k, mean);m 
ni<-tapply(y, k, length);ni 
c1<-c(1,-2,1) 
sum((c1^2)/ni) 
mse<-2.24
f<-sum(c1*m)^2/(mse*sum(c1^2/ni)) 
f
1-pf(f,length(k)-1,n-k2)

c2<-c(2,-1,-1) 
sum((c2^2)/ni) 
mse<-2.24
f<-sum(c2*m)^2/(mse*sum(c2^2/ni)) 
f
1-pf(f,length(k)-1,n-k2)

c3<-c(1,-2,1)
sum((c3^2)/ni) 
mse<-2.24
f<-sum(c3*m)^2/(mse*sum(c3^2/ni)) 
f
1-pf(f,length(k)-1,n-k2)



#2번
trt<-c(1,1,1, 2,2,2, 3,3,3, 4,4,4, 5,5,5) #trt=위치
block<-c(1,2,3, 1,2,3, 1,2,3, 1,2,3, 1,2,3) #block=콩 종류
y<-c(12, 21, 20,
12, 19, 18,
14, 18, 18,
14, 20, 20, 
15, 12, 17)
trt<-as.factor(trt)
block<-as.factor(block)

aov2<-aov(y~block+trt)
summary(aov2)

tapply(y,block,mean)
tapply(y,block, sd)

tapply(y, trt, mean)
pairwise.t.test(y, trt, p.adjust="none")



#3번
trt2<-c(2,2,2, 4,4,4, 6,6,6, 8,8,8, 10,10,10, 12,12,12, 14,14,14) #trt2=농도
block2<-c(4,6,7, 1,5,7, 1,2,6, 2,3,7, 1,3,4, 2,4,5, 3,5,6) #block2=실험일
y<-c(140,130,140,
114, 120, 117,
126, 120,119,
137, 117, 134,
132, 130, 138,
145, 148, 145,
120,118,124)
trt2<-as.factor(trt2) #처리
block2<-as.factor(block2) #블록

fit<-lm(y~block2+trt2) #농도
anova(fit)
fit.b<-lm(y~trt2+block2) #실험일
anova(fit.b)
















