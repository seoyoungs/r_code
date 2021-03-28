#프로그램4.1       
trt<-c(1,1,1,1, 2,2,2,2, 3,3,3,3) #세로 줄
block<-c(1,2,3,4, 1,2,3,4, 1,2,3,4) #가로 줄
y<-c(20,22,18,25,
16,18,17,19,
30,34,29,27)
trt<-as.factor(trt)
block<-as.factor(block)
mean(y) #전체평균
tapply(y, trt,mean) #처리(세제종류)별 평균 
tapply(y,trt,var)     #처리(세제종류)별 분산
tapply(y,block,mean)  #블록(실험일)별 평균
tapply(y,block,var)  #블록(실험일)별 분산

tapply(y, trt,mean)-mean(y) #처리효과의 추정값
tapply(y,block,mean)-mean(y) #블록효과의 추정값

aov.out<-aov(y~trt+block) #분산분석만드는 과정
summary(aov.out)

pairwise.t.test(y,trt,p.adjust="none")  #treatment에 대한 LSD

aov.out$fitted #hat y
aov.out$residuals #잔차
shapiro.test(aov.out$residuals) #잔차분석
plot(aov.out) #잔차에 대한 그림

#프로그램4.2 csv데이터를 이용해 확률화완전블록설계
b=read.csv(file="C:/Users/김서영/Desktop/문서/2020년 1학기/실험계획/car.csv", header=TRUE)
attach(b)
additive<-as.factor(additive) #treatment(세로줄)
car<-as.factor(car)   #block(가로줄)
par(mfrow=c(1,2)) 
plot(y~additive); plot(y~car) #첨가제,자동차종류에 따른 상자그림
tapply(y,additive,mean); tapply(y,car,mean) #첨가제별, 자동차별처리평균
fit<-lm(y~car+additive)
anova(fit) #분산분석표

pairwise.t.test(y,additive,p.adjust="none")
detach(b)

#프로그램4.3 균형불완비블록설계
bb<-read.csv(file="C://Users/김서영/Documents/r프로그램/b.csv", header=TRUE)
bb
attach(bb)
trt<-as.factor(trt)
block<-as.factor(block)
par(mfrow=c(1,2))
plot(y~trt); plot(y~block)
fit<-lm(y~ trt+block,data=bb)
anova(fit)
fit.b<-lm(y~block+trt, data=bb)
anova(fit.b)
tapply(y,trt,mean)
tapply(y, block, mean)
pairwise.t.test(bb$y, bb$block, p.adjust="none")
detach(bb)


















