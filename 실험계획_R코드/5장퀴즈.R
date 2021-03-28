#2018년 5장
driver<-c(1,1,1,1, 2,2,2,2, 3,3,3,3, 4,4,4,4)
car<-c(1,2,3,4, 1,2,3,4, 1,2,3,4, 1,2,3,4)
tire<-c("A","B","C","D",
"D","A","B","C",
"C","D","A","B",
"B","C","D","A")
y<-c(26,19,28,22,
21,23,20,24,
29,22,24,20,
22,28,20,28)
driver<-as.factor(driver)
car<-as.factor(car)
tire<-as.factor(tire)
by(y, tire, FUN=function(x){ c(mean(x)) })
by(y, car, FUN=function(x){ c(mean(x)) })
a1<-lm(y~driver+car+tire)
anova(a1)
pairwise.t.test(y,tire,p.adjust="none")



#5장 연습 3번
wlf<-c(1,1,1,1, 2,2,2,2, 3,3,3,3, 4,4,4,4)
rl<-c(1,2,3,4, 1,2,3,4, 1,2,3,4, 1,2,3,4)
pnut<-c("C","A","B","D",
"A","B","D","C",
"B","D","C","A",
"D","C","A","B")
y<-c(26,19,28,29,
23,20,24,29,
28,20,29,27,
25,17,28,30)
wlf<-as.factor(wlf)
rl<-as.factor(rl)
pnut<-as.factor(pnut)
by(y, pnut, FUN=function(x){ c(mean(x),sd(x)) })
by(y, rl, FUN=function(x){ c(mean(x),sd(x)) })
by(y, wlf, FUN=function(x){ c(mean(x),sd(x)) })
#종류별 상자그림
par(mfrow=c(1,3))
plot(y~wlf+rl+pnut)

a2<-lm(y~wlf+rl+pnut)
anova(a2)
a3<-lm(y~wlf+pnut+rl)
anova(a3)
a4<-lm(y~rl+pnut+wlf)
anova(a4)
pairwise.t.test(y,pnut,p.adjust="none")

#4번
pro<-c(1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3, 4,4,4,4,4, 5,5,5,5,5)
lo<-c(1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5)
ele<-c("A","B","C","D","E",
"B","C","D","E","A",
"C","D","E","A","B",
"D","E","A","B","C",
"E","A","B","C","D")
y<-c(64,61,62,62,62,
62,62,63,62,63,
61,62,63,63,63,
63,64,63,63,63,
62,61,63,63,62)
pro<-as.factor(pro)
lo<-as.factor(lo)
ele<-as.factor(ele)
by(y, ele, FUN=function(x){ c(mean(x)) })
by(y, pro, FUN=function(x){ c(mean(x)) })
by(y, lo, FUN=function(x){ c(mean(x)) })

par(mfrow=c(1,3))
plot(y~ele+pro+lo)

a5<-lm(y~pro+lo+ele)
anova(a5)
a6<-lm(y~lo+ele+pro)
anova(a6)
a7<-lm(y~ele+pro+lo)
anova(a7)

par(mfrow=c(2,2))
plot(a5)


#5장 연습 1번
driver2<-c(1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3, 4,4,4,4,4, 5,5,5,5,5)
car2<-c(1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5)
trt<-c("A","B","C","D","E",
"E","A","B","C","D",
"D","E","A","B","C",
"C","D","E","A","B",
"B","C","D","E","A")
y<-c(26,19,28,29,20,
21,23,20,24,29,
29,22,23,20,24,
28,28,20,28,15,
17,28,23,20,25)
x1<-cbind(car2,driver2,trt,y)
x1
trt<-as.factor(trt)
driver2<-as.factor(driver2)
car2<-as.factor(car2)
by(y, trt, FUN=function(x){ c(mean(x),sd(x)) })
by(y, car2, FUN=function(x){ c(mean(x),sd(x)) })
by(y, driver2, FUN=function(x){ c(mean(x),sd(x)) })
a8<-lm(y~car2+driver2+trt)
anova(a8)
a9<-lm(y~trt+car2+driver2)
anova(a9)
a10<-lm(y~driver2+trt+car2)
anova(a10)
pairwise.t.test(y,trt,p.adjust="none")


#타오햇 구하기
tapply(y, group, mean)-mean(y)











