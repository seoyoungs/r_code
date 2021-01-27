b1=read.csv(file="C:/Users/김서영/Documents/r프로그램/vertebral2.csv", header=TRUE)
attach(b1)
x=b1[,3:8]
p=ncol(x)
xbar=apply(x,2,mean) #표본평균벡터
xbar
vv=apply(x,2,var)
vv
v1=apply(x,2,sd)
v1
head(b1)

tapply(x1,group,mean);tapply(x1,group,sd) #x1이 그룹a일때와 그룹b일때의 평균과 표준편차
tapply(x2,group,mean);tapply(x2,group,sd)
tapply(x3,group,mean);tapply(x3,group,sd)
tapply(x4,group,mean);tapply(x4,group,sd)
tapply(x5,group,mean);tapply(x5,group,sd)
tapply(x6,group,mean);tapply(x6,group,sd)

max(x1);min(x1) #x1일때 최대 최소값
max(x2);min(x2)
max(x3);min(x3)
max(x4);min(x4)
max(x5);min(x5)
max(x6);min(x6)

par(mfrow=c(1,6)) #boxplot
plot(x1~group); plot(x2~group);plot(x3~group);
plot(x4~group);plot(x5~group);plot(x6~group)

plot(x1~group)
plot(x2~group)
plot(x3~group)
plot(x4~group)
plot(x5~group)
plot(x6~group)
#체르노프얼굴
install.packages("aplpack")
library(aplpack)
b1_1 <- b1[c(1:20),c("x1", "x2", "x3", "x4", "x5", "x6")] 
faces(b1_1, face.type = 1,  main = "Chernoff faces: face.type = 1")
#별그림
stars(b1_1)

d2=princomp(x, cor=TRUE) #상관행렬을 이용해 주성분분석
summary(d2)
round(L,3)
d2$loadings
d2$sdev #고유백터
plot(d2)
biplot(d2)
aa1=subset(b1, b1$group=='a') #그룹 a(abnormal)만 주성분 분석
xx1=aa1[,3:8]
dd1=princomp(xx1, cor = TRUE, scores = TRUE, covmat = NULL,
 subset = rep(TRUE, nrow(as.matrix(xx1))))
summary(dd1)
dd1$loadings
dd1$sdev
biplot(dd1)

aa2=subset(b1, b1$group=='b') #그룹 b(normal)만 주성분 분석
xx2=aa2[,3:8]
dd2=princomp(xx2, cor = TRUE, scores = TRUE, covmat = NULL,
subset = rep(TRUE, nrow(as.matrix(xx2))))
summary(dd2)
dd2$loadings
dd2$sdev
plot(dd2)
biplot(dd2)

library(graphics) #스크리 그래프
prin=princomp(x)
screeplot(prin, npcs=6, type="lines", main="scree plot")
prin=princomp(xx1) #a
screeplot(prin, npcs=6, type="lines", main="scree plot")
prin=princomp(xx2)  #b
screeplot(prin, npcs=6, type="lines", main="scree plot")

#군집분석 
#K-means clustering 과정
b11<-b1 #새로운 변수 설정
b11$group <-NULL
kmeans.result<-kmeans(b11,2)
kmeans.result
table(b1$group,kmeans.result$cluster) #실제 클러스터링 결과 점검위해 테이블 생성하고 비교
plot(b11[c("x1","x2","x3","x4","x5","x6")],col=kmeans.result$cluster,pch=15)

#인자분석
fact1=factanal(xx3, factors=2, rotation="none") #no rotation
fact1
fact2=factanal(xx3, factors=2, score="regression") #varimax
fact2
fact3=factanal(xx3, factors=2, rotation="promax") #promax
fact3

#인자분석용 스크리 그래프
library(graphics) #스크리 그래프
prin=princomp(xx3)
screeplot(prin, npcs=5, type="lines", main="scree plot")

namevar=names(fact2$loadings)=c("x2" ,"x3" ,"x4" ,"x5" ,"x6") 
plot(fact2$loadings[,1], fact2$loadings[,2], pch=16, xlab="factor1",
ylab="factor2", main="factor pattern")
text(x=fact2$loadings[,1], y=fact2$loadings[,2], labels=namevar, adj=0)
abline(v=0, h=0)

plot(fact2$scores[,1], fact2$scores[,2], pch="*", xlab="factor1", 
ylab="factor2", main="factor scores")













