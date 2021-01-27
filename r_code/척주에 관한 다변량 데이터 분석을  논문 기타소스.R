b1=read.csv(file="C:/Users/김서영/Documents/r프로그램/vertebral2.csv", header=TRUE)
attach(b1)
x=b1[,3:8]
z<-scale(x); z
head(z)
boxplot(x, col=rainbow(6))
boxplot(z, col=rainbow(6))
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

par(mfrow=c(1,6), col=c(2)) #boxplot
plot(x1~group); plot(x2~group);plot(x3~group);
plot(x4~group);plot(x5~group);plot(x6~group)

plot(x1~group, col="red")
plot(x2~group, col="blue")
plot(x3~group, col="green")
plot(x4~group, col="yellow")
plot(x5~group, col="purple")
plot(x6~group, col="orange")
#체르노프얼굴
install.packages("aplpack")
library(aplpack)
train2 <- sample(2:310, 50)
train2
b1_1 <- b1[train2,c("x1", "x2", "x3", "x4", "x5", "x6")] 
faces(b1_1, face.type = 1,  main = "Chernoff faces: face.type = 1")
#별그림
stars(b1_1)

#변수끼리의 산점도
splom(x)

S=cov(x)
S
R=cor(z)
R
eigen(R) #고유값
R1=cor(xx1) #그룹 a(abnormal) 표본공분산행렬
R1
R2=cor(xx2) #그룹 b(normal) 표본공분산행렬
R2

par(mfrow=c(2,2))
q1<-NULL
plot(q1, x1, pch=16, xlim=c(-3,3)

d2=princomp(z, cor=TRUE) #상관행렬을 이용해 주성분분석
summary(d2)

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

#기술통계량
summary(x)
summary(xx1)
summary(xx2)


install.packages("corrplot")
library(corrplot)
corrplot(R)


library(graphics) #스크리 그래프
prin=princomp(x)
screeplot(prin, npcs=6, type="lines", main="scree plot")
prin=princomp(xx1) #a
screeplot(prin, npcs=6, type="lines", main="scree plot")
prin=princomp(xx2)  #b
screeplot(prin, npcs=6, type="lines", main="scree plot")

#선형판별분석
colnames(x)   
levels(b1$group)
library(MASS)
xx3=b1[,4:8] #x1이 모든 변수와 공선형관계 있다 따라서 x1 제거후 돌리기
#각 변수의 그룹별 분포에 대한 히스토그램과 확률밀도함수 추정선
ldahist(data=x1, g=group, type="both")
ldahist(data=x2, g=group, type="both")
ldahist(data=x3, g=group, type="both")
ldahist(data=x4, g=group, type="both")
ldahist(data=x5, g=group, type="both")
ldahist(data=x6, g=group, type="both")
ld <- lda(group ~ x2+x3+x4+x5+x6, data = b1)
ld
pc<-predict(ld,b1)$class
pc<-as.numeric(pc)
pc[(pc==1)]="a"
pc[(pc==2)]="b"
pc
n=dim(b1)[[1]]
res=cbind(group, pc)
correct=res[(group==pc),] #match
correct.rate=dim(correct)[[1]]/n
correct.rate
error.rate=1-correct.rate
error.rate
library(klaR)

#partition 그래프 ab가 어떤 변수에서 잘 나눠져 있는지 볼 수 있다.
partimat(group ~ x2+x3+x4+x5+x6, data =b1, method="lda") 

#이차판별함수(오분류율 구하기)
qd=qda(xx3, group)
qd
qc<-predict(qd)$class
qc<-as.numeric(qc)
qc
qc[(qc==1)]="a"
qc[(qc==2)]="b"
qc
resq=cbind(group,qc)
correctq=resq[(group==qc),] #match
correctq.rate=dim(correctq)[[1]]/n
correctq.rate
errorq.rate=1-correctq.rate
errorq.rate

summary(b1)

#단순회귀분석
glm.fit = glm(group~x1, data=group, family = binomial)
summary(glm.fit)
glm.fit = glm(group~x2, data=group, family = binomial)
summary(glm.fit)
glm.fit = glm(group~x3, data=group, family = binomial)
summary(glm.fit)
glm.fit = glm(group~x4, data=group, family = binomial)
summary(glm.fit)
glm.fit = glm(group~x5, data=group, family = binomial)
summary(glm.fit)
glm.fit = glm(group~x6, data=group, family = binomial)
summary(glm.fit)

#군집분석 
#K-means clustering 과정
k1 <- kmeans(scale(b1[,3:8]), centers = 8, iter.max = 1000) #군집 2개
k1$betweenss/k1$totss 
c(k1,k=8)
k1$cluster
table(b1[,8]

b11<-b1 #새로운 변수 설정
b11$group <-NULL #그룹에 대한 사정정보 없다고 가정하려고 group을 없앰
kmeans.result<-kmeans(b11,8) #군집 8개로 군집분석
summary(kmeans.result)
table(b1$group,kmeans.result$cluster) #실제 클러스터링 결과 점검위해 테이블 생성하고 비교
plot(b11[c("x1","x2","x3","x4","x5","x6")],col=kmeans.result$cluster)

rownames(b1) <- paste0(b1$group,"_",rownames(b1)) #행이름 변경
train <- sample(1:dim(b1)[1], 300) #무작위로 50개 추출 
dist_b1 <- dist(scale(z[train, -6]), method = "euclidean") #거리 계산
hc1 <- hclust(dist_b1,  method = "single") # 최단연결법(1)
hc2 <- hclust(dist_b1,  method = "complete") #최장연결법
hc3 <- hclust(dist_b1, method = "ward.D") #ward
hc4 <- hclust(dist_b1,  method = "average") #평균연결법
summary(b1.hclust)
plot(hc1)
plot(hc2)
plot(hc3)
plot(hc4)


#인자분석
xx3=b1[,4:8] #x1이 모든 변수와 공선형관계 있다 따라서 x1 제거후 돌리기
en <- eigen(cor(xx3))
names(en)
en$values  #고유값(x2,x3,x4,x5,x6)
mean(en$values) #고유값 전체 평균


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


#로지스틱 회귀분석
mod3 <- glm(as.integer(group) ~., data= b1)
summary(mod3)
b1_1<-glm(z, data=b1, family='binomial')
a <- subset(b1, group == "a" | group == "b") 
a$group <- factor(a$group) 
str(a) 
d1 <- glm(group ~ x1, data=a, family=binomial)
summary(d1)
cdplot(group ~ x1, data=a)
d2 <- glm(group ~ x2, data=a, family=binomial)
summary(d2)
cdplot(group ~ x2, data=a)
d3 <- glm(group ~ x3, data=a, family=binomial)
summary(d3)
cdplot(group ~ x3, data=a)
d4 <- glm(group ~ x4, data=a, family=binomial)
summary(d4)
cdplot(group ~ x4, data=a)
d5 <- glm(group ~ x5, data=a, family=binomial)
summary(d5)
cdplot(group ~ x5, data=a)
#x6 glm.fit: 적합된 확률값들이 0 또는 1 에러

m1<-lm(x1~., data=b1)
summary(m1)
stats::anova(group)








