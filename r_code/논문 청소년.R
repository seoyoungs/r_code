setwd("C:/Users/김서영/Desktop/문서/2020년2학기/사회과학자료/중2 패널 데이터(SPSS)")
#install.packages("Hmisc")
library(Hmisc)
test<-spss.get("C:/Users/김서영/Desktop/문서/2020년2학기/사회과학자료/중2 패널 데이터(SPSS)/04-2 중2 패널 2차년도 데이터(SPSS).sav",
               use.value.labels=FALSE)
#names(test)
ah <-c("id","sexw2","scharew2","areuw2","q1a1w2","q1a2w2","q1a3w2","q1a4w2","q1a5w2","q1a6w2","q1a7w2", "q2w2","q10a05w2","q10a06w2","q10a07w2","q10a08w2","q10a09w2","q10a10w2","q10a11w2","q19b1w2","q19b2w2","q19b3w2","q19b4w2","q19b5w2","q24w2","q29w2","q34a1w2","q34a2w2","q34a3w2","q34a4w2","q34a5w2","q34a6w2","q35a4w2","q35a5w2","q35a6w2","q36b1w2","q36b2w2","q36b3w2","q36b4w2","q48a01w2","q48a02w2","q48a03w2","q48b1w2","q48b2w2","q48b3w2","q49a02w2","q49a03w2","q49a04w2","q49a12w2","q49a13w2","q49a14w2","q49a15w2","q50w2")

test1 <- test[ah]
#spssdata1 <- test1[which(test1$scharew2 >= 420 & test1$scharew2 < 487), ]
#spssdata2 <- test1[which(test1$scharew2 >= 410 & test1$scharew2 < 416),]
#spssdata<-rbind(spssdata1, spssdata2)

###########이거 나중에 하기 변수이름##########
#library(sjmisc)
#library(sjlabelled)
#test.lables<-spss.get("C:/Users/김서영/Desktop/문서/2020년2학기/사회과학자료/중2 패널 데이터(SPSS)/04-2 중2 패널 2차년도 데이터(SPSS).sav",use.value.labels=TRUE)

#변수 다 합치기
attach(test1)
test1$careerch = q1a1w2+q1a2w2+q1a3w2+q1a4w2+q1a5w2+q1a6w2+q1a7w2

#spssdata <- transform(spssdata, attachment=q33a01w1+q33a02w1+q33a03w1+q33a04w1+q33a05w1+q33a06w1)

test1$nowsi= q10a05w2+q10a06w2+q10a07w2+q10a08w2+q10a09w2+q10a10w2+q10a11w2

test1$score = q19b1w2+q19b2w2+q19b3w2+q19b4w2+q19b5w2

test1$mind = q34a1w2+q34a2w2+q34a3w2+q34a4w2+q34a5w2+q34a6w2

test1$schlife = q35a4w2+q35a5w2+q35a6w2

test1$friend=q36b1w2+q36b2w2+q36b3w2+q36b4w2

test1$tru= q48b1w2+q48b2w2+q48b3w2

test1$paren=q49a02w2+q49a03w2+q49a04w2

test1$fri2=q49a12w2+q49a13w2+q49a14w2+q49a15w2
detach(test1)

save(test1, file="shsans.RData")

##############################################
#주성분 & 요인분석
load("shsans.RData")


#################### 요인분석 #####################


#####요인분석1-직업선택
myvar1 <- c("q1a1w2","q1a2w2","q1a3w2","q1a4w2","q1a5w2","q1a6w2","q1a7w2")
PCA1   <- test1[myvar1]
PCA1_1 <- na.omit(PCA1)
#요인분석
fit1  <- princomp(PCA1_1, cor=TRUE)
summary(fit1) #주성분분석 처음에 0.5 이상인지 아니면 두번째 인지
loadings(fit1) #이걸보고 2갈래 어디서 나눠지는지 안다.
myvar1a <- c("q1a1w2","q1a2w2")
myvar1b <- c("q1a3w2","q1a4w2","q1a5w2","q1a6w2","q1a7w2")
PCA1a <- test1[myvar1a]
PCA1b <- test1[myvar1b]
#신뢰도분석 : 크롬바흐 알파
library(psych)
alpha(PCA1a, na.rm=TRUE)
alpha(PCA1b, na.rm=TRUE)


#####요인분석2-학교 생활 적응
myvar2 <- c("q10a05w2","q10a06w2","q10a07w2","q10a08w2","q10a09w2","q10a10w2","q10a11w2")
PCA2   <- test1[myvar2]
PCA2_1 <- na.omit(PCA2)
#요인분석
fit2  <- princomp(PCA2_1, cor=TRUE)
summary(fit2) #주성분분석 처음에 0.5 이상인지 아니면 두번째 인지
loadings(fit2) #이걸보고 2갈래 어디서 나눠지는지 안다.
myvar2a <- c("q10a05w2","q10a06w2","q10a07w2")
myvar2b <- c("q10a08w2","q10a09w2","q10a10w2","q10a11w2")
PCA2a <- test1[myvar2a]
PCA2b <- test1[myvar2b]
#신뢰도분석 : 크롬바흐 알파
#library(psych)
alpha(PCA2a, na.rm=TRUE)
alpha(PCA2b, na.rm=TRUE)


######요인분석-학교 수업의 성적향상 효과여부
myvar3 <- c("q19b1w2","q19b2w2","q19b3w2","q19b4w2","q19b5w2")
PCA3   <- test1[myvar3]
PCA3_1 <- na.omit(PCA3)
#요인분석
fit3  <- princomp(PCA3_1, cor=TRUE)
summary(fit3) #주성분분석 처음에 0.5 이상인지 아니면 두번째 인지
loadings(fit3) #이걸보고 2갈래 어디서 나눠지는지 안다.
#신뢰도분석 : 크롬바흐 알파
#library(psych)
alpha(PCA3_1 , na.rm=TRUE)


######요인분석-나의 성격유형
myvar4 <- c("q34a1w2","q34a2w2","q34a3w2","q34a4w2","q34a5w2","q34a6w2")
PCA4   <- test1[myvar4]
PCA4_1 <- na.omit(PCA4)
#요인분석
fit4  <- princomp(PCA4_1, cor=TRUE)
summary(fit4) #주성분분석 처음에 0.5 이상인지 아니면 두번째 인지
loadings(fit4) #이걸보고 2갈래 어디서 나눠지는지 안다.
myvar4a <- c("q34a1w2","q34a2w2")
myvar4b <- c("q34a3w2","q34a4w2","q34a5w2","q34a6w2")
PCA4a <- test1[myvar4a]
PCA4b <- test1[myvar4b]
#신뢰도분석 : 크롬바흐 알파
#library(psych)
alpha(PCA4a, na.rm=TRUE)
alpha(PCA4b, na.rm=TRUE)


#####요인분석-학교 선생님과의 관계
myvar5 <- c("q35a4w2","q35a5w2","q35a6w2")
PCA5   <- test1[myvar5]
PCA5_1 <- na.omit(PCA5)
#요인분석
fit5  <- princomp(PCA5_1, cor=TRUE)
summary(fit5) #주성분분석 처음에 0.5 이상인지 아니면 두번째 인지
loadings(fit5) 
#신뢰도분석 : 크롬바흐 알파
#library(psych)
alpha(PCA5_1, na.rm=TRUE)


#####요인분석-매우 친한 친구와의 관계
myvar6 <- c("q35a6w2","q36b1w2","q36b2w2","q36b3w2","q36b4w2")
PCA6   <- test1[myvar6]
PCA6_1 <- na.omit(PCA6)
#요인분석
fit6  <- princomp(PCA6_1, cor=TRUE)
summary(fit6) #주성분분석 처음에 0.5 이상인지 아니면 두번째 인지
loadings(fit6)
#신뢰도분석 : 크롬바흐 알파
#library(psych)
alpha(PCA6_1, na.rm=TRUE)


#####요인분석-자신에 대한 신뢰도
myvar7 <- c("q48b1w2","q48b2w2","q48b3w2")
PCA7   <- test1[myvar7]
PCA7_1 <- na.omit(PCA7)
#요인분석
fit7  <- princomp(PCA7_1, cor=TRUE)
summary(fit7) #주성분분석 처음에 0.5 이상인지 아니면 두번째 인지
loadings(fit7)
#신뢰도분석 : 크롬바흐 알파
#library(psych)
alpha(PCA7_1, na.rm=TRUE)


#####요인분석-부모님과의 갈등으로 겪는 스트레스
myvar8 <- c("q49a02w2","q49a03w2","q49a04w2")
PCA8   <- test1[myvar8]
PCA8_1 <- na.omit(PCA8)
#요인분석
fit8  <- princomp(PCA8_1, cor=TRUE)
summary(fit8) #주성분분석 처음에 0.5 이상인지 아니면 두번째 인지
loadings(fit8)
#신뢰도분석 : 크롬바흐 알파
#library(psych)
alpha(PCA8_1, na.rm=TRUE)


#####요인분석 - 외모에 대한 불만
myvar9 <- c("q49a12w2","q49a13w2","q49a14w2","q49a15w2")
PCA9   <- test1[myvar9]
PCA9_1 <- na.omit(PCA9)
#요인분석
fit9  <- princomp(PCA9_1, cor=TRUE)
summary(fit9) #주성분분석 처음에 0.5 이상인지 아니면 두번째 인지
loadings(fit9)
#신뢰도분석 : 크롬바흐 알파
#library(psych)
alpha(PCA9_1, na.rm=TRUE)


#####반응변수 만족도 요인분석과 신뢰도 분석
myvar9 <- c("q50w2","q29w2","q48a01w2","q48a02w2","q48a03w2")
PCA9   <- test1[myvar9]
PCA9_1 <- na.omit(PCA9)
#요인분석
fit9  <- princomp(PCA9_1, cor=TRUE)
summary(fit9) #주성분분석 처음에 0.5 이상인지 아니면 두번째 인지
loadings(fit9)
#신뢰도분석 : 크롬바흐 알파
library(psych)
alpha(PCA9_1, na.rm=TRUE)

