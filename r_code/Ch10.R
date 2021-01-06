setwd("C:/Users/김서영/Desktop/문서/2020년2학기/사회과학자료/중2 패널 데이터(SPSS)")
### 데이터 불러들이기
load("spssdata.RData")

### 데이터 만들기- 33번 중 12번만 빼고
myvar <- c("q33a01w1", "q33a02w1", "q33a03w1", "q33a04w1", "q33a05w1",
           "q33a06w1", "q33a07w1", "q33a08w1", "q33a09w1", "q33a10w1"  )
##서울의 학생 589명만 따로 추출한것 PCA1로 이름 지정
PCA1   <- spssdata[myvar]
# names(spssdata)
# PCA1 <- spssdata[,c(9:18)]
PCA1_1 <- na.omit(PCA1) #결측값 제거

#################### 요인분석 #####################
### Factor의 수 결정 : 주성분분석(princomp)
fit1  <- princomp(PCA1_1, cor=TRUE)
summary(fit1)
plot(fit1, type="lines") #요인분석의 고유값 도표
#우리는 1보다 큰 것이 목ㅍ이므로 plot의
#variances가 1보다 큰게 2개 이므로 2개로 할 것
loadings(fit1) #요인들의 적재량-첫번째 요인은 각 요인의 평균임
#sum(loadings(fit1)[,1]^2) -> 첫번째 분산이 전체 분산에 대해 1이 나온다는 것을 알 수 있다.

### Factor에 따른 변수결정
######### OPTION 1 #########
fit <- factanal(PCA1_1, 2, rotation="varimax") 
#요인수 2개의 요인분석 결과를 varimax 방법을 사용하여 회전
print(fit, digits=3, sort=TRUE) #입력된 문항 순서대로 출력: sort=TRUE 옵션
#summary(fit)
load <- fit$loadings[,1:2]
plot(load) #도표출력
text(load, labels=names(PCA1_1), cex=0.7)

######### OPTION 2 ########
install.packages("psych")
library(psych)
fit1_1 <- principal(PCA1_1, nfactors=2, rotate="varimax")
fit1_1

#################### 신뢰도분석 ####################
# 데이터 쪼개기
myvar1a <- c("q33a01w1", "q33a02w1", "q33a03w1", "q33a04w1", "q33a05w1", "q33a06w1")
myvar1b <- c("q33a07w1", "q33a08w1", "q33a09w1", "q33a10w1")
# myvar1a <- myvar[1:6] ; myvar1b <- myvar[7:10]
PCA1a <- spssdata[myvar1a]
PCA1b <- spssdata[myvar1b]

###신뢰도분석 : 크롬바흐 알파
library(psych)
alpha(PCA1a, na.rm=TRUE)
alpha(PCA1b, na.rm=TRUE)

###########################################################################
# sjPlot 패키지를 이용한 요인분석과 신뢰도분석
#install.packages("sjPlot")
library(sjPlot)
tab_pca(PCA1, title="부모에 대한 애착", wrap.labels=20, show.cronb=TRUE,
        show.var=TRUE, string.pov="분산비율", string.cpov="누적 비율")


##############################
## 새로운 변수 생성
attach(spssdata)
spssdata$gattach <- q33a01w1+q33a02w1+q33a03w1+q33a04w1+q33a05w1+q33a06w1
spssdata$outatt  <- q33a07w1+q33a08w1+q33a09w1+q33a10w1
detach(spssdata)



###########################################################################
# sjPlot 패키지를 이용한 요인분석과 신뢰도분석
library(Hmisc)
test <- spss.get("C:/Users/김서영/Desktop/문서/2020년2학기/사회과학자료/중2 패널 데이터(SPSS)/04-1 중2 패널 1차년도 데이터(SPSS).sav",
                 use.value.labels=FALSE)
newdata <- subset(test, scharew1 >= 100 & scharew1 < 200, 
                  select=c("q48a01w1","q48a02w1","q48a03w1","q48a04w1","q48a05w1","q48a06w1",
                           "q48a07w1","q48a08w1","q48a09w1","q48a10w1","q48a11w1","q48a12w1"))
library(sjPlot)
tab_pca(newdata, title="자기만족도", wrap.labels=20, show.cronb=TRUE,
        show.var=TRUE, string.pov="분산비율", string.cpov="누적 비율")

############################################################################
#################### 신뢰도분석 ####################
# 데이터 쪼개기
myvar1a <- c("q48a01w1", "q48a02w1", "q48a03w1")
myvar1b <- c("q48a04w1", "q48a05w1", "q48a06w1")

PCA1a <- newdata[myvar1a]
PCA1b <- newdata[myvar1b]

###신뢰도분석 : 크롬바흐 알파
library(psych)
alpha(PCA1a, na.rm=TRUE)
alpha(PCA1b, na.rm=TRUE)

attach(newdata)
newdata$rq48a04w1[q48a04w1==1] <- 5
newdata$rq48a04w1[q48a04w1==2] <- 4
newdata$rq48a04w1[q48a04w1==3] <- 3
newdata$rq48a04w1[q48a04w1==4] <- 2
newdata$rq48a04w1[q48a04w1==5] <- 1

newdata$rq48a05w1[q48a05w1==1] <- 5
newdata$rq48a05w1[q48a05w1==2] <- 4
newdata$rq48a05w1[q48a05w1==3] <- 3
newdata$rq48a05w1[q48a05w1==4] <- 2
newdata$rq48a05w1[q48a05w1==5] <- 1

newdata$rq48a06w1[q48a06w1==1] <- 5
newdata$rq48a06w1[q48a06w1==2] <- 4
newdata$rq48a06w1[q48a06w1==3] <- 3
newdata$rq48a06w1[q48a06w1==4] <- 2
newdata$rq48a06w1[q48a06w1==5] <- 1

myvar <- c("q48a01w1", "q48a02w1", "q48a03w1" , "rq48a04w1", "rq48a05w1", "rq48a06w1")
PCA1aa <- newdata[myvar]

alpha(PCA1aa, na.rm=TRUE)
