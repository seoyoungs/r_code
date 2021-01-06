setwd("C:/Users/김서영/Desktop/문서/2020년2학기/사회과학자료/중2 패널 데이터(SPSS)")
### 데이터 불러들이기
load("spssdata.RData")
attach(spssdata)

#####################################################################
### 독립표본 T검증의 분석 방법 :
## 연구가설 : 성별에 따라 부모에 대한 애착의 정도에 차이가 있을 것이다.
##
## 1. 부모에 대한 애착변수 만들기
spssdata$attachment <- q33a01w1+q33a02w1+q33a03w1+q33a04w1+q33a05w1+q33a06w1
#spssdata$attachment.ave <- (q33a01w1+q33a02w1+q33a03w1+q33a04w1+q33a05w1+q33a06w1)/6

# 변수 설명 입력
library(sjlabelled)
spssdata$attachment <- set_label(spssdata$attachment, "부모에 대한 애착")

#문자형 변수로 변환
spssdata$sexw1a <- factor(spssdata$sexw1, levels=c(1,2), labels=c("남자","여자"))

## 2. 분산 동질성 검정
var.test(attachment ~ sexw1, data=spssdata)
var.test(attachment ~ sexw1a, data=spssdata)

## 3. 연구가설 검증
t.test(attachment ~ sexw1, var.equal=TRUE, data=spssdata)
t.test(attachment ~ sexw1a, var.equal=TRUE, data=spssdata)

## 4. 집단에 따른 기술 통계량
library(psych)
describeBy(spssdata$attachment, group=spssdata$sexw1a)

## 5. 집단에 따른 평균 비교 도표
#install.packages("gplots")
library(gplots)
plotmeans(attachment ~ sexw1a, data=spssdata, xlab="성별", ylab="부모에 대한 애착",
          ci.label=TRUE, mean.label=TRUE, ylim=c(18,22), barwidth=5, 
          main="성별에 따른 부모에 대한 애착 수준", digits=3, pch="*")

#####################################################################
### 일표본 T검증의 분석 방법 :
## 연구가설 :자기통제력의 평균은 다른 연구에서의 자기통제력의 평균인 
##          18.5와 차이가 있을 것이다.
##
## 1. 분석에 사용되는 변수 만들기 : 

###########################################################################
# sjPlot 패키지를 이용한 요인분석과 신뢰도분석
self <- spssdata[c("q34a1w1","q34a2w1","q34a3w1","q34a4w1","q34a5w1","q34a6w1")]
library(sjPlot)
tab_pca(self, title="자기통제력", wrap.labels=20, show.cronb=TRUE,
        show.var=TRUE, string.pov="분산비율", string.cpov="누적 비율")
library(psych)
alpha(self, na.rm=TRUE)

##  점수가 높을수록 자기통제력이 높은 방향으로 역코딩 
spssdata$rq34a1w1[q34a1w1==1] <- 5
spssdata$rq34a1w1[q34a1w1==2] <- 4
spssdata$rq34a1w1[q34a1w1==3] <- 3
spssdata$rq34a1w1[q34a1w1==4] <- 2
spssdata$rq34a1w1[q34a1w1==5] <- 1

spssdata$rq34a2w1[q34a2w1==1] <- 5
spssdata$rq34a2w1[q34a2w1==2] <- 4
spssdata$rq34a2w1[q34a2w1==3] <- 3
spssdata$rq34a2w1[q34a2w1==4] <- 2
spssdata$rq34a2w1[q34a2w1==5] <- 1

spssdata$rq34a3w1[q34a3w1==1] <- 5
spssdata$rq34a3w1[q34a3w1==2] <- 4
spssdata$rq34a3w1[q34a3w1==3] <- 3
spssdata$rq34a3w1[q34a3w1==4] <- 2
spssdata$rq34a3w1[q34a3w1==5] <- 1

spssdata$rq34a4w1[q34a4w1==1] <- 5
spssdata$rq34a4w1[q34a4w1==2] <- 4
spssdata$rq34a4w1[q34a4w1==3] <- 3
spssdata$rq34a4w1[q34a4w1==4] <- 2
spssdata$rq34a4w1[q34a4w1==5] <- 1

spssdata$rq34a5w1[q34a5w1==1] <- 5
spssdata$rq34a5w1[q34a5w1==2] <- 4
spssdata$rq34a5w1[q34a5w1==3] <- 3
spssdata$rq34a5w1[q34a5w1==4] <- 2
spssdata$rq34a5w1[q34a5w1==5] <- 1

spssdata$rq34a6w1[q34a6w1==1] <- 5
spssdata$rq34a6w1[q34a6w1==2] <- 4
spssdata$rq34a6w1[q34a6w1==3] <- 3
spssdata$rq34a6w1[q34a6w1==4] <- 2
spssdata$rq34a6w1[q34a6w1==5] <- 1


## 2. 자기통제 변수를 만들기 
#attach(spssdata)
spssdata$self.control <- rq34a1w1+rq34a2w1+rq34a3w1+rq34a4w1+rq34a5w1+rq34a6w1
detach(spssdata)

# 변수 설명 입력
library(sjlabelled)
spssdata$self.control <- set_label(spssdata$self.control, "자기통제력")

## 3. 일표본 T 검증
t.test(spssdata$self.control, mu=18.5)


#####################################################################
### 짝지어진 T검증의 분석 방법 :
## 연구가설 : 1차년도와 2차년도의 자아존중감은 차이가 있을 것이다.
##
###########################################################################
# sjPlot 패키지를 이용한 요인분석과 신뢰도분석
library(Hmisc)
test<-spss.get("C:/Users/김서영/Desktop/문서/2020년2학기/사회과학자료/중2 패널 데이터(SPSS)/04-1 중2 패널 1차년도 데이터(SPSS).sav",
               use.value.labels=FALSE)
newdata <- subset(test, scharew1 >= 100 & scharew1 < 200, 
                  select=c("q48a01w1","q48a02w1","q48a03w1","q48a04w1","q48a05w1","q48a06w1",
                           "q48a07w1","q48a08w1","q48a09w1","q48a10w1","q48a11w1","q48a12w1"))
library(sjPlot)
tab_pca(newdata, title="자기만족도", wrap.labels=20, show.cronb=TRUE,
        show.var=TRUE, string.pov="분산비율", string.cpov="누적 비율")
############################################################################

## 1. 2차년도 데이터 불러오기 : 
library(Hmisc)
second<-spss.get("C:/Users/김서영/Desktop/문서/2020년2학기/사회과학자료/중2 패널 데이터(SPSS)/04-2 중2 패널 2차년도 데이터(SPSS).sav",
               use.value.labels=FALSE)
# 데이터 합치기
mergedata <- merge(spssdata, second, by="id")

## 2. 역부호화된 변수 만들기
attach(mergedata)
#1차년도 변수
mergedata$rq48a04w1[q48a04w1==1] <- 5
mergedata$rq48a04w1[q48a04w1==2] <- 4
mergedata$rq48a04w1[q48a04w1==3] <- 3
mergedata$rq48a04w1[q48a04w1==4] <- 2
mergedata$rq48a04w1[q48a04w1==5] <- 1

mergedata$rq48a05w1[q48a05w1==1] <- 5
mergedata$rq48a05w1[q48a05w1==2] <- 4
mergedata$rq48a05w1[q48a05w1==3] <- 3
mergedata$rq48a05w1[q48a05w1==4] <- 2
mergedata$rq48a05w1[q48a05w1==5] <- 1

mergedata$rq48a06w1[q48a06w1==1] <- 5
mergedata$rq48a06w1[q48a06w1==2] <- 4
mergedata$rq48a06w1[q48a06w1==3] <- 3
mergedata$rq48a06w1[q48a06w1==4] <- 2
mergedata$rq48a06w1[q48a06w1==5] <- 1
#2차년도 변수
mergedata$rq48a04w2[q48a04w2==1] <- 5
mergedata$rq48a04w2[q48a04w2==2] <- 4
mergedata$rq48a04w2[q48a04w2==3] <- 3
mergedata$rq48a04w2[q48a04w2==4] <- 2
mergedata$rq48a04w2[q48a04w2==5] <- 1

mergedata$rq48a05w2[q48a05w2==1] <- 5
mergedata$rq48a05w2[q48a05w2==2] <- 4
mergedata$rq48a05w2[q48a05w2==3] <- 3
mergedata$rq48a05w2[q48a05w2==4] <- 2
mergedata$rq48a05w2[q48a05w2==5] <- 1

mergedata$rq48a06w2[q48a06w2==1] <- 5
mergedata$rq48a06w2[q48a06w2==2] <- 4
mergedata$rq48a06w2[q48a06w2==3] <- 3
mergedata$rq48a06w2[q48a06w2==4] <- 2
mergedata$rq48a06w2[q48a06w2==5] <- 1
detach(mergedata)

## 3. 변수만들기
attach(mergedata)
mergedata$self.esteemw1 <- q48a01w1+q48a02w1+q48a03w1+rq48a04w1+rq48a05w1+rq48a06w1
mergedata$self.esteemw2 <- q48a01w2+q48a02w2+q48a03w2+rq48a04w2+rq48a05w2+rq48a06w2
detach(mergedata)

## 4. 짝지어진 T 검증
t.test(mergedata$self.esteemw1, mergedata$self.esteemw2, paired=TRUE)

## 5. 기술통계량 구하기
#기술통계량을 구할 대상 변수를 선택하여 데이터로 만들기
compare_self.esteem <- c("self.esteemw1", "self.esteemw2")
describe_self.esteem <- mergedata[compare_self.esteem]
#기술통계량
library(psych)
describe(describe_self.esteem)

## 6. 결측값 사례를 제외한 기술통계량을 출력하기위한 과정 
describe_self.esteem$none_self.esteemw1 <- describe_self.esteem$self.esteemw1
describe_self.esteem$none_self.esteemw1[is.na(describe_self.esteem$
              self.esteemw1)|is.na(describe_self.esteem$self.esteemw2)] <- NA

describe_self.esteem$none_self.esteemw2 <- describe_self.esteem$self.esteemw2
describe_self.esteem$none_self.esteemw2[is.na(describe_self.esteem$self.esteemw1)|
                                          is.na(describe_self.esteem$self.esteemw2)] <- NA

describe(describe_self.esteem)
summary(describe_self.esteem)

save(spssdata, file="spssdata.RData")
