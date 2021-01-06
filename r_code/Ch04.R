setwd("C:/Users/김서영/Desktop/문서/2020년2학기/사회과학자료/중2 패널 데이터(SPSS)")
### 데이터 불러들이기
load("spssdata.RData")
attach(spssdata)

table(q50w1) #관측도수가 적은편(1,5가 그래서 다른것과 합칠예정-순서형범주)
#####################################################################
#######교차분석
### table 함수 이용--->1,2,3,4,5번 새로운 변수로 정의
spssdata$satisfaction[q50w1==1|q50w1==2] <- 1 #만족하지 못하는 편이다
spssdata$satisfaction[q50w1==3] <-2           #보통
spssdata$satisfaction[q50w1==4|q50w1==5] <- 3 #만족하는 편

library(sjlabelled)
table(spssdata$satisfaction) #새롭게 변수완성
spssdata$satisfaction <- set_labels(spssdata$satisfaction, 
                                     labels=c("만족하지 못하는 편", "보통", "만족하는 편"))
#교차표
devi02_1.re <- table(spssdata$satisfaction, spssdata$sexw1)
devi02_1.re #성별에 따른 만족도

#변수값 설명 교차표
attach(spssdata)
spssdata$satisfaction_1[satisfaction==1] <- "1_만족하지 못하는 편"
spssdata$satisfaction_1[satisfaction==2] <- "2_보통"
spssdata$satisfaction_1[satisfaction==3] <- "3_만족하는 편"
table(spssdata$satisfaction_1)
spssdata$sexw1_1[sexw1==1] <- "1_남자"
spssdata$sexw1_1[sexw1==2] <- "2_여자"
detach(spssdata)

devi02.re <- table(spssdata$satisfaction_1, spssdata$sexw1_1)
devi02.re

#주변합
margin.table(devi02.re) # 전체합계
margin.table(devi02.re, 1) #satisfaction_1 변수의 주변합계
margin.table(devi02.re, 2) #sexw1_1 변수의 주변합계

#백분율
round(prop.table(devi02.re)*100, 2)  #셀의 비율 산출
round(prop.table(devi02.re, 2)*100, 2)   #열의 비율 산출

## 성별과 생활만족도의 독립성 검정
# H0 : 성별에 따라 전반적인 생활만족도는 차이가 없다. (성별과 생활만족도는 독립니다.)
# H1 : 성별에 따라 전반적인 생활만족도는 차이가 있다. (성별과 생활만족도는 독립이 아니다.)
chisq.test(devi02.re, correct=FALSE)
# p-값이 1.428*10^(-5) <0.05 귀무가설 기각한다. 
# 즉 유의수준 0.05에서 성별에 따른 생활만족도의 분포는 차이가 있다고 할 수 있다. 


### gmodels 패키지
#install.packages("gmodels")
library(gmodels)
CrossTable(spssdata$satisfaction_1, spssdata$sexw1_1, digits=2,
           prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE,
           chisq=TRUE)

table(areaw1) #범주형 범수 연관성분석

### sjplot 패키지
library(sjlabelled)
spssdata$sexw1<- set_labels(spssdata$sexw1, 
                                    labels=c("납자", "여자"))

library(sjPlot)
tab_xtab(spssdata$satisfaction, spssdata$sexw1, show.col.prc=TRUE, 
         var.labels=c("전반적 생활만족도", "성별"), encoding="UTF-8")

#난 view가 안보이므로 꼭 밑에 옵션 사용
tab_xtab(spssdata$satisfaction, spssdata$sexw1, show.col.prc=TRUE, 
         var.labels=c("전반적 생활만족도", "성별"), encoding="EUC-KR",
         file="satissex.html") #working directory에 저장

#도표출력
set_theme(geom.label.size=4.5, axis.textsize=1.1, legend.pos="bottom")
plot_xtab(spssdata$satisfaction, spssdata$sexw1, type="bar", y.offset=0.01,
          margin="col", coord.flip=TRUE, wrap.labels=7, geom.colors="Set2",
          show.summary=TRUE)



