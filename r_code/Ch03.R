setwd("C:/Users/김서영/Desktop/문서/2020년2학기/사회과학자료/중2 패널 데이터(SPSS)")
### 데이터 불러들이기
load("spssdata.RData")
attach(spssdata)

#####################################################################
##빈도표 출력: 삶의 만족도
table(q50w1) 
library(plyr) #빈도표 위한 함수
#names(spssdata)
count(spssdata, "q50w1")

#cbind 분석결과 묶는 함수(열), 행은 rbind
cbind(Freq=table(q50w1),           #빈도
      Cumul=cumsum(table(q50w1)),  #누적빈도
      Relative=prop.table(table(q50w1)), #빈도별 비율
      Cum.prop = cumsum(prop.table(table(q50w1)))) #누적비율
###########cbind 줄인것
tab = table(q50w1)
cbind(Freq=tab, Cumul=cumsum(tab), Relative=prop.table(tab), Cum.prop=cumsum(prop.table(tab)))
###########빈도별 퍼센트로 바꾸기 소수점 3자리까지
count(spssdata, "scharew1") #코드북보고 앞에 번호 어디인지 보기
cbind(Freq=table(q50w1),
      percentage=100*prop.table(table(q50w1)),
      relative=round(100*prop.table(table(q50w1)),3))

#####또다른 빈도표출력함수 describe: 이게 젤 좋다(결측치도)######
library(Hmisc)
describe(q50w1)
#####
# summarytools 패키지를 이용한 빈도표 출력
##
#install.packages("summarytools")
library(summarytools)
view(freq(q50w1))
freq(q50w1, plain.ascii=FALSE, style="rmarkdown")

# 설명: freq(빈도)의 % Valid(결측값 뺀 비율),  % Total(결측값 포함 비율)


#####################################################################
##기술통계량 출력 
summary(q50w1)
#여기서 1st Qu 3.000 은 1사분위수: 3보다 작게 대답한인원수가 전체 595명중 25%라는 말
str(q50w1)
library(psych)
describe(q50w1) #앞에서 한 describe랑은 다르다
# trimmed 절사평균
#mad: sd떨어진 정도 skew왜도, se 표준오차, range=최대-최소


var1 <- c("q33a01w1", "q50w1")
tab1 <- spssdata[var1]
library(summarytools)
freq(tab1)
#위두개 범주형 취급 함수
library(psych)
describe(tab1)
#위 두개 연속형 취급함수

library(sjmisc)
descr(tab1, encoding="EUC-KR", out="viewer")

#여기까지 표 만들기
#####################################################################
##여러 형태의 도표그리기 
lab.val <- c("전혀 만족하지 못한다", "만족하지 못하는 편이다", "보통이다", 
             "만족하는 편이다", "매우 만족한다")
# 바 도표
par(mfrow=c(2,1))
barplot(table(q50w1),  #빈도
        names.arg=lab.val,
        space=1.5, border=NA, cex.names=0.7, ylim=c(0, 350))

barplot(prop.table(table(q50w1)), #비율
        names.arg=lab.val,
        space=1.5, border=NA, cex.names=0.7, ylim=c(0, 1))

par(mfrow=c(1,1))

library(RColorBrewer) #색깔
display.brewer.all()
display.brewer.pal(5, "Set2")
pal1 <- brewer.pal(5, "Set2")
barplot(prop.table(table(q50w1)), col=pal1, 
        names.arg=lab.val,
        space=1.5, border=NA, cex.names=0.7, ylim=c(0, 1))

library(sjPlot)
plot_frq(q50w1)
#옵션 조정하기 (굳이 안해도 됨, geom.size 바 사이즈조정)
plot_frq(q50w1, geom.size=0.3, ylim=c(0,350), geom.colors="grey47", title="",) 

tab.val <- factor(q50w1, levels=c(1:5), labels=lab.val)
plot_frq(tab.val, axis.title="삶의 만족도", geom.colors="red", geom.size=0.5)

# 히스토그램 교과서112p
#애착변수 생성
spssdata$attachment <- q33a01w1 + q33a02w1 + q33a03w1 + q33a04w1 + q33a05w1 + q33a06w1
#attach(spssdata)

library(psych)
describe(attachment)
#des에서 나온 평균 높은편
par(mfrow=c(1,2))
hist(attachment)#어느정도 대칭성만족
library(RColorBrewer) #색깔
pal1 <- brewer.pal(7, "Set2")
hist(attachment, breaks=20, col=pal1) # breaks=20:범위를 20개로 잘라라
par(mfrow=c(1,1))

par(mfrow=c(1,2))
plot(density(attachment, na.rm=TRUE))
d <- density(attachment, na.rm=TRUE)
plot(d)
polygon(d, col="red", border="blue") #히스토그램 선으로 표현한것
par(mfrow=c(1,1))

####
hist(attachment, color="blue", border="black", prob = TRUE, main="부모에 대한 애착")
lines(density(attachment, na.rm=TRUE), lwd = 2, col="red")
####

# 파이 도표-이건 범주형범수(보고서는 안씀)
tab <- table(q50w1)
pie(tab, labels=lab.val)

pct <- round(100*prop.table(tab), 1)
lbls <- paste(lab.val, pct)
lbls <- paste(lbls, "%")
pie(pct, labels=lbls, col=rainbow(5),
    main="50. 학생은 학생의 삶에 전반적으로 얼마나 만족하고 있습니까?",
    init.angle=180, radius=1.0)

#3D 파이도표
install.packages("plotrix")
library(plotrix)
pie3D(pct, labels = lbls, col=rainbow(5),
      main="50. 학생은 학생의 삶에 전반적으로 얼마나 만족하고 있습니까?",
      labelcex=1.1, explode = 0.1, theta=1.1, shade=0.3)

library(RColorBrewer) #색깔
pal1 <- brewer.pal(5, "Set2")
pie3D(pct, labels = lbls, col=pal1,
      main="50. 학생은 학생의 삶에 전반적으로 얼마나 만족하고 있습니까?",
      labelcex=1.1, explode = 0.1, theta=1.1, shade=0.3)