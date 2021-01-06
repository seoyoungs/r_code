setwd("C:/Users/김서영/Desktop/문서/2020년2학기/사회과학자료/중2 패널 데이터(SPSS)")
### 데이터 불러들이기
load("spssdata.RData")

##########################################################################
## 일원분산분석 : 
 # 학교 성적에 따른 집단가의 자아존중감 수준은 다를 것이다.
##########################################################################
attach(spssdata)
############################
# 독립변수 
spssdata$grade <- q18a1w1 + q18a2w1 + q18a3w1
 # 학교성적 변수 grade 생성
attach(spssdata)

library(summarytools)
freq(grade, plain.ascii=FALSE,style="rmarkdown")
 # 학교성적 변수 grade의 분포

spssdata$grp.grade[grade >= min(grade) & grade <= 8] <- 1
spssdata$grp.grade[grade >= 9 & grade <= 10] <- 2
spssdata$grp.grade[grade >= 11 & grade <= max(grade)] <- 3
 # 학교성적 변수를 3집단으로 분류

#library(sjlabelled)
spssdata$grp.grade <- set_label(spssdata$grp.grade, "학교성적")
 # 변수설명
spssdata$grp.grade <- set_labels(spssdata$grp.grade, levels=c(1,2,3),
      labels=c("낮은 학교성적 집단", "중간 학교성적 집단", "높은 학교성적 집단"))
 # 변수값 설명

spssdata$grp.grade.factor <- factor(spssdata$grp.grade, levels=c(1,2,3),
      labels=c("낮은 학교성적 집단", "중간 학교성적 집단", "높은 학교성적 집단"))
 # 문자형 변수로 변환

############################
# 종속변수 
# 역방향 코딩
attach(spssdata)
spssdata$rq48a04w1[q48a04w1==1] <- 5
spssdata$rq48a04w1[q48a04w1==2] <- 4
spssdata$rq48a04w1[q48a04w1==3] <- 3
spssdata$rq48a04w1[q48a04w1==4] <- 2
spssdata$rq48a04w1[q48a04w1==5] <- 1

spssdata$rq48a05w1[q48a05w1==1] <- 5
spssdata$rq48a05w1[q48a05w1==2] <- 4
spssdata$rq48a05w1[q48a05w1==3] <- 3
spssdata$rq48a05w1[q48a05w1==4] <- 2
spssdata$rq48a05w1[q48a05w1==5] <- 1

spssdata$rq48a06w1[q48a06w1==1] <- 5
spssdata$rq48a06w1[q48a06w1==2] <- 4
spssdata$rq48a06w1[q48a06w1==3] <- 3
spssdata$rq48a06w1[q48a06w1==4] <- 2
spssdata$rq48a06w1[q48a06w1==5] <- 1

#attach(spssdata)
spssdata$self.esteem <- q48a01w1+q48a02w1+q48a03w1+rq48a04w1+rq48a05w1+rq48a06w1
 # 자아존중감 변수 
spssdata$self.esteem <- set_label(spssdata$self.esteem ,"자아존중감")
 # 변수설명

############################
# 일원분산분석 : 
# 학교 성적에 따른 집단가의 자아존중감 수준은 다를 것이다.
ano1 <- aov(self.esteem ~ grp.grade.factor, data=spssdata)
anova(ano1)
#ano <- aov(self.esteem ~ as.factor(grp.grade), data=spssdata)
#anova(ano)

# 집단별 평균값
#library(psych)
describeBy(spssdata$self.esteem, group=spssdata$grp.grade)

# sjstats 패키지를 이용한 분산분석 테이블
library(sjstats)
means_by_group(spssdata, dv=self.esteem, grp=grp.grade, encoding="EUC-KR", out="viewer")
 
# sjstats 패키지를 이용한 분산분석 그래프
#library(sjPlot)
set_theme(geom.label.size=4.5, axis.textsize=1.2)
sjp.aov1(spssdata$self.esteem, spssdata$grp.grade, geom.size=0.5, 
         wrap.labels=7, axis.lim=c(17.5,22), meansums = TRUE, show.summary = TRUE)

# gplots 패키지를 이용한 기술통계량 비교 도표
library(gplots)
plotmeans(self.esteem~grp.grade.factor, data=spssdata, xlab="학교성적",
          ylab="자아존중감", ci.label=TRUE, mean.label=TRUE, ylim=c(16,23),
          barwidth=5, digits=2, col="brown", pch=1, barcol="red",
          main="학교성적에 따른 집단별 자아존중감 수준")

############################
# 일원분산분석 : 다중비교
install.packages("agricolae")
library(agricolae)
scheffe.test(ano1, "grp.grade.factor", alpha=0.05, console=TRUE)
  #Scheffe의 사후검정
LSD.test(ano1, "grp.grade.factor", alpha=0.05, console=TRUE)
  #Fisher의 LSD(Least Significant Difference) 사후검정
duncan.test(ano1, "grp.grade.factor", alpha=0.05, console=TRUE)
  #Duncan의 사후 검정
## 사후검정 결과 학교성적 정도에 따른 3집단 간의 평균차이는 실험별오류율 0.05에서
## 모두 통계적으로 유의한 차이가 있는 것으로 나타났다.


##########################################################################
## 이원분산분석 : 
# 성별에 따른 학교성적 집단 간에 부모에 대한 애착 수준은 다를 것이다.
#  (교호작용 효과)
# 성별 집단 간에 부모에 대한 애착 수준은 다를 것이다. (성별 주효과)
# 학교성적에 따른 집단 간에 부모에 대한 애착 수준은 다를 것이다. (성적 주효과)
##########################################################################
attach(spssdata)
spssdata$attachment <- q33a01w1+q33a02w1+q33a03w1+q33a04w1+q33a05w1+q33a06w1
library(sjlabelled)
spssdata$attachment <- set_label(spssdata$attachment, "부모에 대한 애착")
 # 부모에 대한 애착 변수

spssdata$sexw1.factor <- factor(spssdata$sexw1, levels=c(1,2),
                                labels=c("남자","여자"))
  #범주형 변수로 변환

## 이원분산분석
tw.ano1a <- aov(attachment ~ sexw1.factor + grp.grade.factor + 
                sexw1.factor:grp.grade.factor, data=spssdata)
anova(tw.ano1a)

tw.ano1b <- aov(attachment ~ sexw1.factor*grp.grade.factor, data=spssdata)
anova(tw.ano1b)

############################
# 이원분산분석 : 다중비교
library(agricolae)
scheffe.test(tw.ano1a, "grp.grade.factor", alpha=0.05, console=TRUE)
#Scheffe의 사후검정
LSD.test(tw.ano1a, "grp.grade.factor", alpha=0.05, console=TRUE)
#Fisher의 LSD(Least Significant Difference) 사후검정
duncan.test(tw.ano1a, "grp.grade.factor", alpha=0.05, console=TRUE)
#Duncan의 사후 검정
## 교호작용과 성별 주효과가 고려된 이원분산분석에서 학교성적에 대한 다중비교 결과 
## 학교성적 정도에 따른 3집단 간의 평균차이는 실험별오류율 0.05에서
## 모두 통계적으로 유의한 차이가 있는 것으로 나타났다.

###################################
# 교호작용 효과: 집단별 평균 비교
library(pshych)
describeBy(spssdata$attachment, list(spssdata$sexw1,spssdata$grp.grade),
           mat=TRUE, digits=2)
 # describeBy함수 사용: 집단별 기초통계량 값 출력
aggregate(attachment ~ sexw1+grp.grade, data=spssdata, FUN='mean')
 # FUN='mean'으로 평균 출력 지정
tapply(spssdata$attachment, spssdata[,c("sexw1","grp.grade")], mean)
tapply(spssdata$attachment, list(spssdata$sexw1, spssdata$grp.grade), mean)

### 두 독립변수의 변수값 설명을 하나의 변수로 변환
attach(spssdata)
spssdata$grp.sex.grade[sexw1==1 & grp.grade==1] <- 11
spssdata$grp.sex.grade[sexw1==1 & grp.grade==2] <- 12
spssdata$grp.sex.grade[sexw1==1 & grp.grade==3] <- 13
spssdata$grp.sex.grade[sexw1==2 & grp.grade==1] <- 21
spssdata$grp.sex.grade[sexw1==2 & grp.grade==2] <- 22
spssdata$grp.sex.grade[sexw1==2 & grp.grade==3] <- 23
detach(spssdata)


library(sjlabelled)
spssdata$grp.sex.grade <- set_label(spssdata$grp.sex.grade, "성별 학교성적")
  # 변수설명
spssdata$grp.sex.grade <- set_labels(spssdata$grp.sex.grade,
      labels=c("남자/낮은 학교성적 집단", "남자/중간 학교성적 집단", 
               "남자/높은 학교성적 집단", "여자/낮은 학교성적 집단", 
               "여자/중간 학교성적 집단", "여자/높은 학교성적 집단"))
  # 변수값 설명

## 집단에 따른 기술통계량 
# 집단별 기술통계량 표
library(sjstats)
means_by_group(spssdata, dv=attachment, grp=grp.sex.grade, encoding="EUC-KR", out="viewer")

# gplots 패키지를 이용한 기술통계량 비교 도표
spssdata$grp.sex.grade.factor <- factor(spssdata$grp.sex.grade,
            labels=c("남자/낮은 학교성적 집단", "남자/중간 학교성적 집단", 
                     "남자/높은 학교성적 집단", "여자/낮은 학교성적 집단", 
                     "여자/중간 학교성적 집단", "여자/높은 학교성적 집단"))
# 문자형 변수로 변환
library(gplots)
plotmeans(attachment~grp.sex.grade.factor, data=spssdata, xlab="성별 학교성적",
          ylab="부모애착", ci.label=TRUE, mean.label=TRUE, ylim=c(17,24.5),
          barwidth=5, digits=2, col="brown", pch=1, barcol="red",
          main="성별 학교성적에 따른 집단별 부모애착 수준")

# 교호작용 그림
interaction.plot(spssdata$grp.grade.factor, spssdata$sexw1.factor, 
                 spssdata$attachment , col=c(2,4),
                 trace.label="성별", xlab="학교성적", ylab="부모에 대한 애착",
                 ylim=c(17,24), type="b", 
                 fun=function(x) mean(x,na.rm=TRUE))

save(spssdata, file="spssdata.RData")