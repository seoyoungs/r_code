setwd("D:/Work/Class/Survey Data Analysis/사회과학 통계분석/실습")
### 데이터 불러들이기
load("spssdata.RData")
attach(spssdata)

####변수만들기
library(sjlabelled)
# 부정적양육
spssdata$negative.parenting <- q33a12w1+q33a13w1+q33a14w1+q33a15w1
spssdata$negative.parenting <- set_label(spssdata$negative.parenting, "부정적양육")
# 자기신뢰감
spssdata$self.confidence <- q48b1w1+q48b2w1+q48b3w1
spssdata$self.confidence <- set_label(spssdata$self.confidence, "자기신뢰감")
# 공격성
spssdata$aggressive <- q48c1w1+q48c2w1+q48c3w1+q48c4w1+q48c5w1+q48c6w1
spssdata$aggressive <- set_label(spssdata$aggressive, "공격성")

attach(spssdata)

#### cor 함수를 이용한 상관분석
cor(spssdata[c("attachment","negative.parenting","self.control","self.esteem",
               "self.confidence","aggressive")], use="pairwise.complete.obs")
round(cor(spssdata[c("attachment","negative.parenting","self.control",
                     "self.esteem","self.confidence","aggressive")], 
          use="pairwise.complete.obs"),2)
  #상관계수의 소수점 자리 조정

#분석하려는 변수만을 객체로 만들어 분석하는 방법
cor.var <- spssdata[c("attachment","negative.parenting","self.control",
                      "self.esteem","self.confidence","aggressive")]
cor(cor.var, use="pairwise.complete.obs")
round(cor(cor.var, use="pairwise.complete.obs"),2) #소수점자리

##두 변수 간의 상관계수와 유의도 출력
cor.test(attachment, self.confidence)

#### psych 패키지를 이용한 상관분석
library(psych)
cor.var <- spssdata[c("attachment","negative.parenting","self.control",
                      "self.esteem","self.confidence","aggressive")]
corr.test(cor.var, method="pearson", use="pairwise.complete.obs", adjust="none")
   #모든변수 간의 상관계수와 유의도 출력

#### sjPlot 패키지를 이용한 상관분석
library(sjPlot)
cor.var <- spssdata[c("attachment","negative.parenting","self.control",
                      "self.esteem","self.confidence","aggressive")]
tab_corr(cor.var, corr.method="pearson", na.deletion="pairwise", 
         p.numeric=TRUE, triangle="lower", encoding="EUC-KR")
  #상관분석 표

set_theme(axis.textsize=1.0)
sjp.corr(cor.var, corr.method="pearson", wrap.labels=5, na.deletion="pairwise")
  #상관분석 도표


#####################################################################################
# Spearman's Rank Correlation and Kendall's Tau
library(sjlabelled)
spssdata$q50w1 <- set_label(spssdata$q50w1, "삶의만족도")

cor.var2 <- spssdata[c("grp.grade","attachment","negative.parenting","self.control",
                      "self.esteem","self.confidence","aggressive","q50w1")]
  #순서형 변수 : grp.grade(성적그룹) & q50w1(삶의 만족도)

## Spearman's Rank correlation
round(cor(cor.var2, method="spearman", use="pairwise.complete.obs"),2) 

library(psych)
corr.test(cor.var2, method="spearman", use="pairwise.complete.obs", adjust="none")

library(sjPlot)
tab_corr(cor.var2, corr.method="spearman", na.deletion="pairwise", 
         p.numeric=TRUE, triangle="lower", encoding="EUC-KR")
set_theme(axis.textsize=1.0)
sjp.corr(cor.var2, corr.method="spearman", wrap.labels=5, na.deletion="pairwise")


## Kendall's Tau
round(cor(cor.var2, method="kendall", use="pairwise.complete.obs"),2) 

library(psych)
corr.test(cor.var2, method="kendall", use="pairwise.complete.obs", adjust="none")

library(sjPlot)
tab_corr(cor.var2, corr.method="kendall", na.deletion="pairwise", 
         p.numeric=TRUE, triangle="lower", encoding="EUC-KR")
set_theme(axis.textsize=1.0)
sjp.corr(cor.var2, corr.method="kendall", wrap.labels=5, na.deletion="pairwise")


##################################
### 편상관분석 : 자기통제력을 통젷한 후 
#                부모에 대한 애착과 공격성 간에는 관계가 있을 것이다.

## psych 패키지 사용
cor.var3 <- spssdata[c("attachment","aggressive","self.confidence",
                       "self.control","negative.parenting")]
cor1 <- cor(cor.var3, use="pairwise.complete.obs")

library(psych)
partial.r(cor1, c(1,2), 4) #편상관계수


## ggm 패키지 사용
examdata1 <- spssdata[c("attachment","aggressive","self.control")]
install.packages("ggm")
library(ggm)
pcor(c("attachment","aggressive","self.control"), var(examdata1, na.rm=TRUE))
  #자기통제력이 제어된 상태에서 부모에 대한 애착과 공격성 간에 편상관계수
pcor.test(pcor(c("attachment","aggressive","self.control"), 
               var(examdata1, na.rm=TRUE)), 1, n=nrow(examdata1))

pcor1 <- var(examdata1, na.rm=TRUE)
parcor(pcor1)

## 자기신뢰감 통제, 공격성과 자기통제력의 편상관계수
examdata2 <- spssdata[c("aggressive","self.control","self.confidence")]
pcor(c("aggressive","self.control","self.confidence"), var(examdata2, na.rm=TRUE))
pcor.test(pcor(c("aggressive","self.control","self.confidence"), 
               var(examdata2, na.rm=TRUE)), 1, n=nrow(examdata2))
pcor2 <- var(examdata2, na.rm=TRUE)
parcor(pcor2)