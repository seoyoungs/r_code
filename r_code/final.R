setwd("C:/Users/김서영/Desktop/문서/2020년2학기/사회과학자료/중2 패널 데이터(SPSS)")
load("shsans.RData")
attach(test1)

########################################################################
######### 다중회귀분석 ##########
# 연구가설
#  1-1) 성별은 자아존중감에 영향을 미칠 것인다.
#  1-1) 자기신뢰감은 자아존중감에 영향을 미칠 것인다.
#  1-1) 부모에 대한 애착은 자아존중감에 영향을 미칠 것인다.
#  1-1) 부모 감독은 자아존중감에 영향을 미칠 것인다.
#  1-1) 부정적 양육은 자아존중감에 영향을 미칠 것인다.

#변수만들기
library(sjlabelled)

test1$sexw1.re[sexw2 == 1] <- 0
test1$sexw1.re[sexw2 == 2] <- 1
test1$sexw1.re <- set_label(test1$sexw1.re, "성별")
test1$sexw1.re <- set_labels(test1$sexw1.re, labels=c("남자","여자"))
test1$major <- q50w2+q29w2+q48a01w2+q48a02w2+q48a03w2
library(sjlabelled)
test1$major  <- set_label(test1$major, "전반적인 만족도")

###### 다중회귀모형 적합
regression2 <- lm(major ~ sexw1.re+nowsi+score+
                  tru+fri2+mind:schlife+friend:tru+friend:paren
                    , data=test1)
summary(regression2)


library(car)
durbinWatsonTest(regression2)
id3 <- c(1:nrow(test1))
resid3 <- rstandard(regression2)
plot(id3, resid3, main="잔차의 독립성", ylab="표준화잔차",pch=21)













