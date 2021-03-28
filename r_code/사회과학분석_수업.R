setwd("C:/Users/김서영/Desktop/문서/2020년2학기/사회과학자료/중2 패널 데이터(SPSS)")
install.packages("Hmisc")
library(Hmisc)
test<-spss.get("C:/Users/김서영/Desktop/문서/2020년2학기/사회과학자료/중2 패널 데이터(SPSS)/04-1 중2 패널 1차년도 데이터(SPSS).sav",
               use.value.labels=FALSE)
#names(test)
select_variables <- c(1,4,9,10,19,101,102,103,328:337,
                      339:348,372,375,377,379,484:489,
                      496:504,532)
test1<-test[select_variables]
#test1<-test[,select_variables]
spssdata<-test1[which(test1$scharewl >= 
                        100&test1$scharew1 <200), ]
####################################################
install.packages("sjmisc")
library(sjmisc)
install.packages("sjlabelled")
library(sjlabelled)
test.lables<-spss.get("C:/Users/김서영/Desktop/문서/2020년2학기/사회과학자료/중2 패널 데이터(SPSS)/04-1 중2 패널 1차년도 데이터(SPSS).sav",
                      use.value.labels=TRUE)
test.lables1<-test.lables[select_variables]
spssdata1<-test.lables[which(as.numeric(test.lables1$scharew1)<26),]
labels.spss.values<-get_labels(spssdata1)
spssdata1<-set_lables(spssdata, labels=labels.spss.values,
                      force.values=FALSE, force.labels=TRUE)
#변수생성
attach(spssdata)
spssdata$attachment = q33a01w1+q33a02w1+q33a03w1+
  q33a04w1+q33a05w1+q33a06w1
#아니면 위에꺼말고 spssdata <-transform(spss,
#attachment=q33a01w1+q33a02w1+q33a03w1+q33a04w1+q33a05w1+q33a06w1)
spssdata$grade=q18a1w1+q18a2w1+q18a3w1
detach(spssdata)

#학교성적 연속형말고 변수를 3집단으로 분류하기
install.packages("epiDisplay")
libarary(epiDisplay)
#이거쓰면 교과서말고 한꺼번에 자료 볼수 있음
tab1(spssdata$grade, cum.percent=TURE)
attach(spssdata)
spssdata$grp.grade[grade>=min(grade)&grade<=8]<-1
spssdata$grp.grade[grade>=9$grade<=10]<-2
spssdata$grp.grade[grade>=11&grade<=max(grade)]<-3
#detach(spssdata)
tab1(spssdata$grp.grade, cum.percent=TURE)

#q50w1의 속성을 부정/중립/긍정으로
attach(spssdata)
spssdata$satisfaction[]





