setwd("C:/Users/김서영/Desktop/문서/2020년2학기/사회과학자료/중2 패널 데이터(SPSS)")
load("shsans.RData")
attach(test1)

#-------------------------빈도표
#library(Hmisc)
describe(sexw2, na.rm=T)
describe(scharew2, na.rm=T)
describe(areuw2, na.rm=T)

################-----------독립성 T검정
##전반적인 만족도에 대한 변수 만들기 위해 변수 합치기
test1$major <- q50w2+q29w2+q48a01w2+q48a02w2+q48a03w2
library(sjlabelled)
test1$major  <- set_label(test1$major, "전반적인 만족도")
#--------------------성별 독립성검정
#문자형 변수로 변환
test1$sexw2a <- factor(test1$sexw2, levels=c(1,2), labels=c("남자","여자"))
## 2. 분산 동질성 검정
var.test(major ~ sexw2, data=test1)
var.test(major ~ sexw2a, data=test1)
## 3. 연구가설 검증
t.test(major ~ sexw2, var.equal=TRUE, data=test1)
t.test(major ~ sexw2a, var.equal=TRUE, data=test1)
## 4. 집단에 따른 기술 통계량
library(psych)
describeBy(test1$major, group=test1$sexw2a)

#--------------직업계획 독립성검정
#문자형 변수로 변환
test1$q24w2a <- factor(test1$q24w2, levels=c(1,2), labels=c("있다","없다"))
## 2. 분산 동질성 검정
var.test(major ~ q24w2a, data=test1)
## 3. 연구가설 검증
t.test(major ~ q24w2a, var.equal=TRUE, data=test1)
## 4. 집단에 따른 기술 통계량
#library(psych)
describeBy(test1$major, group=test1$q24w2a)

###################################----일원분산분석
#-------------------------거주지
# 변수설명
test1$areuw2a <- set_label(test1$areuw2, "거주지")
# 변수값 설명
test1$areuw2a <- set_labels(test1$areuw2,levels=c(1,2,3), labels=c("동","읍","면"))
# 문자형 변수로 변환
test1$areuw2aa <- factor(test1$areuw2, levels=c(1,2,3), labels=c("동","읍","면"))
#젤먼저 분산 동질성 검정
#install.packages("car")
library(car)
leveneTest(major ~ areuw2aa, data=test1)
#직업 일원분산분석
ano1 <- aov(major ~ areuw2aa, data=test1)
anova(ano1)
# 집단별 평균값
#library(psych)
describeBy(test1$major, group=test1$areuw2aa)

#---------------------------구체적 직업계획 여부
# 변수설명
#install.packages("lawstat")


test1$q2w2a <- set_label(test1$q2w2, "구체적인 직업(창업)계획 여부")
# 변수값 설명
test1$q2w2a <- set_labels(test1$q2w2,levels=c(1,2,3), labels=c("구체적으로 확정해 놓은 직업 있다","대강 생각해 놓은 직업있다","아직 정해놓은 장래의 직업이 없다"))
# 문자형 변수로 변환
test1$q2w2aa <- factor(test1$q2w2, levels=c(1,2,3), labels=c("구체적으로 확정해 놓은 직업 있다","대강 생각해 놓은 직업있다","아직 정해놓은 장래의 직업이 없다"))
#동질성 검정
leveneTest(major ~ q2w2aa, data=test1, center=mean)
#거주지 일원분산분석
ano1 <- aov(major ~ q2w2aa, data=test1)
anova(ano1)

# 집단별 평균값
#library(psych)
describeBy(test1$major, group=test1$q2w2aa)

###################----------이원배치 분산 분석
#----------(직업선택에 대한 현재상황& 학교생활 적응)
ano1a <- aov(major ~ careerch + nowsi + 
               careerch:nowsi , data=test1)
anova(ano1a)
#----------(직업선택에 대한 현재상황&학교 수업의 성적향상 #효과여부-주요과목)
ano2a <- aov(major ~ careerch + score + 
               careerch:score , data=test1)
anova(ano2a)
#-------------직업선택에 대한 현재상황&나의 성격유형
ano3a <- aov(major ~ careerch + mind + 
               careerch:mind , data=test1)
anova(ano3a)
#-------------직업선택에 대한 현재상황&학교 선생님과의 관계
ano4a <- aov(major ~ careerch + schlife + 
               careerch:schlife , data=test1)
anova(ano4a)
#-------------직업선택에 대한 현재상황&매우 친한 친구와의 관계
ano5a <- aov(major ~ careerch + friend + 
               careerch:friend , data=test1)
anova(ano5a)
#-------------직업선택에 대한 현재상황&자신에 대한 신뢰
ano6a <- aov(major ~ careerch + tru + 
               careerch:tru , data=test1)
anova(ano6a)
#-------------직업선택에 대한 현재상황&부모님과의 마찰로 겪는 #스트레스
ano7a <- aov(major ~ careerch + paren + 
               careerch:paren , data=test1)
anova(ano7a)
#-------------직업선택에 대한 현재상황&외모에 대한 불만
ano8a <- aov(major ~ careerch + fri2 + 
               careerch:fri2 , data=test1)
anova(ano8a)
#------------학교생활적응&학교 수업의 성적향상 효과여부 (주요과목만)
ano9a <- aov(major ~ nowsi + score + 
               nowsi:score , data=test1)
anova(ano9a)
#------------학교생활적응&나의 성격유형
ano10a <- aov(major ~ nowsi + mind + 
               nowsi:mind , data=test1)
anova(ano10a)
#------------학교생활적응&학교 선생님과의 관계
ano11a <- aov(major ~ nowsi + schlife + 
                nowsi:schlife , data=test1)
anova(ano11a)
#------------학교생활적응&매우 친한 친구와의 관계
ano12a <- aov(major ~ nowsi + friend + 
                nowsi:friend , data=test1)
anova(ano12a)
#------------학교생활적응&자신에 대한 신뢰도
ano13a <- aov(major ~ nowsi + tru + 
                nowsi:tru , data=test1)
anova(ano13a)
#------------학교생활적응&부모님과의 갈등으로 겪는 스트레스
ano14a <- aov(major ~ nowsi + paren + 
                nowsi:paren , data=test1)
anova(ano14a)
#------------학교생활적응&외모에 대한 불만
ano15a <- aov(major ~ nowsi + fri2 + 
                nowsi:fri2 , data=test1)
anova(ano15a)
#-----------학교 수업의 성적향상 효과여부 (주요과목만)&나의 성격유형 
ano16a <- aov(major ~ score + mind + 
                score:mind , data=test1)
anova(ano16a)
#-----------학교수업의 성적향상 효과여부 (주요과목만)&학교 선생님과의 관계
ano17a <- aov(major ~ score + schlife + 
                score:schlife , data=test1)
anova(ano17a)
#-----------학교 수업의 성적향상 효과여부 (주요과목만)&매우 친한 친구와의 관계
ano18a <- aov(major ~ score + friend + 
                score:friend , data=test1)
anova(ano18a)
#-----------학교 수업의 성적향상 효과여부 (주요과목만)&자신에 대한 신뢰도
ano19a <- aov(major ~ score + tru + 
                score:tru , data=test1)
anova(ano19a)
#-----------학교 수업의 성적향상 효과여부 (주요과목만) &부모님과의 갈등으로 겪는 스트레스
ano20a <- aov(major ~ score + paren + 
                score:paren , data=test1)
anova(ano20a)
#-----------학교 수업의 성적향상 효과여부 (주요과목만)&외모에 대한 불만
ano21a <- aov(major ~ score + fri2 + 
                score:fri2 , data=test1)
anova(ano21a)
#----------나의 성격유형&학교 선생님과의 관계
ano22a <- aov(major ~ mind + schlife + 
                mind:schlife , data=test1)
anova(ano22a)
#----------나의 성격유형&매우 친한 친구와의 관계
ano23a <- aov(major ~ mind + friend + 
                mind:friend , data=test1)
anova(ano23a)
#----------나의 성격유형&자신에 대한 신뢰도
ano24a <- aov(major ~ mind + tru + 
                mind:tru , data=test1)
anova(ano24a)
#----------나의 성격유형&부모님과의 갈등으로 겪는 스트레스
ano25a <- aov(major ~ mind + paren + 
                mind:paren , data=test1)
anova(ano25a)
#----------나의 성격유형&외모에 대한 불만 
ano26a <- aov(major ~ mind + fri2 + 
                mind:fri2, data=test1)
anova(ano26a)
#-----------학교 선생님과의 관계&매우 친한 친구와의 관계 
ano27a <- aov(major ~ schlife + friend + 
                schlife:friend, data=test1)
anova(ano27a)
#-----------학교 선생님과의 관계&자신에 대한 신뢰도
ano28a <- aov(major ~ schlife + tru + 
                schlife:tru, data=test1)
anova(ano28a)
#-----------학교 선생님과의 관계&부모님과의 갈등으로 겪는 스트레스
ano29a <- aov(major ~ schlife + paren + 
                schlife:paren, data=test1)
anova(ano29a)
#-----------학교 선생님과의 관계& 외모에 대한 불만
ano30a <- aov(major ~ schlife + fri2 + 
                schlife:fri2, data=test1)
anova(ano30a)
#-----------매우 친한 친구와의 관계&자신에 대한 신뢰도
ano31a <- aov(major ~ friend + tru + 
                friend:tru, data=test1)
anova(ano31a)
#-----------매우 친한 친구와의 관계&부모님과의 갈등으로 겪는 스트레스
ano32a <- aov(major ~ friend + paren + 
                friend:paren, data=test1)
anova(ano32a)
#-----------매우 친한 친구와의 관계&외모에 대한 불만
ano33a <- aov(major ~ friend + fri2 + 
                friend:fri2, data=test1)
anova(ano33a)
#-----------자신에 대한 신뢰도&부모님과의 갈등으로 겪는 스트레스
ano34a <- aov(major ~ tru + paren + 
                tru:paren, data=test1)
anova(ano34a)
#-----------자신에 대한 신뢰도&외모에 대한 불만
ano35a <- aov(major ~ tru + fri2 + 
                tru:fri2, data=test1)
anova(ano35a)
#----------부모님과의 갈등으로 겪는 스트레스&외모에 대한 불만
ano36a <- aov(major ~ paren + fri2 + 
                paren:fri2, data=test1)
anova(ano36a)


##########--상관분석
#### sjPlot 패키지를 이용한 상관분석
library(sjPlot)
library(sjlabelled)
test1$careerch <-set_label(test1$careerch, "직업 선택에 대한 현재 상황")
test1$nowsi<-set_label(test1$nowsi,"학교 생활 적응")
test1$score<-set_label(test1$score, "학교 생활 적응")
test1$mind<-set_label(test1$mind,"나의 성격유형")
test1$schlife<-set_label(test1$schlife,"학교 선생님과의 관계")
test1$friend<-set_label(test1$friend,"매우 친한 친구와의 관계")
test1$tru<-set_label(test1$tru,"자신에 대한 신뢰도")
test1$paren<-set_label(test1$paren,"부모님과의 갈등으로 겪는 스트레스")
test1$fri2<-set_label(test1$fri2,"외모에 대한 불만 ")
cor.var <- test1[c("major","careerch","nowsi","score","mind","schlife","friend","tru","paren","fri2")]
sjp.corr(cor.var, corr.method="pearson", na.deletion="pairwise", p.numeric=TRUE)



