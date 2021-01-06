setwd("D:/Work/Class/Survey Data Analysis/사회과학 통계분석/실습")
### 데이터 불러들이기
load("spssdata.RData")
attach(spssdata)

######### 단순회귀분석 ##########
# 연구가설
#  1) 자기신뢰감은 자아존중감에 영향을 미칠 것인다.

regression1_1 <- lm(self.esteem ~ self.confidence, data=spssdata)
summary(regression1_1)

#jtools 패키지
library(jtools)
summ(regression1_1, confint=TRUE)
effect_plot(regression1_1, pred = self.confidence, interval = TRUE, 
            plot.points = TRUE, data=spssdata)

#sjPlot 패키지
library(sjPlot)
tab_model(regression1_1, show.se=TRUE, show.ci=0.95, show.fstat=TRUE,
          auto.label=FALSE)

#ggplot 패키지
require(ggplot2)
equation = function(x) {
  lm_coef <- list(a = round(coef(x)[1], digits = 2),
                  b = round(coef(x)[2], digits = 2),
                  r2 = round(summary(x)$r.squared, digits = 2));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq));                 
}


p11 <- ggplot(spssdata, aes(x=self.confidence, y=self.esteem)) + geom_point(shape=1) + 
  geom_smooth(method="lm", formula=y~x) +
  scale_x_continuous(name = "자기신뢰감") +
  scale_y_continuous(name = "자기존중감") +
  annotate("rect", xmin = 3.50, xmax = 8.1, ymin = 27.5, ymax = 29.5, 
           fill="white", colour="red") +
  annotate("text", x = 6.0, y = 28.5, label = equation(regression1_1), parse = TRUE)
p11

library(ggpubr)
formul <- y ~ x
p <- ggplot(spssdata, aes(x=self.confidence, y=self.esteem)) +
  geom_point() +
  geom_smooth(method = "lm", formula=y~x)  +
  scale_x_continuous(name = "자기신뢰감") +
  scale_y_continuous(name = "자기존중감") +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = formul
  ) +
  theme_bw()
ggpar(p, palette = "jco")

##################################
# 연구가설
#  2) 부모에 대한 애착은 자아존중감에 영향을 미칠 것이다.

regression1_2 <- lm(self.esteem ~ attachment, data=spssdata)
summary(regression1_2)

#jtools 패키지
library(jtools)
summ(regression1_2, confint=TRUE)
effect_plot(regression1_2, pred = attachment, interval = TRUE, 
            plot.points = TRUE, data=spssdata)

install.packages("huxtable")
export_summs(regression1_1, regression1_2, scale=TRUE,
             error_format = "[{conf.low}, {conf.high}]")

#sjPlot 패키지
library(sjPlot)
tab_model(regression1_1, regression1_2, show.se=TRUE, show.ci=FALSE, show.fstat=TRUE,
          show.aic=TRUE, auto.label=FALSE)
#ggplot 패키지
require(ggplot2)
library(ggpubr)
formul2 <- y ~ x
p2 <- ggplot(spssdata, aes(x=attachment, y=self.esteem)) +
  geom_point() +
  geom_smooth(method = "lm", formula=y~x)  +
  scale_x_continuous(name = "부모에 대한 애착") +
  scale_y_continuous(name = "자기존중감") +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = formul2
  ) +
  theme_bw()
ggpar(p2, palette = "jco")

########################################################################
######### 다중회귀분석 ##########
# 연구가설
#  1-1) 성별은 자아존중감에 영향을 미칠 것인다.
#  1-1) 자기신뢰감은 자아존중감에 영향을 미칠 것인다.
#  1-1) 부모에 대한 애착은 자아존중감에 영향을 미칠 것인다.
#  1-1) 부모 감독은 자아존중감에 영향을 미칠 것인다.
#  1-1) 부정적 양육은 자아존중감에 영향을 미칠 것인다.

#변수만들기
spssdata$monitor <- q33a07w1 + q33a08w1 + q33a09w1 + q33a10w1
library(sjlabelled)
spssdata$monitor <- set_label(spssdata$monitor, "부모 감독")

spssdata$sexw1.re[sexw1 == 1] <- 0
spssdata$sexw1.re[sexw1 == 2] <- 1
spssdata$sexw1.re <- set_label(spssdata$sexw1.re, "성별")
spssdata$sexw1.re <- set_labels(spssdata$sexw1.re, labels=c("남자","여자"))

###### 다중회귀모형 적합
regression2 <- lm(self.esteem ~ sexw1.re+self.confidence+attachment+
                    monitor+negative.parenting, data=spssdata)
summary(regression2)

# 표준화 계수값
library(QuantPsyc)
lm.beta(regression2)
round(lm.beta(regression2), 3)

# 다중공선성 진단
library(car)
vif(regression2)

#jtools 패키지
library(jtools)
summ(regression2, confint=TRUE)
effect_plot(regression2, pred = self.confidence, interval = TRUE, 
            plot.points = TRUE, data=spssdata)

#sjPlot 패키지
library(sjPlot)
tab_model(regression2, show.se=TRUE, show.std=TRUE, show.ci=0.95, 
          show.fstat=TRUE, auto.label=FALSE)

set_theme(axis.title.size=1.2, axis.textsize=1.0, title.align="center")
plot_model(regression2, show.values=TRUE, wrap.labels=5, line.size=1.2,
           axis.title="자아존중감")

plot_model(regression2, type="std", show.values=TRUE, wrap.labels=5, 
           axis.title="자아존중감(표준화계수)", line.size=1.2)
  # 표준화회귀계수

#lmtest 패키지 : 다중공선성 진단
library(mctest)
mc.plot(regression2)
imcdiag(regression2)

###dummy variable
spssdata$job.dummy1 <- ifelse(spssdata$q2w1==2, 1, 0)
spssdata$job.dummy2 <- ifelse(spssdata$q2w1==3, 1, 0)

spssdata$job.dummy1 <- set_label(spssdata$job.dummy1, "직업결정_대강의 생각")
spssdata$job.dummy2 <- set_label(spssdata$job.dummy2, "직업결정_정해지지 않음")

table(spssdata$q2w1)
table(spssdata$job.dummy1)
table(spssdata$job.dummy2)

regression4 <- lm(self.esteem ~ sexw1.re+attachment+monitor+job.dummy1+job.dummy2,
                  data=spssdata)
summary(regression4)

spssdata$q2w1a <- factor(spssdata$q2w1)
spssdata$q2w1a <- set_label(spssdata$q2w1a, "직업결정")
regression5 <- lm(self.esteem ~ sexw1.re+attachment+monitor+q2w1a,
                  data=spssdata)
summary(regression5)

# 표준화 계수값
tab_model(regression5, show.se=TRUE, show.std=TRUE, show.ci=0.95, 
          show.fstat=TRUE, auto.label=FALSE)


### 변수선택
spssdata.no.na <- na.omit(spssdata[c("self.esteem","sexw1.re","self.confidence",
                                     "attachment","monitor","negative.parenting")])
regression3 <- lm(self.esteem ~ sexw1.re+self.confidence+attachment+
                    monitor+negative.parenting, data=spssdata.no.na)
null <- lm(self.esteem ~ 1, data=spssdata.no.na)

step1 <- step(null, scope=list(lower=null, upper=regression3), direction="forward")
summary(step1)
  # 전진선택

step2 <- step(regression3, direction="backward")
summary(step2)
  # 후진제거

step3 <- step(regression3, direction="both")
summary(step3)
 # 선택제거

### 교호작용 모형
regression6 <- lm(self.confidence ~ sexw1.re+
                    attachment*monitor+negative.parenting, data=spssdata)
summary(regression6)
vif(regression6) 

set_theme(axis.title.size=1.2, axis.textsize=1.0, legend.title.size=1.2,
          legend.size=1.1, title.align="center", legend.post="bottom")
plot_model(regression6, show.values=TRUE, wrap.labels=5, 
           title="부모애착과 부모감독의 교호작용 모형", 
           axis.title=c("자기신뢰감"))

#############
## Full model
spssdata$q37a01w1.re[q37a01w1 == 1] <- 0
spssdata$q37a01w1.re[q37a01w1 == 2] <- 1
spssdata$q37a02w1.re[q37a02w1 == 1] <- 0
spssdata$q37a02w1.re[q37a02w1 == 2] <- 1
spssdata$q37a03w1.re[q37a03w1 == 1] <- 0
spssdata$q37a03w1.re[q37a03w1 == 2] <- 1
spssdata$q37a04w1.re[q37a04w1 == 1] <- 0
spssdata$q37a04w1.re[q37a04w1 == 2] <- 1

attach(spssdata)
spssdata$delinquency <- q37a01w1.re+q37a02w1.re+q37a03w1.re+q37a04w1.re
spssdata$delinquency <- set_label(spssdata$delinquency, "청소년 비행")

spssdata$aggression <- q48c1w1+q48c2w1+q48c3w1+q48c4w1+q48c5w1+q48c6w1
spssdata$aggression <- set_label(spssdata$aggression, "공격성")

spssdata$satisfaction <- q50w1
spssdata$satisfaction <- set_label(spssdata$satisfaction, "삶의 만족도")

spssdata$centered.attachment <- scale(spssdata$attachment, center=TRUE, scale=FALSE)

attach(spssdata)
spssdata.na.omit <- na.omit(spssdata[c("self.esteem","sexw1.factor","grp.grade.factor",
                    "q2w1a","attachment","monitor","negative.parenting",
                    "self.control","delinquency", "self.confidence", "aggression",
                    "satisfaction")])

regression.f1 <- lm(self.esteem ~ sexw1.factor+grp.grade.factor+
                    sexw1.factor:grp.grade.factor+ q2w1a+
                    attachment+ monitor+negative.parenting+self.control+ delinquency+
                    self.confidence+aggression+satisfaction , data=spssdata.na.omit)
summary(regression.f1)
library(sjPlot)
tab_model(regression.f1, show.se=TRUE, show.fstat=TRUE,
          auto.label=TRUE)

#lmtest 패키지 : 다중공선성 진단
library(mctest)
mc.plot(regression.f1)
imcdiag(regression.f1)

# 표준화 계수값
tab_model(regression.f1, show.se=TRUE, show.std=TRUE, auto.label=TRUE)

regression.f2 <- lm(self.esteem ~ sexw1.factor+grp.grade.factor+
                     q2w1a+ grp.grade.factor:q2w1a+
                     attachment+ monitor+negative.parenting+self.control+ delinquency+
                     self.confidence+aggression+satisfaction , data=spssdata.na.omit)
summary(regression.f2)
tab_model(regression.f2, show.se=TRUE, show.fstat=TRUE,
          auto.label=TRUE)

# 최종모형 후보
regression.f <- lm(self.esteem ~ sexw1.factor+grp.grade.factor+
                      q2w1a+ attachment+ monitor+negative.parenting+
                      self.control+ delinquency+
                      self.confidence+aggression+satisfaction , data=spssdata.na.omit)
summary(regression.f)
tab_model(regression.f, show.se=TRUE, show.fstat=TRUE,
          auto.label=TRUE)

#lmtest 패키지 : 다중공선성 진단
library(mctest)
mc.plot(regression.f)
imcdiag(regression.f)

# 변수선택
step3 <- step(regression.f, direction="both")
summary(step3)
# 선택제거

# 변수선택 결과 모형
reg.mod <- lm(self.esteem ~ grp.grade.factor + q2w1a + attachment + 
self.control + self.confidence + aggression + satisfaction, 
data = spssdata.na.omit)

summary(reg.mod)
tab_model(reg.mod, show.se=TRUE, show.fstat=TRUE,
          auto.label=TRUE)

#lmtest 패키지 : 다중공선성 진단
library(mctest)
mc.plot(reg.mod)
imcdiag(reg.mod)

# 표준화 계수값
tab_model(reg.mod, show.se=TRUE, show.std=TRUE, auto.label=TRUE)

# Partial plots
library(ggplot2)
effect_plot(reg.mod, pred = grp.grade.factor, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = q2w1a, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = attachment, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = self.control, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = self.confidence, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = aggression, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = satisfaction, interval = TRUE, plot.points = TRUE)

############################ 잔차분석
# 더빈왓슨통계량 : 잔차의 독립성
library(car)
durbinWatsonTest(reg.mod)

id <- c(1:nrow(spssdata.na.omit))
resid <- rstandard(reg.mod)
plot(id, resid, main="잔차의 독립성", ylab="표준화잔차",pch=21)

# 잔차그림 : 오차의 정규성 및 이상점, 영향점
pred <- predict(reg.mod)
plot(pred, resid, main="잔차 vs 적합값", pch=21, col="red", ylab="표준화잔차", 
     xlab="적합값")
abline(0,0)

library(sjPlot)
plot_residuals(reg.mod)

par(mfrow=c(3,2))
for(i in 1:5) plot(reg.mod, i)
par(mfrow=c(1,1))

library(ggfortify)
autoplot(reg.mod)

# Partial residuals plots
effect_plot(reg.mod, pred = grp.grade.factor, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = q2w1a, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = attachment, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = self.control, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = self.confidence, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = aggression, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = satisfaction, interval = TRUE, partial.residuals = TRUE)

###### 이상점 제거
remove <- c("444","477","284")
spssdata.out <- spssdata.na.omit[!row.names(spssdata.na.omit)%in%remove,]

# full model
regression.fout <- lm(self.esteem ~ sexw1.factor+grp.grade.factor+ q2w1a+
                     attachment+ monitor+negative.parenting+self.control+ delinquency+
                     self.confidence+aggression+satisfaction , data=spssdata.out)
summary(regression.fout)
tab_model(regression.fout, show.se=TRUE, show.fstat=TRUE,
          auto.label=TRUE)

#lmtest 패키지 : 다중공선성 진단
library(mctest)
mc.plot(regression.fout)
imcdiag(regression.fout)

# 표준화 계수값
tab_model(regression.fout, show.se=TRUE, show.std=TRUE, auto.label=TRUE)

# 변수선택
step3out <- step(regression.fout, direction="both")
summary(step3out)
# 선택제거

# 변수선택 결과 모형
reg.mod2 <- lm(self.esteem ~ grp.grade.factor + attachment + 
                 self.control + self.confidence + aggression + satisfaction, 
               data = spssdata.out)
summary(reg.mod2)
tab_model(reg.mod2, show.se=TRUE, show.fstat=TRUE,
          auto.label=TRUE)

# 직업결정 변수 추가
reg.mod3 <- lm(self.esteem ~ grp.grade.factor + q2w1a + attachment + 
                 self.control + self.confidence + aggression + satisfaction, 
               data = spssdata.out)
summary(reg.mod3)
tab_model(reg.mod3, show.se=TRUE, show.fstat=TRUE,
          auto.label=TRUE)

#lmtest 패키지 : 다중공선성 진단
library(mctest)
mc.plot(reg.mod3)
imcdiag(reg.mod3)

# 표준화 계수값
tab_model(reg.mod3, show.se=TRUE, show.std=TRUE, auto.label=TRUE)

############################ 잔차분석
# 더빈왓슨통계량 : 잔차의 독립성
library(car)
durbinWatsonTest(reg.mod3)

id3 <- c(1:nrow(spssdata.out))
resid3 <- rstandard(reg.mod3)
plot(id3, resid3, main="잔차의 독립성", ylab="표준화잔차",pch=21)

# 잔차그림 : 오차의 정규성 및 이상점, 영향점
pred3 <- predict(reg.mod3)
plot(pred3, resid3, main="잔차 vs 적합값", pch=21, col="red", ylab="표준화잔차", 
     xlab="적합값")
abline(0,0)

library(sjPlot)
plot_residuals(reg.mod3)

par(mfrow=c(3,2))
for(i in 1:5) plot(reg.mod3, i)
par(mfrow=c(1,1))


##################################################################
###### 이상점 제거
remove2 <- c("865","1000","1486")
spssdata.out2 <- spssdata.out[!row.names(spssdata.out)%in%remove2,]

# full model
regression.fout2 <- lm(self.esteem ~ sexw1.factor+grp.grade.factor+ q2w1a+
                        attachment+ monitor+negative.parenting+self.control+ delinquency+
                        self.confidence+aggression+satisfaction , data=spssdata.out2)
summary(regression.fout2)
tab_model(regression.fout2, show.se=TRUE, show.fstat=TRUE,
          auto.label=TRUE)

#lmtest 패키지 : 다중공선성 진단
library(mctest)
mc.plot(regression.fout2)
imcdiag(regression.fout2)

# 표준화 계수값
tab_model(regression.fout2, show.se=TRUE, show.std=TRUE, auto.label=TRUE)

# 변수선택
step3out2 <- step(regression.fout2, direction="both")
summary(step3out2)
# 선택제거

# 변수선택 결과 모형
reg.mod4 <- lm(self.esteem ~ grp.grade.factor + q2w1a + attachment + negative.parenting +
                 self.control + self.confidence + aggression + satisfaction, 
               data = spssdata.out2)
summary(reg.mod4)
tab_model(reg.mod4, show.se=TRUE, show.fstat=TRUE,
          auto.label=TRUE)

#lmtest 패키지 : 다중공선성 진단
library(mctest)
mc.plot(reg.mod4)
imcdiag(reg.mod4)

# 표준화 계수값
tab_model(reg.mod4, show.se=TRUE, show.std=TRUE, auto.label=TRUE)

############################ 잔차분석
# 더빈왓슨통계량 : 잔차의 독립성
library(car)
durbinWatsonTest(reg.mod4)

id4 <- c(1:nrow(spssdata.out2))
resid4 <- rstandard(reg.mod4)
plot(id4, resid4, main="잔차의 독립성", ylab="표준화잔차",pch=21)

# 잔차그림 : 오차의 정규성 및 이상점, 영향점
pred4 <- predict(reg.mod4)
plot(pred4, resid4, main="잔차 vs 적합값", pch=21, col="red", ylab="표준화잔차", 
     xlab="적합값")
abline(0,0)

library(sjPlot)
plot_residuals(reg.mod4)

par(mfrow=c(3,2))
for(i in 1:5) plot(reg.mod4, i)
par(mfrow=c(1,1))

### 최종모형 : 이상점 6개 제거 후 적합한 모형
reg.final <- lm(self.esteem ~ grp.grade.factor + q2w1a + attachment + negative.parenting +
                 self.control + self.confidence + aggression + satisfaction, 
               data = spssdata.out2)
tab_model(reg.final, show.se=TRUE, show.fstat=TRUE,
          auto.label=TRUE)
# 표준화 계수값
tab_model(reg.final, show.se=TRUE, show.std=TRUE, auto.label=TRUE)
