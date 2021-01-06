setwd("D:/Work/Class/Survey Data Analysis/��ȸ���� ���м�/�ǽ�")
### ������ �ҷ����̱�
load("spssdata.RData")
attach(spssdata)

######### �ܼ�ȸ�ͺм� ##########
# ��������
#  1) �ڱ�ŷڰ��� �ھ����߰��� ������ ��ĥ ���δ�.

regression1_1 <- lm(self.esteem ~ self.confidence, data=spssdata)
summary(regression1_1)

#jtools ��Ű��
library(jtools)
summ(regression1_1, confint=TRUE)
effect_plot(regression1_1, pred = self.confidence, interval = TRUE, 
            plot.points = TRUE, data=spssdata)

#sjPlot ��Ű��
library(sjPlot)
tab_model(regression1_1, show.se=TRUE, show.ci=0.95, show.fstat=TRUE,
          auto.label=FALSE)

#ggplot ��Ű��
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
  scale_x_continuous(name = "�ڱ�ŷڰ�") +
  scale_y_continuous(name = "�ڱ����߰�") +
  annotate("rect", xmin = 3.50, xmax = 8.1, ymin = 27.5, ymax = 29.5, 
           fill="white", colour="red") +
  annotate("text", x = 6.0, y = 28.5, label = equation(regression1_1), parse = TRUE)
p11

library(ggpubr)
formul <- y ~ x
p <- ggplot(spssdata, aes(x=self.confidence, y=self.esteem)) +
  geom_point() +
  geom_smooth(method = "lm", formula=y~x)  +
  scale_x_continuous(name = "�ڱ�ŷڰ�") +
  scale_y_continuous(name = "�ڱ����߰�") +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = formul
  ) +
  theme_bw()
ggpar(p, palette = "jco")

##################################
# ��������
#  2) �θ� ���� ������ �ھ����߰��� ������ ��ĥ ���̴�.

regression1_2 <- lm(self.esteem ~ attachment, data=spssdata)
summary(regression1_2)

#jtools ��Ű��
library(jtools)
summ(regression1_2, confint=TRUE)
effect_plot(regression1_2, pred = attachment, interval = TRUE, 
            plot.points = TRUE, data=spssdata)

install.packages("huxtable")
export_summs(regression1_1, regression1_2, scale=TRUE,
             error_format = "[{conf.low}, {conf.high}]")

#sjPlot ��Ű��
library(sjPlot)
tab_model(regression1_1, regression1_2, show.se=TRUE, show.ci=FALSE, show.fstat=TRUE,
          show.aic=TRUE, auto.label=FALSE)
#ggplot ��Ű��
require(ggplot2)
library(ggpubr)
formul2 <- y ~ x
p2 <- ggplot(spssdata, aes(x=attachment, y=self.esteem)) +
  geom_point() +
  geom_smooth(method = "lm", formula=y~x)  +
  scale_x_continuous(name = "�θ� ���� ����") +
  scale_y_continuous(name = "�ڱ����߰�") +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = formul2
  ) +
  theme_bw()
ggpar(p2, palette = "jco")

########################################################################
######### ����ȸ�ͺм� ##########
# ��������
#  1-1) ������ �ھ����߰��� ������ ��ĥ ���δ�.
#  1-1) �ڱ�ŷڰ��� �ھ����߰��� ������ ��ĥ ���δ�.
#  1-1) �θ� ���� ������ �ھ����߰��� ������ ��ĥ ���δ�.
#  1-1) �θ� ������ �ھ����߰��� ������ ��ĥ ���δ�.
#  1-1) ������ ������ �ھ����߰��� ������ ��ĥ ���δ�.

#���������
spssdata$monitor <- q33a07w1 + q33a08w1 + q33a09w1 + q33a10w1
library(sjlabelled)
spssdata$monitor <- set_label(spssdata$monitor, "�θ� ����")

spssdata$sexw1.re[sexw1 == 1] <- 0
spssdata$sexw1.re[sexw1 == 2] <- 1
spssdata$sexw1.re <- set_label(spssdata$sexw1.re, "����")
spssdata$sexw1.re <- set_labels(spssdata$sexw1.re, labels=c("����","����"))

###### ����ȸ�͸��� ����
regression2 <- lm(self.esteem ~ sexw1.re+self.confidence+attachment+
                    monitor+negative.parenting, data=spssdata)
summary(regression2)

# ǥ��ȭ �����
library(QuantPsyc)
lm.beta(regression2)
round(lm.beta(regression2), 3)

# ���߰����� ����
library(car)
vif(regression2)

#jtools ��Ű��
library(jtools)
summ(regression2, confint=TRUE)
effect_plot(regression2, pred = self.confidence, interval = TRUE, 
            plot.points = TRUE, data=spssdata)

#sjPlot ��Ű��
library(sjPlot)
tab_model(regression2, show.se=TRUE, show.std=TRUE, show.ci=0.95, 
          show.fstat=TRUE, auto.label=FALSE)

set_theme(axis.title.size=1.2, axis.textsize=1.0, title.align="center")
plot_model(regression2, show.values=TRUE, wrap.labels=5, line.size=1.2,
           axis.title="�ھ����߰�")

plot_model(regression2, type="std", show.values=TRUE, wrap.labels=5, 
           axis.title="�ھ����߰�(ǥ��ȭ���)", line.size=1.2)
  # ǥ��ȭȸ�Ͱ��

#lmtest ��Ű�� : ���߰����� ����
library(mctest)
mc.plot(regression2)
imcdiag(regression2)

###dummy variable
spssdata$job.dummy1 <- ifelse(spssdata$q2w1==2, 1, 0)
spssdata$job.dummy2 <- ifelse(spssdata$q2w1==3, 1, 0)

spssdata$job.dummy1 <- set_label(spssdata$job.dummy1, "��������_�밭�� ����")
spssdata$job.dummy2 <- set_label(spssdata$job.dummy2, "��������_�������� ����")

table(spssdata$q2w1)
table(spssdata$job.dummy1)
table(spssdata$job.dummy2)

regression4 <- lm(self.esteem ~ sexw1.re+attachment+monitor+job.dummy1+job.dummy2,
                  data=spssdata)
summary(regression4)

spssdata$q2w1a <- factor(spssdata$q2w1)
spssdata$q2w1a <- set_label(spssdata$q2w1a, "��������")
regression5 <- lm(self.esteem ~ sexw1.re+attachment+monitor+q2w1a,
                  data=spssdata)
summary(regression5)

# ǥ��ȭ �����
tab_model(regression5, show.se=TRUE, show.std=TRUE, show.ci=0.95, 
          show.fstat=TRUE, auto.label=FALSE)


### ��������
spssdata.no.na <- na.omit(spssdata[c("self.esteem","sexw1.re","self.confidence",
                                     "attachment","monitor","negative.parenting")])
regression3 <- lm(self.esteem ~ sexw1.re+self.confidence+attachment+
                    monitor+negative.parenting, data=spssdata.no.na)
null <- lm(self.esteem ~ 1, data=spssdata.no.na)

step1 <- step(null, scope=list(lower=null, upper=regression3), direction="forward")
summary(step1)
  # ��������

step2 <- step(regression3, direction="backward")
summary(step2)
  # ��������

step3 <- step(regression3, direction="both")
summary(step3)
 # ��������

### ��ȣ�ۿ� ����
regression6 <- lm(self.confidence ~ sexw1.re+
                    attachment*monitor+negative.parenting, data=spssdata)
summary(regression6)
vif(regression6) 

set_theme(axis.title.size=1.2, axis.textsize=1.0, legend.title.size=1.2,
          legend.size=1.1, title.align="center", legend.post="bottom")
plot_model(regression6, show.values=TRUE, wrap.labels=5, 
           title="�θ������ �θ𰨵��� ��ȣ�ۿ� ����", 
           axis.title=c("�ڱ�ŷڰ�"))

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
spssdata$delinquency <- set_label(spssdata$delinquency, "û�ҳ� ����")

spssdata$aggression <- q48c1w1+q48c2w1+q48c3w1+q48c4w1+q48c5w1+q48c6w1
spssdata$aggression <- set_label(spssdata$aggression, "���ݼ�")

spssdata$satisfaction <- q50w1
spssdata$satisfaction <- set_label(spssdata$satisfaction, "���� ������")

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

#lmtest ��Ű�� : ���߰����� ����
library(mctest)
mc.plot(regression.f1)
imcdiag(regression.f1)

# ǥ��ȭ �����
tab_model(regression.f1, show.se=TRUE, show.std=TRUE, auto.label=TRUE)

regression.f2 <- lm(self.esteem ~ sexw1.factor+grp.grade.factor+
                     q2w1a+ grp.grade.factor:q2w1a+
                     attachment+ monitor+negative.parenting+self.control+ delinquency+
                     self.confidence+aggression+satisfaction , data=spssdata.na.omit)
summary(regression.f2)
tab_model(regression.f2, show.se=TRUE, show.fstat=TRUE,
          auto.label=TRUE)

# �������� �ĺ�
regression.f <- lm(self.esteem ~ sexw1.factor+grp.grade.factor+
                      q2w1a+ attachment+ monitor+negative.parenting+
                      self.control+ delinquency+
                      self.confidence+aggression+satisfaction , data=spssdata.na.omit)
summary(regression.f)
tab_model(regression.f, show.se=TRUE, show.fstat=TRUE,
          auto.label=TRUE)

#lmtest ��Ű�� : ���߰����� ����
library(mctest)
mc.plot(regression.f)
imcdiag(regression.f)

# ��������
step3 <- step(regression.f, direction="both")
summary(step3)
# ��������

# �������� ��� ����
reg.mod <- lm(self.esteem ~ grp.grade.factor + q2w1a + attachment + 
self.control + self.confidence + aggression + satisfaction, 
data = spssdata.na.omit)

summary(reg.mod)
tab_model(reg.mod, show.se=TRUE, show.fstat=TRUE,
          auto.label=TRUE)

#lmtest ��Ű�� : ���߰����� ����
library(mctest)
mc.plot(reg.mod)
imcdiag(reg.mod)

# ǥ��ȭ �����
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

############################ �����м�
# ����ӽ���跮 : ������ ������
library(car)
durbinWatsonTest(reg.mod)

id <- c(1:nrow(spssdata.na.omit))
resid <- rstandard(reg.mod)
plot(id, resid, main="������ ������", ylab="ǥ��ȭ����",pch=21)

# �����׸� : ������ ���Լ� �� �̻���, ������
pred <- predict(reg.mod)
plot(pred, resid, main="���� vs ���հ�", pch=21, col="red", ylab="ǥ��ȭ����", 
     xlab="���հ�")
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

###### �̻��� ����
remove <- c("444","477","284")
spssdata.out <- spssdata.na.omit[!row.names(spssdata.na.omit)%in%remove,]

# full model
regression.fout <- lm(self.esteem ~ sexw1.factor+grp.grade.factor+ q2w1a+
                     attachment+ monitor+negative.parenting+self.control+ delinquency+
                     self.confidence+aggression+satisfaction , data=spssdata.out)
summary(regression.fout)
tab_model(regression.fout, show.se=TRUE, show.fstat=TRUE,
          auto.label=TRUE)

#lmtest ��Ű�� : ���߰����� ����
library(mctest)
mc.plot(regression.fout)
imcdiag(regression.fout)

# ǥ��ȭ �����
tab_model(regression.fout, show.se=TRUE, show.std=TRUE, auto.label=TRUE)

# ��������
step3out <- step(regression.fout, direction="both")
summary(step3out)
# ��������

# �������� ��� ����
reg.mod2 <- lm(self.esteem ~ grp.grade.factor + attachment + 
                 self.control + self.confidence + aggression + satisfaction, 
               data = spssdata.out)
summary(reg.mod2)
tab_model(reg.mod2, show.se=TRUE, show.fstat=TRUE,
          auto.label=TRUE)

# �������� ���� �߰�
reg.mod3 <- lm(self.esteem ~ grp.grade.factor + q2w1a + attachment + 
                 self.control + self.confidence + aggression + satisfaction, 
               data = spssdata.out)
summary(reg.mod3)
tab_model(reg.mod3, show.se=TRUE, show.fstat=TRUE,
          auto.label=TRUE)

#lmtest ��Ű�� : ���߰����� ����
library(mctest)
mc.plot(reg.mod3)
imcdiag(reg.mod3)

# ǥ��ȭ �����
tab_model(reg.mod3, show.se=TRUE, show.std=TRUE, auto.label=TRUE)

############################ �����м�
# ����ӽ���跮 : ������ ������
library(car)
durbinWatsonTest(reg.mod3)

id3 <- c(1:nrow(spssdata.out))
resid3 <- rstandard(reg.mod3)
plot(id3, resid3, main="������ ������", ylab="ǥ��ȭ����",pch=21)

# �����׸� : ������ ���Լ� �� �̻���, ������
pred3 <- predict(reg.mod3)
plot(pred3, resid3, main="���� vs ���հ�", pch=21, col="red", ylab="ǥ��ȭ����", 
     xlab="���հ�")
abline(0,0)

library(sjPlot)
plot_residuals(reg.mod3)

par(mfrow=c(3,2))
for(i in 1:5) plot(reg.mod3, i)
par(mfrow=c(1,1))


##################################################################
###### �̻��� ����
remove2 <- c("865","1000","1486")
spssdata.out2 <- spssdata.out[!row.names(spssdata.out)%in%remove2,]

# full model
regression.fout2 <- lm(self.esteem ~ sexw1.factor+grp.grade.factor+ q2w1a+
                        attachment+ monitor+negative.parenting+self.control+ delinquency+
                        self.confidence+aggression+satisfaction , data=spssdata.out2)
summary(regression.fout2)
tab_model(regression.fout2, show.se=TRUE, show.fstat=TRUE,
          auto.label=TRUE)

#lmtest ��Ű�� : ���߰����� ����
library(mctest)
mc.plot(regression.fout2)
imcdiag(regression.fout2)

# ǥ��ȭ �����
tab_model(regression.fout2, show.se=TRUE, show.std=TRUE, auto.label=TRUE)

# ��������
step3out2 <- step(regression.fout2, direction="both")
summary(step3out2)
# ��������

# �������� ��� ����
reg.mod4 <- lm(self.esteem ~ grp.grade.factor + q2w1a + attachment + negative.parenting +
                 self.control + self.confidence + aggression + satisfaction, 
               data = spssdata.out2)
summary(reg.mod4)
tab_model(reg.mod4, show.se=TRUE, show.fstat=TRUE,
          auto.label=TRUE)

#lmtest ��Ű�� : ���߰����� ����
library(mctest)
mc.plot(reg.mod4)
imcdiag(reg.mod4)

# ǥ��ȭ �����
tab_model(reg.mod4, show.se=TRUE, show.std=TRUE, auto.label=TRUE)

############################ �����м�
# ����ӽ���跮 : ������ ������
library(car)
durbinWatsonTest(reg.mod4)

id4 <- c(1:nrow(spssdata.out2))
resid4 <- rstandard(reg.mod4)
plot(id4, resid4, main="������ ������", ylab="ǥ��ȭ����",pch=21)

# �����׸� : ������ ���Լ� �� �̻���, ������
pred4 <- predict(reg.mod4)
plot(pred4, resid4, main="���� vs ���հ�", pch=21, col="red", ylab="ǥ��ȭ����", 
     xlab="���հ�")
abline(0,0)

library(sjPlot)
plot_residuals(reg.mod4)

par(mfrow=c(3,2))
for(i in 1:5) plot(reg.mod4, i)
par(mfrow=c(1,1))

### �������� : �̻��� 6�� ���� �� ������ ����
reg.final <- lm(self.esteem ~ grp.grade.factor + q2w1a + attachment + negative.parenting +
                 self.control + self.confidence + aggression + satisfaction, 
               data = spssdata.out2)
tab_model(reg.final, show.se=TRUE, show.fstat=TRUE,
          auto.label=TRUE)
# ǥ��ȭ �����
tab_model(reg.final, show.se=TRUE, show.std=TRUE, auto.label=TRUE)