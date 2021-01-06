setwd("D:/Work/Class/Survey Data Analysis/��ȸ���� ���м�/�ǽ�")
### ������ �ҷ����̱�
load("spssdata.RData")
attach(spssdata)

####���������
library(sjlabelled)
# ����������
spssdata$negative.parenting <- q33a12w1+q33a13w1+q33a14w1+q33a15w1
spssdata$negative.parenting <- set_label(spssdata$negative.parenting, "����������")
# �ڱ�ŷڰ�
spssdata$self.confidence <- q48b1w1+q48b2w1+q48b3w1
spssdata$self.confidence <- set_label(spssdata$self.confidence, "�ڱ�ŷڰ�")
# ���ݼ�
spssdata$aggressive <- q48c1w1+q48c2w1+q48c3w1+q48c4w1+q48c5w1+q48c6w1
spssdata$aggressive <- set_label(spssdata$aggressive, "���ݼ�")

attach(spssdata)

#### cor �Լ��� �̿��� ����м�
cor(spssdata[c("attachment","negative.parenting","self.control","self.esteem",
               "self.confidence","aggressive")], use="pairwise.complete.obs")
round(cor(spssdata[c("attachment","negative.parenting","self.control",
                     "self.esteem","self.confidence","aggressive")], 
          use="pairwise.complete.obs"),2)
  #�������� �Ҽ��� �ڸ� ����

#�м��Ϸ��� �������� ��ü�� ����� �м��ϴ� ���
cor.var <- spssdata[c("attachment","negative.parenting","self.control",
                      "self.esteem","self.confidence","aggressive")]
cor(cor.var, use="pairwise.complete.obs")
round(cor(cor.var, use="pairwise.complete.obs"),2) #�Ҽ����ڸ�

##�� ���� ���� �������� ���ǵ� ���
cor.test(attachment, self.confidence)

#### psych ��Ű���� �̿��� ����м�
library(psych)
cor.var <- spssdata[c("attachment","negative.parenting","self.control",
                      "self.esteem","self.confidence","aggressive")]
corr.test(cor.var, method="pearson", use="pairwise.complete.obs", adjust="none")
   #��纯�� ���� �������� ���ǵ� ���

#### sjPlot ��Ű���� �̿��� ����м�
library(sjPlot)
cor.var <- spssdata[c("attachment","negative.parenting","self.control",
                      "self.esteem","self.confidence","aggressive")]
tab_corr(cor.var, corr.method="pearson", na.deletion="pairwise", 
         p.numeric=TRUE, triangle="lower", encoding="EUC-KR")
  #����м� ǥ

set_theme(axis.textsize=1.0)
sjp.corr(cor.var, corr.method="pearson", wrap.labels=5, na.deletion="pairwise")
  #����м� ��ǥ


#####################################################################################
# Spearman's Rank Correlation and Kendall's Tau
library(sjlabelled)
spssdata$q50w1 <- set_label(spssdata$q50w1, "���Ǹ�����")

cor.var2 <- spssdata[c("grp.grade","attachment","negative.parenting","self.control",
                      "self.esteem","self.confidence","aggressive","q50w1")]
  #������ ���� : grp.grade(�����׷�) & q50w1(���� ������)

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
### ������м� : �ڱ��������� �렫�� �� 
#                �θ� ���� ������ ���ݼ� ������ ���谡 ���� ���̴�.

## psych ��Ű�� ���
cor.var3 <- spssdata[c("attachment","aggressive","self.confidence",
                       "self.control","negative.parenting")]
cor1 <- cor(cor.var3, use="pairwise.complete.obs")

library(psych)
partial.r(cor1, c(1,2), 4) #��������


## ggm ��Ű�� ���
examdata1 <- spssdata[c("attachment","aggressive","self.control")]
install.packages("ggm")
library(ggm)
pcor(c("attachment","aggressive","self.control"), var(examdata1, na.rm=TRUE))
  #�ڱ��������� ����� ���¿��� �θ� ���� ������ ���ݼ� ���� ��������
pcor.test(pcor(c("attachment","aggressive","self.control"), 
               var(examdata1, na.rm=TRUE)), 1, n=nrow(examdata1))

pcor1 <- var(examdata1, na.rm=TRUE)
parcor(pcor1)

## �ڱ�ŷڰ� ����, ���ݼ��� �ڱ��������� ��������
examdata2 <- spssdata[c("aggressive","self.control","self.confidence")]
pcor(c("aggressive","self.control","self.confidence"), var(examdata2, na.rm=TRUE))
pcor.test(pcor(c("aggressive","self.control","self.confidence"), 
               var(examdata2, na.rm=TRUE)), 1, n=nrow(examdata2))
pcor2 <- var(examdata2, na.rm=TRUE)
parcor(pcor2)