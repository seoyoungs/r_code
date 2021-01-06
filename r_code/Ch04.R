setwd("C:/Users/�輭��/Desktop/����/2020��2�б�/��ȸ�����ڷ�/��2 �г� ������(SPSS)")
### ������ �ҷ����̱�
load("spssdata.RData")
attach(spssdata)

table(q50w1) #���������� ������(1,5�� �׷��� �ٸ��Ͱ� ��ĥ����-����������)
#####################################################################
#######�����м�
### table �Լ� �̿�--->1,2,3,4,5�� ���ο� ������ ����
spssdata$satisfaction[q50w1==1|q50w1==2] <- 1 #�������� ���ϴ� ���̴�
spssdata$satisfaction[q50w1==3] <-2           #����
spssdata$satisfaction[q50w1==4|q50w1==5] <- 3 #�����ϴ� ��

library(sjlabelled)
table(spssdata$satisfaction) #���Ӱ� �����ϼ�
spssdata$satisfaction <- set_labels(spssdata$satisfaction, 
                                     labels=c("�������� ���ϴ� ��", "����", "�����ϴ� ��"))
#����ǥ
devi02_1.re <- table(spssdata$satisfaction, spssdata$sexw1)
devi02_1.re #������ ���� ������

#������ ���� ����ǥ
attach(spssdata)
spssdata$satisfaction_1[satisfaction==1] <- "1_�������� ���ϴ� ��"
spssdata$satisfaction_1[satisfaction==2] <- "2_����"
spssdata$satisfaction_1[satisfaction==3] <- "3_�����ϴ� ��"
table(spssdata$satisfaction_1)
spssdata$sexw1_1[sexw1==1] <- "1_����"
spssdata$sexw1_1[sexw1==2] <- "2_����"
detach(spssdata)

devi02.re <- table(spssdata$satisfaction_1, spssdata$sexw1_1)
devi02.re

#�ֺ���
margin.table(devi02.re) # ��ü�հ�
margin.table(devi02.re, 1) #satisfaction_1 ������ �ֺ��հ�
margin.table(devi02.re, 2) #sexw1_1 ������ �ֺ��հ�

#�����
round(prop.table(devi02.re)*100, 2)  #���� ���� ����
round(prop.table(devi02.re, 2)*100, 2)   #���� ���� ����

## ������ ��Ȱ�������� ������ ����
# H0 : ������ ���� �������� ��Ȱ�������� ���̰� ����. (������ ��Ȱ�������� �����ϴ�.)
# H1 : ������ ���� �������� ��Ȱ�������� ���̰� �ִ�. (������ ��Ȱ�������� ������ �ƴϴ�.)
chisq.test(devi02.re, correct=FALSE)
# p-���� 1.428*10^(-5) <0.05 �͹����� �Ⱒ�Ѵ�. 
# �� ���Ǽ��� 0.05���� ������ ���� ��Ȱ�������� ������ ���̰� �ִٰ� �� �� �ִ�. 


### gmodels ��Ű��
#install.packages("gmodels")
library(gmodels)
CrossTable(spssdata$satisfaction_1, spssdata$sexw1_1, digits=2,
           prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE,
           chisq=TRUE)

table(areaw1) #������ ���� �������м�

### sjplot ��Ű��
library(sjlabelled)
spssdata$sexw1<- set_labels(spssdata$sexw1, 
                                    labels=c("����", "����"))

library(sjPlot)
tab_xtab(spssdata$satisfaction, spssdata$sexw1, show.col.prc=TRUE, 
         var.labels=c("������ ��Ȱ������", "����"), encoding="UTF-8")

#�� view�� �Ⱥ��̹Ƿ� �� �ؿ� �ɼ� ���
tab_xtab(spssdata$satisfaction, spssdata$sexw1, show.col.prc=TRUE, 
         var.labels=c("������ ��Ȱ������", "����"), encoding="EUC-KR",
         file="satissex.html") #working directory�� ����

#��ǥ���
set_theme(geom.label.size=4.5, axis.textsize=1.1, legend.pos="bottom")
plot_xtab(spssdata$satisfaction, spssdata$sexw1, type="bar", y.offset=0.01,
          margin="col", coord.flip=TRUE, wrap.labels=7, geom.colors="Set2",
          show.summary=TRUE)


