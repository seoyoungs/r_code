setwd("C:/Users/�輭��/Desktop/����/2020��2�б�/��ȸ�����ڷ�/��2 �г� ������(SPSS)")
load("shsans.RData")
attach(test1)

#-------------------------��ǥ
#library(Hmisc)
describe(sexw2, na.rm=T)
describe(scharew2, na.rm=T)
describe(areuw2, na.rm=T)

################-----------������ T����
##�������� �������� ���� ���� ����� ���� ���� ��ġ��
test1$major <- q50w2+q29w2+q48a01w2+q48a02w2+q48a03w2
library(sjlabelled)
test1$major  <- set_label(test1$major, "�������� ������")
#--------------------���� ����������
#������ ������ ��ȯ
test1$sexw2a <- factor(test1$sexw2, levels=c(1,2), labels=c("����","����"))
## 2. �л� ������ ����
var.test(major ~ sexw2, data=test1)
var.test(major ~ sexw2a, data=test1)
## 3. �������� ����
t.test(major ~ sexw2, var.equal=TRUE, data=test1)
t.test(major ~ sexw2a, var.equal=TRUE, data=test1)
## 4. ���ܿ� ���� ��� ��跮
library(psych)
describeBy(test1$major, group=test1$sexw2a)

#--------------������ȹ ����������
#������ ������ ��ȯ
test1$q24w2a <- factor(test1$q24w2, levels=c(1,2), labels=c("�ִ�","����"))
## 2. �л� ������ ����
var.test(major ~ q24w2a, data=test1)
## 3. �������� ����
t.test(major ~ q24w2a, var.equal=TRUE, data=test1)
## 4. ���ܿ� ���� ��� ��跮
#library(psych)
describeBy(test1$major, group=test1$q24w2a)

###################################----�Ͽ��л�м�
#-------------------------������
# ��������
test1$areuw2a <- set_label(test1$areuw2, "������")
# ������ ����
test1$areuw2a <- set_labels(test1$areuw2,levels=c(1,2,3), labels=c("��","��","��"))
# ������ ������ ��ȯ
test1$areuw2aa <- factor(test1$areuw2, levels=c(1,2,3), labels=c("��","��","��"))
#������ �л� ������ ����
#install.packages("car")
library(car)
leveneTest(major ~ areuw2aa, data=test1)
#���� �Ͽ��л�м�
ano1 <- aov(major ~ areuw2aa, data=test1)
anova(ano1)
# ���ܺ� ��հ�
#library(psych)
describeBy(test1$major, group=test1$areuw2aa)

#---------------------------��ü�� ������ȹ ����
# ��������
#install.packages("lawstat")


test1$q2w2a <- set_label(test1$q2w2, "��ü���� ����(â��)��ȹ ����")
# ������ ����
test1$q2w2a <- set_labels(test1$q2w2,levels=c(1,2,3), labels=c("��ü������ Ȯ���� ���� ���� �ִ�","�밭 ������ ���� �����ִ�","���� ���س��� �巡�� ������ ����"))
# ������ ������ ��ȯ
test1$q2w2aa <- factor(test1$q2w2, levels=c(1,2,3), labels=c("��ü������ Ȯ���� ���� ���� �ִ�","�밭 ������ ���� �����ִ�","���� ���س��� �巡�� ������ ����"))
#������ ����
leveneTest(major ~ q2w2aa, data=test1, center=mean)
#������ �Ͽ��л�м�
ano1 <- aov(major ~ q2w2aa, data=test1)
anova(ano1)

# ���ܺ� ��հ�
#library(psych)
describeBy(test1$major, group=test1$q2w2aa)

###################----------�̿���ġ �л� �м�
#----------(�������ÿ� ���� �����Ȳ& �б���Ȱ ����)
ano1a <- aov(major ~ careerch + nowsi + 
               careerch:nowsi , data=test1)
anova(ano1a)
#----------(�������ÿ� ���� �����Ȳ&�б� ������ ������� #ȿ������-�ֿ����)
ano2a <- aov(major ~ careerch + score + 
               careerch:score , data=test1)
anova(ano2a)
#-------------�������ÿ� ���� �����Ȳ&���� ��������
ano3a <- aov(major ~ careerch + mind + 
               careerch:mind , data=test1)
anova(ano3a)
#-------------�������ÿ� ���� �����Ȳ&�б� �����԰��� ����
ano4a <- aov(major ~ careerch + schlife + 
               careerch:schlife , data=test1)
anova(ano4a)
#-------------�������ÿ� ���� �����Ȳ&�ſ� ģ�� ģ������ ����
ano5a <- aov(major ~ careerch + friend + 
               careerch:friend , data=test1)
anova(ano5a)
#-------------�������ÿ� ���� �����Ȳ&�ڽſ� ���� �ŷ�
ano6a <- aov(major ~ careerch + tru + 
               careerch:tru , data=test1)
anova(ano6a)
#-------------�������ÿ� ���� �����Ȳ&�θ�԰��� ������ �޴� #��Ʈ����
ano7a <- aov(major ~ careerch + paren + 
               careerch:paren , data=test1)
anova(ano7a)
#-------------�������ÿ� ���� �����Ȳ&�ܸ� ���� �Ҹ�
ano8a <- aov(major ~ careerch + fri2 + 
               careerch:fri2 , data=test1)
anova(ano8a)
#------------�б���Ȱ����&�б� ������ ������� ȿ������ (�ֿ����)
ano9a <- aov(major ~ nowsi + score + 
               nowsi:score , data=test1)
anova(ano9a)
#------------�б���Ȱ����&���� ��������
ano10a <- aov(major ~ nowsi + mind + 
               nowsi:mind , data=test1)
anova(ano10a)
#------------�б���Ȱ����&�б� �����԰��� ����
ano11a <- aov(major ~ nowsi + schlife + 
                nowsi:schlife , data=test1)
anova(ano11a)
#------------�б���Ȱ����&�ſ� ģ�� ģ������ ����
ano12a <- aov(major ~ nowsi + friend + 
                nowsi:friend , data=test1)
anova(ano12a)
#------------�б���Ȱ����&�ڽſ� ���� �ŷڵ�
ano13a <- aov(major ~ nowsi + tru + 
                nowsi:tru , data=test1)
anova(ano13a)
#------------�б���Ȱ����&�θ�԰��� �������� �޴� ��Ʈ����
ano14a <- aov(major ~ nowsi + paren + 
                nowsi:paren , data=test1)
anova(ano14a)
#------------�б���Ȱ����&�ܸ� ���� �Ҹ�
ano15a <- aov(major ~ nowsi + fri2 + 
                nowsi:fri2 , data=test1)
anova(ano15a)
#-----------�б� ������ ������� ȿ������ (�ֿ����)&���� �������� 
ano16a <- aov(major ~ score + mind + 
                score:mind , data=test1)
anova(ano16a)
#-----------�б������� ������� ȿ������ (�ֿ����)&�б� �����԰��� ����
ano17a <- aov(major ~ score + schlife + 
                score:schlife , data=test1)
anova(ano17a)
#-----------�б� ������ ������� ȿ������ (�ֿ����)&�ſ� ģ�� ģ������ ����
ano18a <- aov(major ~ score + friend + 
                score:friend , data=test1)
anova(ano18a)
#-----------�б� ������ ������� ȿ������ (�ֿ����)&�ڽſ� ���� �ŷڵ�
ano19a <- aov(major ~ score + tru + 
                score:tru , data=test1)
anova(ano19a)
#-----------�б� ������ ������� ȿ������ (�ֿ����) &�θ�԰��� �������� �޴� ��Ʈ����
ano20a <- aov(major ~ score + paren + 
                score:paren , data=test1)
anova(ano20a)
#-----------�б� ������ ������� ȿ������ (�ֿ����)&�ܸ� ���� �Ҹ�
ano21a <- aov(major ~ score + fri2 + 
                score:fri2 , data=test1)
anova(ano21a)
#----------���� ��������&�б� �����԰��� ����
ano22a <- aov(major ~ mind + schlife + 
                mind:schlife , data=test1)
anova(ano22a)
#----------���� ��������&�ſ� ģ�� ģ������ ����
ano23a <- aov(major ~ mind + friend + 
                mind:friend , data=test1)
anova(ano23a)
#----------���� ��������&�ڽſ� ���� �ŷڵ�
ano24a <- aov(major ~ mind + tru + 
                mind:tru , data=test1)
anova(ano24a)
#----------���� ��������&�θ�԰��� �������� �޴� ��Ʈ����
ano25a <- aov(major ~ mind + paren + 
                mind:paren , data=test1)
anova(ano25a)
#----------���� ��������&�ܸ� ���� �Ҹ� 
ano26a <- aov(major ~ mind + fri2 + 
                mind:fri2, data=test1)
anova(ano26a)
#-----------�б� �����԰��� ����&�ſ� ģ�� ģ������ ���� 
ano27a <- aov(major ~ schlife + friend + 
                schlife:friend, data=test1)
anova(ano27a)
#-----------�б� �����԰��� ����&�ڽſ� ���� �ŷڵ�
ano28a <- aov(major ~ schlife + tru + 
                schlife:tru, data=test1)
anova(ano28a)
#-----------�б� �����԰��� ����&�θ�԰��� �������� �޴� ��Ʈ����
ano29a <- aov(major ~ schlife + paren + 
                schlife:paren, data=test1)
anova(ano29a)
#-----------�б� �����԰��� ����& �ܸ� ���� �Ҹ�
ano30a <- aov(major ~ schlife + fri2 + 
                schlife:fri2, data=test1)
anova(ano30a)
#-----------�ſ� ģ�� ģ������ ����&�ڽſ� ���� �ŷڵ�
ano31a <- aov(major ~ friend + tru + 
                friend:tru, data=test1)
anova(ano31a)
#-----------�ſ� ģ�� ģ������ ����&�θ�԰��� �������� �޴� ��Ʈ����
ano32a <- aov(major ~ friend + paren + 
                friend:paren, data=test1)
anova(ano32a)
#-----------�ſ� ģ�� ģ������ ����&�ܸ� ���� �Ҹ�
ano33a <- aov(major ~ friend + fri2 + 
                friend:fri2, data=test1)
anova(ano33a)
#-----------�ڽſ� ���� �ŷڵ�&�θ�԰��� �������� �޴� ��Ʈ����
ano34a <- aov(major ~ tru + paren + 
                tru:paren, data=test1)
anova(ano34a)
#-----------�ڽſ� ���� �ŷڵ�&�ܸ� ���� �Ҹ�
ano35a <- aov(major ~ tru + fri2 + 
                tru:fri2, data=test1)
anova(ano35a)
#----------�θ�԰��� �������� �޴� ��Ʈ����&�ܸ� ���� �Ҹ�
ano36a <- aov(major ~ paren + fri2 + 
                paren:fri2, data=test1)
anova(ano36a)


##########--����м�
#### sjPlot ��Ű���� �̿��� ����м�
library(sjPlot)
library(sjlabelled)
test1$careerch <-set_label(test1$careerch, "���� ���ÿ� ���� ���� ��Ȳ")
test1$nowsi<-set_label(test1$nowsi,"�б� ��Ȱ ����")
test1$score<-set_label(test1$score, "�б� ��Ȱ ����")
test1$mind<-set_label(test1$mind,"���� ��������")
test1$schlife<-set_label(test1$schlife,"�б� �����԰��� ����")
test1$friend<-set_label(test1$friend,"�ſ� ģ�� ģ������ ����")
test1$tru<-set_label(test1$tru,"�ڽſ� ���� �ŷڵ�")
test1$paren<-set_label(test1$paren,"�θ�԰��� �������� �޴� ��Ʈ����")
test1$fri2<-set_label(test1$fri2,"�ܸ� ���� �Ҹ� ")
cor.var <- test1[c("major","careerch","nowsi","score","mind","schlife","friend","tru","paren","fri2")]
sjp.corr(cor.var, corr.method="pearson", na.deletion="pairwise", p.numeric=TRUE)


