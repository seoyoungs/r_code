setwd("C:/Users/�輭��/Desktop/����/2020��2�б�/��ȸ�����ڷ�/��2 �г� ������(SPSS)")
### ������ �ҷ����̱�
load("spssdata.RData")
attach(spssdata)

#####################################################################
##��ǥ ���: ���� ������
table(q50w1) 
library(plyr) #��ǥ ���� �Լ�
#names(spssdata)
count(spssdata, "q50w1")

#cbind �м���� ���� �Լ�(��), ���� rbind
cbind(Freq=table(q50w1),           #��
      Cumul=cumsum(table(q50w1)),  #������
      Relative=prop.table(table(q50w1)), #�󵵺� ����
      Cum.prop = cumsum(prop.table(table(q50w1)))) #��������
###########cbind ���ΰ�
tab = table(q50w1)
cbind(Freq=tab, Cumul=cumsum(tab), Relative=prop.table(tab), Cum.prop=cumsum(prop.table(tab)))
###########�󵵺� �ۼ�Ʈ�� �ٲٱ� �Ҽ��� 3�ڸ�����
count(spssdata, "scharew1") #�ڵ�Ϻ��� �տ� ��ȣ ������� ����
cbind(Freq=table(q50w1),
      percentage=100*prop.table(table(q50w1)),
      relative=round(100*prop.table(table(q50w1)),3))

#####�Ǵٸ� ��ǥ����Լ� describe: �̰� �� ����(����ġ��)######
library(Hmisc)
describe(q50w1)
#####
# summarytools ��Ű���� �̿��� ��ǥ ���
##
#install.packages("summarytools")
library(summarytools)
view(freq(q50w1))
freq(q50w1, plain.ascii=FALSE, style="rmarkdown")

# ����: freq(��)�� % Valid(������ �� ����),  % Total(������ ���� ����)


#####################################################################
##�����跮 ��� 
summary(q50w1)
#���⼭ 1st Qu 3.000 �� 1�������: 3���� �۰� ������ο����� ��ü 595���� 25%��� ��
str(q50w1)
library(psych)
describe(q50w1) #�տ��� �� describe���� �ٸ���
# trimmed �������
#mad: sd������ ���� skew�ֵ�, se ǥ�ؿ���, range=�ִ�-�ּ�


var1 <- c("q33a01w1", "q50w1")
tab1 <- spssdata[var1]
library(summarytools)
freq(tab1)
#���ΰ� ������ ��� �Լ�
library(psych)
describe(tab1)
#�� �ΰ� ������ ����Լ�

library(sjmisc)
descr(tab1, encoding="EUC-KR", out="viewer")

#������� ǥ �����
#####################################################################
##���� ������ ��ǥ�׸��� 
lab.val <- c("���� �������� ���Ѵ�", "�������� ���ϴ� ���̴�", "�����̴�", 
             "�����ϴ� ���̴�", "�ſ� �����Ѵ�")
# �� ��ǥ
par(mfrow=c(2,1))
barplot(table(q50w1),  #��
        names.arg=lab.val,
        space=1.5, border=NA, cex.names=0.7, ylim=c(0, 350))

barplot(prop.table(table(q50w1)), #����
        names.arg=lab.val,
        space=1.5, border=NA, cex.names=0.7, ylim=c(0, 1))

par(mfrow=c(1,1))

library(RColorBrewer) #����
display.brewer.all()
display.brewer.pal(5, "Set2")
pal1 <- brewer.pal(5, "Set2")
barplot(prop.table(table(q50w1)), col=pal1, 
        names.arg=lab.val,
        space=1.5, border=NA, cex.names=0.7, ylim=c(0, 1))

library(sjPlot)
plot_frq(q50w1)
#�ɼ� �����ϱ� (���� ���ص� ��, geom.size �� ����������)
plot_frq(q50w1, geom.size=0.3, ylim=c(0,350), geom.colors="grey47", title="",) 

tab.val <- factor(q50w1, levels=c(1:5), labels=lab.val)
plot_frq(tab.val, axis.title="���� ������", geom.colors="red", geom.size=0.5)

# ������׷� ������112p
#�������� ����
spssdata$attachment <- q33a01w1 + q33a02w1 + q33a03w1 + q33a04w1 + q33a05w1 + q33a06w1
#attach(spssdata)

library(psych)
describe(attachment)
#des���� ���� ��� ������
par(mfrow=c(1,2))
hist(attachment)#������� ��Ī������
library(RColorBrewer) #����
pal1 <- brewer.pal(7, "Set2")
hist(attachment, breaks=20, col=pal1) # breaks=20:������ 20���� �߶��
par(mfrow=c(1,1))

par(mfrow=c(1,2))
plot(density(attachment, na.rm=TRUE))
d <- density(attachment, na.rm=TRUE)
plot(d)
polygon(d, col="red", border="blue") #������׷� ������ ǥ���Ѱ�
par(mfrow=c(1,1))

####
hist(attachment, color="blue", border="black", prob = TRUE, main="�θ� ���� ����")
lines(density(attachment, na.rm=TRUE), lwd = 2, col="red")
####

# ���� ��ǥ-�̰� ����������(�������� �Ⱦ�)
tab <- table(q50w1)
pie(tab, labels=lab.val)

pct <- round(100*prop.table(tab), 1)
lbls <- paste(lab.val, pct)
lbls <- paste(lbls, "%")
pie(pct, labels=lbls, col=rainbow(5),
    main="50. �л��� �л��� � ���������� �󸶳� �����ϰ� �ֽ��ϱ�?",
    init.angle=180, radius=1.0)

#3D ���̵�ǥ
install.packages("plotrix")
library(plotrix)
pie3D(pct, labels = lbls, col=rainbow(5),
      main="50. �л��� �л��� � ���������� �󸶳� �����ϰ� �ֽ��ϱ�?",
      labelcex=1.1, explode = 0.1, theta=1.1, shade=0.3)

library(RColorBrewer) #����
pal1 <- brewer.pal(5, "Set2")
pie3D(pct, labels = lbls, col=pal1,
      main="50. �л��� �л��� � ���������� �󸶳� �����ϰ� �ֽ��ϱ�?",
      labelcex=1.1, explode = 0.1, theta=1.1, shade=0.3)