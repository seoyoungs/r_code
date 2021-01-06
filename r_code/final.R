setwd("C:/Users/�輭��/Desktop/����/2020��2�б�/��ȸ�����ڷ�/��2 �г� ������(SPSS)")
load("shsans.RData")
attach(test1)

########################################################################
######### ����ȸ�ͺм� ##########
# ��������
#  1-1) ������ �ھ����߰��� ������ ��ĥ ���δ�.
#  1-1) �ڱ�ŷڰ��� �ھ����߰��� ������ ��ĥ ���δ�.
#  1-1) �θ� ���� ������ �ھ����߰��� ������ ��ĥ ���δ�.
#  1-1) �θ� ������ �ھ����߰��� ������ ��ĥ ���δ�.
#  1-1) ������ ������ �ھ����߰��� ������ ��ĥ ���δ�.

#���������
library(sjlabelled)

test1$sexw1.re[sexw2 == 1] <- 0
test1$sexw1.re[sexw2 == 2] <- 1
test1$sexw1.re <- set_label(test1$sexw1.re, "����")
test1$sexw1.re <- set_labels(test1$sexw1.re, labels=c("����","����"))
test1$major <- q50w2+q29w2+q48a01w2+q48a02w2+q48a03w2
library(sjlabelled)
test1$major  <- set_label(test1$major, "�������� ������")

###### ����ȸ�͸��� ����
regression2 <- lm(major ~ sexw1.re+nowsi+score+
                  tru+fri2+mind:schlife+friend:tru+friend:paren
                    , data=test1)
summary(regression2)


library(car)
durbinWatsonTest(regression2)
id3 <- c(1:nrow(test1))
resid3 <- rstandard(regression2)
plot(id3, resid3, main="������ ������", ylab="ǥ��ȭ����",pch=21)












