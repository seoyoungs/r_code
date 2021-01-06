setwd("C:/Users/�輭��/Desktop/����/2020��2�б�/��ȸ�����ڷ�/��2 �г� ������(SPSS)")
#install.packages("Hmisc")
library(Hmisc)
test<-spss.get("C:/Users/�輭��/Desktop/����/2020��2�б�/��ȸ�����ڷ�/��2 �г� ������(SPSS)/04-1 ��2 �г� 1���⵵ ������(SPSS).sav",
               use.value.labels=FALSE)
#names(test)
select_variables <-c()
select_variables <- c(1,4,9,10,19,101,102,103,328:337,339:348,372,375,377,379,
                      484:489, 496:504, 532)
test1 <- test[select_variables]
#test1 <- test[,select_variables]
spssdata <- test1[which(test1$scharew1 >= 100 & test1$scharew1 <200), ]

#######################################################################################
#install.packages("sjmisc")
#install.packages("sjlabelled")
library(sjmisc)
library(sjlabelled)
test.lables<-spss.get("C:/Users/�輭��/Desktop/����/2020��2�б�/��ȸ�����ڷ�/��2 �г� ������(SPSS)/04-1 ��2 �г� 1���⵵ ������(SPSS).sav",
                      use.value.labels=TRUE)
test.lables1<-test.lables[select_variables]
spssdata1<-test.lables[which(as.numeric(test.lables1$scharew1)<26),]
labels.spss.values<-get_labels(spssdata1)
spssdata1 <- set_labels(spssdata, labels=labels.spss.values, 
                        force.values=FALSE, force.labels=TRUE)

##############################################################################################

# ��������
attach(spssdata)
spssdata$attachment = q33a01w1+q33a02w1+q33a03w1+q33a04w1+q33a05w1+q33a06w1
#spssdata <- transform(spssdata, attachment=q33a01w1+q33a02w1+q33a03w1+q33a04w1+q33a05w1+q33a06w1)
spssdata$grade = q18a1w1+q18a2w1+q18a3w1
#detach(spssdata)

# �б����� ������ 3�������� �з�
#install.packages("epiDisplay")
library(epiDisplay)
tab1(spssdata$grade, cum.percent = TRUE)
#attach(spssdata)
spssdata$grp.grade[grade>= min(grade) & grade <= 8] <- 1
spssdata$grp.grade[grade>= 9 & grade <= 10] <- 2
spssdata$grp.grade[grade>= 11 & grade <= max(grade)] <- 3
#detach(spssdata)
tab1(spssdata$grp.grade, cum.percent = TRUE)

# q50w1�� �Ӽ��� ����/�߸�/��������
#attach(spssdata)
spssdata$satisfaction[q50w1==1|q50w1==2] <- 1 #�������� ���ϴ� ��
spssdata$satisfaction[q50w1==3] <- 2          #����
spssdata$satisfaction[q50w1==4|q50w1==5] <- 3 #�����ϴ� ��
#detach(spssdata)

# ������ �ڵ�
#attach(spssdata)
spssdata$rq48a04w1[q48a04w1==1] <- 5
spssdata$rq48a04w1[q48a04w1==2] <- 4
spssdata$rq48a04w1[q48a04w1==3] <- 3
spssdata$rq48a04w1[q48a04w1==4] <- 2
spssdata$rq48a04w1[q48a04w1==5] <- 1

spssdata$rq48a05w1[q48a05w1==1] <- 5
spssdata$rq48a05w1[q48a05w1==2] <- 4
spssdata$rq48a05w1[q48a05w1==3] <- 3
spssdata$rq48a05w1[q48a05w1==4] <- 2
spssdata$rq48a05w1[q48a05w1==5] <- 1

spssdata$rq48a06w1[q48a06w1==1] <- 5
spssdata$rq48a06w1[q48a06w1==2] <- 4
spssdata$rq48a06w1[q48a06w1==3] <- 3
spssdata$rq48a06w1[q48a06w1==4] <- 2
spssdata$rq48a06w1[q48a06w1==5] <- 1
#detach(spssdata)

# �� ������ �������� �������� �ϳ��� ���ܺ��� �����
#attach(spssdata)
spssdata$grp.sex.grade[sexw1==1 & grp.grade==1] <- 11
spssdata$grp.sex.grade[sexw1==1 & grp.grade==2] <- 12
spssdata$grp.sex.grade[sexw1==1 & grp.grade==3] <- 13
spssdata$grp.sex.grade[sexw1==2 & grp.grade==1] <- 21
spssdata$grp.sex.grade[sexw1==2 & grp.grade==2] <- 22
spssdata$grp.sex.grade[sexw1==2 & grp.grade==3] <- 23
#detach(spssdata)

# �������� NA�� ����
q33a07w1
table(q33a07w1)
table(q33a07w1, useNA="ifany")

# ������ �����ϱ�
save(spssdata, file="spssdata.RData")
# workspace �����ϱ� 
save.image(file="myspssdata.RData")
#####################
# ������ �����ϱ� 
second<-spss.get("C:/Users/�輭��/Desktop/����/2020��2�б�/��ȸ�����ڷ�/��2 �г� ������(SPSS)/04-2 ��2 �г� 2���⵵ ������(SPSS).sav",
               use.value.labels=FALSE)
mergedata <- merge(spssdata, second, by="id")

#####################
# ������ �����ϱ� 
newdata <- subset(test, scharew1 >= 100 & scharew1 < 200, 
                  select=c("id", "sexw1", "scharew1", "areaw1", "q2w1", "q18a1w1", 
                           "q18a2w1", "q18a3w1"))
