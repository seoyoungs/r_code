setwd("C:/Users/�輭��/Desktop/����/2020��2�б�/��ȸ�����ڷ�/��2 �г� ������(SPSS)")
### ������ �ҷ����̱�
load("spssdata.RData")

### ������ �����- 33�� �� 12���� ����
myvar <- c("q33a01w1", "q33a02w1", "q33a03w1", "q33a04w1", "q33a05w1",
           "q33a06w1", "q33a07w1", "q33a08w1", "q33a09w1", "q33a10w1"  )
##������ �л� 589���� ���� �����Ѱ� PCA1�� �̸� ����
PCA1   <- spssdata[myvar]
# names(spssdata)
# PCA1 <- spssdata[,c(9:18)]
PCA1_1 <- na.omit(PCA1) #������ ����

#################### ���κм� #####################
### Factor�� �� ���� : �ּ��км�(princomp)
fit1  <- princomp(PCA1_1, cor=TRUE)
summary(fit1)
plot(fit1, type="lines") #���κм��� ������ ��ǥ
#�츮�� 1���� ū ���� ���̹Ƿ� plot��
#variances�� 1���� ū�� 2�� �̹Ƿ� 2���� �� ��
loadings(fit1) #���ε��� ���緮-ù��° ������ �� ������ �����
#sum(loadings(fit1)[,1]^2) -> ù��° �л��� ��ü �л꿡 ���� 1�� ���´ٴ� ���� �� �� �ִ�.

### Factor�� ���� ��������
######### OPTION 1 #########
fit <- factanal(PCA1_1, 2, rotation="varimax") 
#���μ� 2���� ���κм� ����� varimax ����� ����Ͽ� ȸ��
print(fit, digits=3, sort=TRUE) #�Էµ� ���� ������� ���: sort=TRUE �ɼ�
#summary(fit)
load <- fit$loadings[,1:2]
plot(load) #��ǥ���
text(load, labels=names(PCA1_1), cex=0.7)

######### OPTION 2 ########
install.packages("psych")
library(psych)
fit1_1 <- principal(PCA1_1, nfactors=2, rotate="varimax")
fit1_1

#################### �ŷڵ��м� ####################
# ������ �ɰ���
myvar1a <- c("q33a01w1", "q33a02w1", "q33a03w1", "q33a04w1", "q33a05w1", "q33a06w1")
myvar1b <- c("q33a07w1", "q33a08w1", "q33a09w1", "q33a10w1")
# myvar1a <- myvar[1:6] ; myvar1b <- myvar[7:10]
PCA1a <- spssdata[myvar1a]
PCA1b <- spssdata[myvar1b]

###�ŷڵ��м� : ũ�ҹ��� ����
library(psych)
alpha(PCA1a, na.rm=TRUE)
alpha(PCA1b, na.rm=TRUE)

###########################################################################
# sjPlot ��Ű���� �̿��� ���κм��� �ŷڵ��м�
#install.packages("sjPlot")
library(sjPlot)
tab_pca(PCA1, title="�θ� ���� ����", wrap.labels=20, show.cronb=TRUE,
        show.var=TRUE, string.pov="�л����", string.cpov="���� ����")


##############################
## ���ο� ���� ����
attach(spssdata)
spssdata$gattach <- q33a01w1+q33a02w1+q33a03w1+q33a04w1+q33a05w1+q33a06w1
spssdata$outatt  <- q33a07w1+q33a08w1+q33a09w1+q33a10w1
detach(spssdata)



###########################################################################
# sjPlot ��Ű���� �̿��� ���κм��� �ŷڵ��м�
library(Hmisc)
test <- spss.get("C:/Users/�輭��/Desktop/����/2020��2�б�/��ȸ�����ڷ�/��2 �г� ������(SPSS)/04-1 ��2 �г� 1���⵵ ������(SPSS).sav",
                 use.value.labels=FALSE)
newdata <- subset(test, scharew1 >= 100 & scharew1 < 200, 
                  select=c("q48a01w1","q48a02w1","q48a03w1","q48a04w1","q48a05w1","q48a06w1",
                           "q48a07w1","q48a08w1","q48a09w1","q48a10w1","q48a11w1","q48a12w1"))
library(sjPlot)
tab_pca(newdata, title="�ڱ⸸����", wrap.labels=20, show.cronb=TRUE,
        show.var=TRUE, string.pov="�л����", string.cpov="���� ����")

############################################################################
#################### �ŷڵ��м� ####################
# ������ �ɰ���
myvar1a <- c("q48a01w1", "q48a02w1", "q48a03w1")
myvar1b <- c("q48a04w1", "q48a05w1", "q48a06w1")

PCA1a <- newdata[myvar1a]
PCA1b <- newdata[myvar1b]

###�ŷڵ��м� : ũ�ҹ��� ����
library(psych)
alpha(PCA1a, na.rm=TRUE)
alpha(PCA1b, na.rm=TRUE)

attach(newdata)
newdata$rq48a04w1[q48a04w1==1] <- 5
newdata$rq48a04w1[q48a04w1==2] <- 4
newdata$rq48a04w1[q48a04w1==3] <- 3
newdata$rq48a04w1[q48a04w1==4] <- 2
newdata$rq48a04w1[q48a04w1==5] <- 1

newdata$rq48a05w1[q48a05w1==1] <- 5
newdata$rq48a05w1[q48a05w1==2] <- 4
newdata$rq48a05w1[q48a05w1==3] <- 3
newdata$rq48a05w1[q48a05w1==4] <- 2
newdata$rq48a05w1[q48a05w1==5] <- 1

newdata$rq48a06w1[q48a06w1==1] <- 5
newdata$rq48a06w1[q48a06w1==2] <- 4
newdata$rq48a06w1[q48a06w1==3] <- 3
newdata$rq48a06w1[q48a06w1==4] <- 2
newdata$rq48a06w1[q48a06w1==5] <- 1

myvar <- c("q48a01w1", "q48a02w1", "q48a03w1" , "rq48a04w1", "rq48a05w1", "rq48a06w1")
PCA1aa <- newdata[myvar]

alpha(PCA1aa, na.rm=TRUE)