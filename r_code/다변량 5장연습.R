#5�� ����5
b1=read.csv(file="C:/Users/�輭��/Documents/r���α׷�/eng.csv", header=TRUE)
b1
attach(b1)
x=b1[,2:3]
p=ncol(x)
xbar=apply(x,2,mean) #ǥ����պ���
xbar
S=cov(x) #ǥ�����л����
S

#manova�������� ����պ���
group<-factor(group)
y=cbind(x1,x2)
fit=manova(y~group)
summary(fit, test="Wilks")





