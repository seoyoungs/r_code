#5Àå ¿¬½À5
b1=read.csv(file="C:/Users/±è¼­¿µ/Documents/rÇÁ·Î±×·¥/eng.csv", header=TRUE)
b1
attach(b1)
x=b1[,2:3]
p=ncol(x)
xbar=apply(x,2,mean) #Ç¥º»Æò±Õº¤ÅÍ
xbar
S=cov(x) #Ç¥º»°øºÐ»êÇà·Ä
S

#manova°ËÁ¤À¸·Î ¸ðÆò±Õº¤ÅÍ
group<-factor(group)
y=cbind(x1,x2)
fit=manova(y~group)
summary(fit, test="Wilks")





