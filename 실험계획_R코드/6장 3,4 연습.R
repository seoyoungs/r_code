cake1=read.csv(file="C:/Users/김서영/Documents/r프로그램/cake.csv", header=TRUE)
cake1
attach(cake1)
B<-as.factor(B)
A<-as.factor(A)
C<-as.factor(C)
rep<-as.factor(rep)
tapply(y,A,mean)
tapply(y,B,mean)
tapply(y,C,mean)
#3번
L=aov(y~A*B*C)
model.tables(L,type="means")
#4번,6번,8번
model.tables(L,type="effects")
#5번, 7번, 9번
anova(L)


#6.4번
wjs1=read.csv(file="C:/Users/김서영/Documents/r프로그램/wjs.csv", header=TRUE)
wjs1
attach(wjs1)
db<-as.factor(db)
on<-as.factor(on)
rep<-as.factor(rep)
interaction.plot(db,on,y,bty="l",main="interaction plot")
interaction.plot(on,db,y)

plot(y~db) #4번
plot(y~on) #5번

tapply(y,db,mean)
tapply(y,on,mean)
tapply(y,db:on,mean)

aov2<-aov(y~db+on+db:on)
anova(aov2)

par(mfrow=c(2,2))
plot(aov2)
















