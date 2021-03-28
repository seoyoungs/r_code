문제48p 2.1

y<-c(5,10,11,11,4,5,13,19,12,9)
mean(y)
sd(y)
var(y)
mu=10
ybar=mean(y)
s=sd(y)
n=length(y)
t=(ybar-mu)/(s/sqrt(n))
t
alpha=0.05
p_value<- 1-pt(T,df=n-1)
p_value
lu=qt(df=n-1,alpha/2)
ld=qt(df=n-1,1-alpha/2)
c(lu,ld)

var(y)
alpha=0.05
lo<- (n-1)*var(y)/qchisq(alpha/2, df=n-1)
up<- (n-1)*var(y)/qchisq(1-alpha/2, df=n-1)
lo ; up;
sigma0<-5
chi.t<-var(y)*(length(y)-1)/sigma0

pchisq(chi.t, length(y)-1, lower.tail=(var(y)<sigma0))*2
pchisq(chi.t, length(y)-1, lower.tail=(var(y)<sigma0))

y=rnorm(20)
hist(y)
qqnorm(y)
qqline(y)
shapiro.test(y)

문제 2.3
x1=c(65,81,57,66,75,70)
x2=c(78,79,82,74,79,84)
mean(x1)
mean(x2)
var(x1)
var(x2)
sd(x1)
sd(x2)

t.test(x1,x2,var.equal=TRUE,alternative="two.sided")

t.test(x1,x2,var.test=FALSE, conf.level=0.95)

var.test(x1,x2)

문제 2.5
a1=c(55,60,70,75,66,78,80,83,88,73)
a2=c(54,55,64,73,61,70,76,65,78,72)
b=a1-a2
b

mean(b)
var(b)
sd(b)

t.test(a1,a2, paired=T)










