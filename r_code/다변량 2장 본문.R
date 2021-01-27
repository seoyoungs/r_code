
A=matrix(c(3,1,-1,5,2,4),nc=3)
A
tA=t(A)
tA

a=c(1,-2,3)
b=c(2,2,4)
a+b
t(a)*b
t(a)%*%b
3*a
la=sqrt(t(a)%*%a)
la
lb=sqrt(t(b)%*%b)
lb
cos_theta=t(a)%*%b/(la*lb)
cos_theta

A=matrix(c(3,1,-1,5,2,4),nc=3)
B=matrix(c(1,-1,2,3,3,5),nc=3)
A+B
t(B)
A%*%t(B)

A=matrix(c(1,-3,2,5),nc=2)
det(A)
A=matrix(c(1,0,1,2,3,5,1,4,6),nc=3)
det(A)

library(MASS)
A=matrix(c(1,-3,2,5),nc=2)
ginv(A)

A=matrix(c(1,0,1,2,3,5,1,4,6),nc=3)
ginv(A)

A=matrix(c(1/sqrt(2),-1/sqrt(2),1/sqrt(2),1/sqrt(2)),nc=2)
t(A)%*%A
I=round(t(A)%*%A,digits=3)
I

A=matrix(c(4,2,-5,-3),nc=2)
lambda=eigen(A)
lambda
lambda$values[[1]]
lambda$vectors[,1]
lambda$values[[2]]
lambda$vectors[,2]


A=matrix(c(3,1,1,3),nc=2)
eigen(A)
trace=sum(diag(A))
trace
det(A)









