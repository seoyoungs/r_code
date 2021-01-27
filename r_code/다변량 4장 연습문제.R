#4.2
g1=matrix(c(1,-2,0,-2,5,0,0,0,2), nc=3)
g1
lamda=eigen(g1) #m번 고유값, 고유벡터구하기
lamda

library(MASS)
g1=matrix(c(1,-2,0,-2,5,0,0,0,2), nc=3)
zz=ginv(g1)
zz

lamda1=eigen(zz)
lamda1













#4.3
s1=matrix(c(14,8,3,8,5,2,3,2,1), nc=3)
s1
S=var(s1)
S
g=det(S) #일반화분산
g
sum(diag(S)) #총분산

s2=matrix(c(6,6,1,6,8,2,1,2,1), nc=3)
z=var(s2)
z
g2=det(z)
g2
sum(diag(z))













