pat=read.csv("C:/Users/김서영/Documents/r프로그램/pa.csv", header=T)
pat
attach(pat)
x=pat[2:6]

cor(x)
#스크리그래프
library(graphics)
prin=princomp(x)
screeplot(prin,npcs=5,type="lines",main="scree plot")

fact2=factanal(x,factor=2, scross="regression")
fact2
library(prin)=princomp(x)
med.varimax = principal(pat,factors=2,rotate='varimax')
med.varimax
med.oblimin = principal(med.data,nfactors=3,rotate='oblimin',scores=T,method='regression')



