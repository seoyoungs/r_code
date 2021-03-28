#프로그램2.2 95%신뢰구간
y<-c(5.8, 8.0, 9.3, 7.2, 7.8, 10.3, 11.2, 10.8, 9.7, 8.5)
n<-length(y) #y길이
y.m<-mean(y);y.m #점추정값
y.sd<-sd(y)
z<-qnorm(0.975, mean=0, sd=1) #신뢰구간 추정시작
bound<-z*y.sd/sqrt(n)
lo<-y.m-bound #신뢰구간최소
up<-y.m+bound #신뢰구간최대
lo;up

#분산에 대한 카이제곱검정
var(y) #분산에 대한 추정값과 95%신뢰구간
alpha=0.05
lo<-(n-1)*var(y)/qchisq(alpha/2, df=n-1)
up<-(n-1)*var(y)/qchisq(1-alpha/2, df=n-1)
lo ; up

sigma0<-3.5 #Ho=3.5에 대해 유의수준0.05에서 검정(양측)
chi.t<-var(y)*(length(y)-1)/sigma0
pchisq(chi.t, length(y)-1, lower.tail=var(y)<sigma0)*2

#H0>=3.5에 대한 유의수준0.05 검정 (단측)
pchisq(chi.t, length(y)-1, lower.tail=var(y)<sigma0)














