
## ##baseline hazard
## k<-2
## lambda<-60
## #
## h<-function(t,k,lambda) (k/lambda)*((t/lambda)^(k-1)) ##baseline hazard, see failure rate here https://en.wikipedia.org/wiki/Weibull_distribution#Cumulative_distribution_function
## t<-seq(0.01,100,length.out=1000)
## par(mfrow=c(1,2),mar=c(3,3,1,1))
## plot(t,h(t,k=k,lambda=lambda),type='l')

## S<-function(t,gamma,...) {
##     ##integrating the hazard
##     t0<-seq(0,t,length.out=1000)
##     del<-mean(diff(t0))
##     t0<-t0+del
##     H0<-h(t0,k=k,lambda=lambda)*exp(gamma)
##     H<-sum(H0*del)
##     ##
##     exp(-1*H) #see expression for survival function at top of p2 here http://people.stat.sfu.ca/~raltman/stat402/402L32.pdf
## }
## Sv<-Vectorize(S,"t")

## plot(t,Sv(t,0,k=k,lambda=lambda),type='l',ylim=0:1)
## lines(t,Sv(t,1,k=k,lambda=lambda))

## N<-1000
## rho<-0.3
## library(MASS)
## xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
## b0<-0
## b1<-b2<-1
## b3<-.1

## sample.time<-function(gamma,...) {
##     qu<-runif(1)
##     #ff<-function(t,qu,...) abs(qu-S(t,gamma,k=k,lambda=lambda))
##     #t<-optim(10,ff,qu=qu,gamma=gamma,k=k,lambda=lambda,S=S,
##     #      method="Brent",
##     #      lower=0,upper=10000)$par
##     #ifelse(t>100,100,t) #censoring
##     z<-seq(.01,100,length.out=250)
##     y<-Sv(z,gamma=gamma,k=k,lambda=lambda)
##     ii<-which.min(abs(y-qu))
##     z[ii]
## }
## gamma<-b0+b1*xz[,1]#+b2*xz[,2]+b3*xz[,1]*xz[,2]
## df<-data.frame(x=xz[,1])
## NA->df$t
## for (i in 1:length(gamma)) df$t[i]<-sample.time(gamma[i],Sv=Sv,k=k,lambda=lambda)
## ifelse(df$t==100,0,1)->df$dead
## library("survival")
## m <- coxph(Surv(t, dead) ~ x, data = df)
## m

#library(flexsurv)

#m<-flexsurvreg(Surv(t, dead) ~ x, data = df,dist="weibull")


##     tl<-sl<-list()
##     counter<-1
##     test<-TRUE
##     while (test) {
##         t.tmp<-seq(counter-1+.001,counter,length.out=100)
##         tl[[as.character(counter)]]<-t.tmp
##         tmp<-S(t.tmp,gamma,k=k,lambda=lambda)
##         sl[[as.character(counter)]]<-tmp
##         test<-!(1-max(tmp)<.01) &  counter<1000
##         print(counter)
##         counter<-counter+1
##     }
    
## S<-function(t,gamma,...) exp(-1*h(t,k=k,lambda=lambda)*exp(gamma))

