## ##b3=0
## N<-c(250,1000,5000)
## rho<-seq(0,.3,by=.1)
## ##baseline hazard
## k<-2
## lambda<-60
## ##
## pars<-expand.grid(N=N,rho=rho,lambda=lambda,k=k)
## tmp<-list()
## for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
## pars<-tmp


## simfun<-function(pars,b1=0.5,b2=0.5) {
##     ##survival functions
##     h<-function(t,k,lambda) (k/lambda)*((t/lambda)^(k-1)) ##baseline hazard, see failure rate here https://en.wikipedia.org/wiki/Weibull_distribution#Cumulative_distribution_function
##     S<-function(t,gamma,...) {
##         ##integrating the hazard
##         t0<-seq(0,t,length.out=1000)
##         del<-mean(diff(t0))
##         t0<-t0+del
##         H0<-h(t0,k=k,lambda=lambda)*exp(gamma)
##         H<-sum(H0*del)
##         ##
##         exp(-1*H) #see expression for survival function at top of p2 here http://people.stat.sfu.ca/~raltman/stat402/402L32.pdf
##     }
##     sample.time<-function(gamma,S,...) { #this will sample the observed time (with censoring) from the survival distribution
##         qu<-runif(1)
##         ff<-function(t,qu,...) abs(qu-S(t,gamma,k=k,lambda=lambda)) 
##         t<-optim(10,ff,qu=qu,gamma=gamma,k=k,lambda=lambda,S=S,
##                  method="Brent",
##                  lower=0,upper=150)$par
##         ifelse(t>100,100,t) #censoring
##     }
##     vst<-Vectorize(sample.time,"gamma")
##     ##
##     for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
##     library(MASS)
##     library("survival")
##     ##inside
##     sig<-numeric()
##     for (i in 1:1000) {
##         xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
##         gamma<-b1*xz[,1]+b2*xz[,2]
##         df<-data.frame(x=xz[,1],z=xz[,2])
##         ## NA->df$t
##         ## for (i in 1:length(gamma)) df$t[i]<-sample.time(gamma[i],
##         ##                                                 S=S,
##         ##                                                 k=k,lambda=lambda
##         ##                                                 )
##         df$t<-vst(gamma,S=S,k=k,lambda=lambda)
##         ifelse(df$t==100,0,1)->df$dead
##         #print(table(df$dead))
##         m <- coxph(Surv(t, dead) ~ x*z, data = df)
##         pv<-summary(m)$coef[3,5]
##         sig[i]<-ifelse(pv<.05,1,0)
##     }
##     c(N,rho,lambda,mean(sig))
## }
## library(parallel)
## out<-mclapply(pars,simfun,mc.cores=25)
## tab<-do.call("rbind",out)


## pdf("/home/bd/Dropbox/Apps/Overleaf/Interaction_problems/surv1.pdf",width=8,height=3)
## L<-split(data.frame(tab),tab[,1])
## par(mgp=c(2,1,0),mfrow=c(1,3),mar=c(3,3,1,1))
## for (ii in 1:length(L)) {
##     zz<-split(L[[ii]],L[[ii]][,2])
##     plot(NULL,xlim=c(0,2),ylim=c(0,1),xlab=expression(lambda),ylab='FDR')
##     cols<-colorRampPalette(c("red", "blue"))( length(zz))
##     for (i in 1:length(zz)) {
##         tmp<-zz[[i]]
##         lines(tmp[,3],tmp[,4],col=cols[i],lwd=2)
##         lines(tmp[,3],tmp[,5],col=cols[i],lwd=2,lty=2)
##     }
##     legend("topleft",bty='n',paste("N=",unique(tmp[,1]),sep=''))
##     legend("topright",bty='n',names(zz),fill=cols,title=expression(rho))
##     abline(h=.05,col='gray')
## }
## legend("bottomright",bty='n',lwd=2,lty=c(1,2),c("Symmetric","Assymetric"))
## dev.off()

pdf("/home/bd/Dropbox/Apps/Overleaf/Interaction_problems/survival0.pdf",width=6,height=6)
par(mgp=c(2,1,0))
##baseline hazard
k<-2
lambda<-60
h<-function(t,k,lambda) (k/lambda)*((t/lambda)^(k-1)) ##baseline hazard, see failure rate here https://en.wikipedia.org/wiki/Weibull_distribution#Cumulative_distribution_function
S<-function(t,gamma,...) {
    ##integrating the hazard
    t0<-seq(0,t,length.out=1000)
    del<-mean(diff(t0))
    t0<-t0+del
    H0<-h(t0,k=k,lambda=lambda)*exp(gamma)
    H<-sum(H0*del)
    ##
    exp(-1*H) #see expression for survival function at top of p2 here http://people.stat.sfu.ca/~raltman/stat402/402L32.pdf
}
S<-Vectorize(S,"t")
t<-seq(0.01,100,length.out=1000)
y<-S(t,gamma=0)
plot(t,y,type='l',lwd=2,ylim=0:1,xlab="t",ylab="S(t)")
dev.off()
     
##b3!=0
N<-c(250,1000,5000)
rho<-c(0,.5) #rho<-seq(0,.3,by=.1)
b3<-seq(0,.075,by=.025/2)
##baseline hazard
k<-2
lambda<-60
##
pars<-expand.grid(N=N,rho=rho,lambda=lambda,k=k,b3=b3)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp


simfun<-function(pars,b1=0.5,b2=0.5) {
    ##survival functions
    h<-function(t,k,lambda) (k/lambda)*((t/lambda)^(k-1)) ##baseline hazard, see failure rate here https://en.wikipedia.org/wiki/Weibull_distribution#Cumulative_distribution_function
    S<-function(t,gamma,...) {
        ##integrating the hazard
        t0<-seq(0,t,length.out=1000)
        del<-mean(diff(t0))
        t0<-t0+del
        H0<-h(t0,k=k,lambda=lambda)*exp(gamma)
        H<-sum(H0*del)
        ##
        exp(-1*H) #see expression for survival function at top of p2 here http://people.stat.sfu.ca/~raltman/stat402/402L32.pdf
    }
    sample.time<-function(gamma,S,...) { #this will sample the observed time (with censoring) from the survival distribution
        qu<-runif(1)
        ff<-function(t,qu,...) abs(qu-S(t,gamma,k=k,lambda=lambda)) 
        t<-optim(10,ff,qu=qu,gamma=gamma,k=k,lambda=lambda,S=S,
                 method="Brent",
                 lower=0,upper=150)$par
        ifelse(t>100,100,t) #censoring
    }
    vst<-Vectorize(sample.time,"gamma")
    ##
    for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
    library(MASS)
    library("survival")
    ##inside
    sig.lm<-sig<-numeric()
    for (i in 1:1000) {
        xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
        gamma<-b1*xz[,1]+b2*xz[,2]+b3*xz[,1]*xz[,2]
        df<-data.frame(x=xz[,1],z=xz[,2])
        ## NA->df$t
        ## for (i in 1:length(gamma)) df$t[i]<-sample.time(gamma[i],
        ##                                                 S=S,
        ##                                                 k=k,lambda=lambda
        ##                                                 )
        df$t<-vst(gamma,S=S,k=k,lambda=lambda)
        ifelse(df$t==100,0,1)->df$dead
        #print(table(df$dead))
        m <- coxph(Surv(t, dead) ~ x*z, data = df)
        pv<-summary(m)$coef[3,5]
        sig[i]<-ifelse(pv<.05,1,0)
        m<-lm(t~x*z,df)
        pv<-summary(m)$coef[4,4]
        sig.lm[i]<-ifelse(pv<.05,1,0)
    }
    c(N,rho,lambda,k,b3,mean(sig),mean(sig.lm))
}
library(parallel)
out<-mclapply(pars,simfun,mc.cores=25,b1=0.5,b2=0.5)
tab<-do.call("rbind",out)
dump("tab","")



pdf("/home/bd/Dropbox/Apps/Overleaf/Interaction_problems/surv1.pdf",width=8,height=3)
source("/home/bd/Dropbox/projects/interaction_problems/src/000_functions.R")
L<-split(data.frame(tab),tab[,1])
par(mgp=c(2,1,0),mfrow=c(1,3),mar=c(3,3,1,1))
for (ii in 1:length(L)) {
    zz<-split(L[[ii]],L[[ii]][,2])
    plot(NULL,xlim=c(0,.075),ylim=c(0,1),xlab=expression(b[3]),ylab='Power')
    vert()
    cols<-colorRampPalette(c("red", "blue"))( length(zz))
    for (i in 1:length(zz)) {
        tmp<-zz[[i]]
        lines(tmp[,5],tmp[,6],col=cols[i],lwd=2)
    }
    lines(tmp[,5],tmp[,7],col='gray',lwd=2)
    legend("topleft",bty='n',paste("N=",unique(tmp[,1]),sep=''))
    legend("topright",bty='n',names(zz),fill=cols,title=expression(rho))
    abline(h=.8,col='gray')
}
dev.off()

