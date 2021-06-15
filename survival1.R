## ff<-function(pars,b1,b2,b3) {
##     for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
##     ##
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
##     Sv<-Vectorize(S,"t")
##     sample.time<-function(gamma,Sv,...) { #this will sample the observed time (with censoring) from the survival distribution
##         qu<-runif(1)
##         ff<-function(t,qu,...) abs(qu-S(t,gamma,k=k,lambda=lambda)) 
##         t<-optim(10,ff,qu=qu,gamma=gamma,k=k,lambda=lambda,S=S,
##                  method="Brent",
##                  lower=0,upper=150)$par
##         ifelse(t>100,100,t) #censoring
##     }
##     library(MASS)
##     xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
##     gamma<-b1*xz[,1]+b2*xz[,2]+b3*xz[,1]*xz[,2]
##     df<-data.frame(x=xz[,1],z=xz[,2])
##     NA->df$t
##     for (i in 1:length(gamma)) df$t[i]<-sample.time(gamma[i],
##                                                     S=S,
##                                                     #Sv=Sv,
##                                                     k=k,lambda=lambda
##                                                     )
##     ifelse(df$t==100,0,1)->df$dead
##     #print(table(df$dead))
##     library("survival")
##     m <- coxph(Surv(t, dead) ~ x*z, data = df)
##     summary(m)$coef
## }

## N<-1000
## rho<-0.3
## b0<-0
## b1<-.5
## b2<-.5
## b3<-.05
## ##baseline hazard
## k<-2
## lambda<-60
## #
## pars<-c(N=1000,rho=rho)
## ff(pars,b1=b1,b2=b2,b3=b3)

## library(parallel)
## pv<-list()
## for (N in c(250,1000,5000)) for (b3 in seq(0,.15,by=.05)) for (rho in seq(0,.3,by=.1)) {
##                                   pars<-c(N=N,rho=rho)
##                                   parsL<-list()
##                                   for (i in 1:250) parsL[[i]]<-pars
##                                   print(paste(N,b3,rho))
##                                   pv[[paste(N,b3,rho)]]<-mclapply(parsL,ff,mc.cores=30,b1=b1,b2=b2,b3=b3)
##                               }


## getp<-function(x) {
##     p<-lapply(x,function(x) x[3,5])
##     mean(unlist(p)<.05,na.rm=TRUE)
## }
## p<-lapply(pv,getp)
## txt<-strsplit(names(p)," ")
## tmp<-do.call("rbind",txt)
## tmp<-apply(tmp,2,as.numeric)
## tab<-cbind(tmp,unlist(p))

## L<-split(data.frame(tab),tab[,1])
## par(mgp=c(2,1,0),mfrow=c(1,3),mar=c(3,3,1,1))
## for (ii in 1:length(L)) {
##     zz<-split(L[[ii]],L[[ii]][,3])
##     plot(NULL,xlim=c(0,.15),ylim=c(0,1),xlab=expression(b[3]),ylab='Power')
##     cols<-colorRampPalette(c("red", "blue"))( length(zz))
##     for (i in 1:length(zz)) {
##         tmp<-zz[[i]]
##         lines(tmp[,2],tmp[,4],col=cols[i],lwd=2)
##         #lines(tmp[,3],tmp[,5],col=cols[i],lwd=2,lty=2)
##     }
##     legend("topleft",bty='n',paste("N=",unique(tmp[,1]),sep=''))
##     legend("topright",bty='n',names(zz),fill=cols,title=expression(rho))
##     abline(h=.05,col='gray')
## }
