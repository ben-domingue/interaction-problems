
## ##b3!=0
## N<-c(250,1000,5000)
## .3<-rho #rho<-seq(0,.3,by=.1)
## b0<-0#,2)
## b3<-seq(0,.15,by=.01)
## pars<-expand.grid(N=N,rho=rho,b0=b0,b3=b3)
## tmp<-list()
## for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
## pars<-tmp

## simfun<-function(pars,b1=1,b2=1,s2=1) {
##     for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
##     library(MASS)
##     library(ordinal)
##     std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
##     sig0<-sig<-numeric()
##     for (i in 1:100) {
##         theta<-sort(rnorm(3))
##         xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
##         y<-b1*xz[,1]+b2*xz[,2]+rnorm(N,sd=sqrt(s2))+b3*xz[,1]*xz[,2]
##         y<-(y-mean(y))/sd(y)
##         J<-(length(theta)+1)
##         p<-matrix(0,nrow=N,ncol=J)
##         sigma<-function(z) 1/(1+exp(-z))
##         for (j in 1:(J-1)) {
##             tmp<-theta[j]-y
##             p[,j]<-sigma(tmp)
##         }
##         cum.p<-p
##         for (j in 2:(J-1))  p[,j]<-cum.p[,j]-cum.p[,(j-1)]
##         p[,J]<-1-rowSums(p)
##         for (ii in 1:N) {
##             v<-rmultinom(1,1,p[ii,])
##             y[ii]<-which(v==1)
##         }
##         ##
##         df<-data.frame(y=y,x=xz[,1],z=xz[,2])
##         m<-clm("factor(y)~x*z",data=df)
##         co<-summary(m)$coef
##         iii<-grep("x:z",rownames(co))
##         pv<-co[iii,4]
##         sig0[i]<-ifelse(pv<.05,1,0)
##         ##
##         m<-lm(y~x*z,df)
##         pv<-summary(m)$coef[4,4]
##         sig[i]<-ifelse(pv<.05,1,0)
##         ##
##     }
##     c(N,rho,b3,mean(sig),mean(sig0))
## }
## library(parallel)
## out<-mclapply(pars,simfun,mc.cores=25)
## tab<-do.call("rbind",out)



## #pdf("/home/bd/Dropbox/Apps/Overleaf/Interaction_problems/ordinal2.pdf",width=8,height=3)
## L<-split(data.frame(tab),tab[,1])
## par(mgp=c(2,1,0),mfrow=c(1,3),mar=c(3,3,1,1))
## for (ii in 1:length(L)) {
##     zz<-split(L[[ii]],L[[ii]][,2])
##     plot(NULL,xlim=c(0,.15),ylim=0:1,xlab=expression(beta[3]),ylab='Power')
##     cols<-colorRampPalette(c("red", "blue"))( length(zz))
##     for (i in 1:length(zz)) {
##         tmp<-zz[[i]]
##         lines(tmp[,3],tmp[,4],col=cols[i],lwd=2)
##         lines(tmp[,3],tmp[,5],col=cols[i],lwd=2,lty=2)
##     }
##     legend("topleft",bty='n',paste("N=",unique(tmp[,1]),sep=''))
##     abline(h=.8,col='gray')
## }
## legend("bottomright",bty='n',names(zz),fill=cols,title=expression(rho))
## #dev.off()
