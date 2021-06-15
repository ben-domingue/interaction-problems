## ##looking at error
## lambdaL<-seq(0,2,by=.1)
## rho<-.3
## #for (lambda in lambdaL) {
## lambda<-1
## xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
## errS<-rnorm(N,mean=0,sd=sqrt(1+lambda*xz[,1]^2))
## #y<-b1*xz[,1]+b2*xz[,2]+err
## #y<-std(y)
## sigma<-1+exp(lambda*xz[,1])-exp(0)
## errA<-rnorm(N,mean=0,sd=sqrt(1+lambda*ifelse(xz[,1]>0,xz[,1]^2,0)))
## plot(xz[,1],errS,pch=19)
## points(xz[,1],errA,col='red',pch=19)

lambda<-seq(0,4,by=.25)
y<-sqrt(1+lambda*1)
plot(lambda,y)

##b3=0
N<-c(250,1000,5000)
rho<-seq(0,.3,by=.1)
#lambda<-seq(0,4,by=.25)
lambda<-seq(0,0.25,by=.05)
pars<-expand.grid(N=N,rho=rho,lambda=lambda)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp

simfun<-function(pars,b1=1,b2=1) {
    for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
    std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
    library(MASS)
    ##inside
    sig<-sig2<-numeric()
    for (i in 1:1000) {
        xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
        ##symmetric error
        #err<-rnorm(N,mean=0,sd=sqrt(1+lambda*xz[,1]^2))
        err<-rnorm(N,mean=0,sd=sqrt(1+lambda*abs(xz[,1])))
        y<-b1*xz[,1]+b2*xz[,2]+err
        y<-std(y)
        ##asymmetric error
        ##sigma<-1+exp(lambda*xz[,1])-exp(0)
        errA<-rnorm(N,mean=0,sd=sqrt(1+lambda*xz[,1]))
        y2<-b1*xz[,1]+b2*xz[,2]+errA
        y2<-std(y2)
        ##
        df<-data.frame(y=y,x=xz[,1],z=xz[,2],y=y2)
        m<-lm(y~x*z,df)
        pv<-summary(m)$coef[4,4]
        sig[i]<-ifelse(pv<.05,1,0)
        m<-lm(y2~x*z,df)
        pv<-summary(m)$coef[4,4]
        sig2[i]<-ifelse(pv<.05,1,0)
    }
    c(N,rho,lambda,mean(sig),mean(sig2))
}
library(parallel)
out<-mclapply(pars,simfun,mc.cores=25)
tab<-do.call("rbind",out)


pdf("/home/bd/Dropbox/Apps/Overleaf/Interaction_problems/hetero1.pdf",width=8,height=3)
source("/home/bd/Dropbox/projects/interaction_problems/src/000_functions.R")
L<-split(data.frame(tab),tab[,1])
par(mgp=c(2,1,0),mfrow=c(1,3),mar=c(3,3,1,1))
for (ii in 1:length(L)) {
    zz<-split(L[[ii]],L[[ii]][,2])
    plot(NULL,xlim=range(tab[,3]),ylim=c(0,0.15),xlab=expression(lambda),ylab='FDR')
    background()
    cols<-colorRampPalette(c("red", "blue"))( length(zz))
    for (i in 1:length(zz)) {
        tmp<-zz[[i]]
        lines(tmp[,3],tmp[,4],col=cols[i],lwd=2)
        lines(tmp[,3],tmp[,5],col=cols[i],lwd=2,lty=2)
    }
    legend("topleft",bty='n',paste("N=",unique(tmp[,1]),sep=''))
    legend("topright",bty='n',names(zz),fill=cols,title=expression(rho))
    abline(h=.05,col='gray')
}
legend("bottomright",bty='n',lwd=2,lty=c(1,2),c("Symmetric","Asymmetric"))
dev.off()


##b3!=0
N<-c(250,1000,5000)
rho<-seq(0,.3,by=.1)
lambda<-seq(0,2,by=.1)
b3<-seq(-.15,.15,by=.01)
pars<-expand.grid(N=N,rho=rho,lambda=1,b3=b3)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp

simfun<-function(pars,b1=1,b2=1) {
    for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
    std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
    library(MASS)
    ##inside
    sig<-numeric()
    for (i in 1:1000) {
        xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
        err<-rnorm(N,mean=0,sd=sqrt(1+lambda*xz[,1]^2))
        y<-b1*xz[,1]+b2*xz[,2]+err+b3*xz[,1]*xz[,2]
        y<-std(y)
        ##
        df<-data.frame(y=y,x=xz[,1],z=xz[,2])
        m<-lm(y~x*z,df)
        pv<-summary(m)$coef[4,4]
        sig[i]<-ifelse(pv<.05,1,0)
    }
    c(N,rho,lambda,b3,mean(sig))
}
library(parallel)
out<-mclapply(pars,simfun,mc.cores=25)
tab<-do.call("rbind",out)

pdf("/home/bd/Dropbox/Apps/Overleaf/Interaction_problems/hetero2.pdf",width=8,height=3)
L<-split(data.frame(tab),tab[,1])
par(mgp=c(2,1,0),mfrow=c(1,3),mar=c(3,3,1,1))
for (ii in 1:length(L)) {
    zz<-split(L[[ii]],L[[ii]][,2])
    plot(NULL,xlim=c(0,.15),ylim=c(0,1),xlab=expression(beta[3]),ylab='Power')
    cols<-colorRampPalette(c("red", "blue"))( length(zz))
    for (i in 1:length(zz)) {
        tmp<-zz[[i]]
        lines(tmp[,4],tmp[,5],col=cols[i],lwd=2)
    }
    legend("topleft",bty='n',paste("N=",unique(tmp[,1]),sep=''))
    legend("topright",bty='n',names(zz),fill=cols,title=expression(rho))
    abline(h=.8,col='gray')
}
dev.off()

