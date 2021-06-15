##b3=0
N<-c(250,1000,5000)
rho<-seq(0,.3,by=.1)
alpha<-seq(0,100,by=5)
pars<-expand.grid(N=N,rho=rho,alpha=alpha)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp


simfun<-function(pars,b1=1,b2=1,s2=1) {
    for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
    library(sn)
    library(e1071)
    library(MASS)
    std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
    sig<-numeric()
    for (i in 1:1000) {
        xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
        err<-rsn(n=N, xi=0, omega=2, alpha=alpha, tau=0, dp=NULL)
        err<-(err-mean(err))/sd(err)
        y<-b1*xz[,1]+b2*xz[,2]+err#rnorm(N)
        y<-std(y)
        ##
        df<-data.frame(y=y,x=xz[,1],z=xz[,2])
        m<-lm(y~x*z,df)
        pv<-summary(m)$coef[4,4]
        sig[i]<-ifelse(pv<.05,1,0)
    }
    c(N,rho,alpha,mean(sig))
}
library(parallel)
out<-mclapply(pars,simfun,mc.cores=25)
tab<-do.call("rbind",out)


pdf("/home/bd/Dropbox/Apps/Overleaf/Interaction_problems/skew1.pdf",width=8,height=3)
L<-split(data.frame(tab),tab[,1])
par(mgp=c(2,1,0),mfrow=c(1,3),mar=c(3,3,1,1))
for (ii in 1:length(L)) {
    zz<-split(L[[ii]],L[[ii]][,2])
    plot(NULL,xlim=c(0,100),ylim=c(0,1),xlab=expression(alpha),ylab='FDR')
    cols<-colorRampPalette(c("red", "blue"))( length(zz))
    for (i in 1:length(zz)) {
        tmp<-zz[[i]]
        lines(tmp[,3],tmp[,4],col=cols[i],lwd=2)
    }
    legend("topleft",bty='n',paste("N=",unique(tmp[,1]),sep=''))
    legend("bottomright",bty='n',names(zz),fill=cols,title=expression(rho))
    abline(h=.05,col='gray')
}
dev.off()



##b3!=0
N<-c(250,1000,5000)
rho<-seq(0,.3,by=.1)
alpha<-50#seq(0,100,by=5)
b3<-seq(-.15,.15,by=.01)
pars<-expand.grid(N=N,rho=rho,alpha=alpha,b3=b3)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp


simfun<-function(pars,b1=1,b2=1,s2=1) {
    for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
    library(sn)
    library(e1071)
    library(MASS)
    std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
    sig<-numeric()
    for (i in 1:1000) {
        xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
        err<-rsn(n=N, xi=0, omega=2, alpha=alpha, tau=0, dp=NULL)
        err<-(err-mean(err))/sd(err)
        y<-b1*xz[,1]+b2*xz[,2]+err+b3*xz[,1]*xz[,2]
        y<-std(y)
        ##
        df<-data.frame(y=y,x=xz[,1],z=xz[,2])
        m<-lm(y~x*z,df)
        pv<-summary(m)$coef[4,4]
        sig[i]<-ifelse(pv<.05,1,0)
    }
    c(N,rho,alpha,b3,mean(sig))
}
library(parallel)
out<-mclapply(pars,simfun,mc.cores=25)
tab<-do.call("rbind",out)

pdf("/home/bd/Dropbox/Apps/Overleaf/Interaction_problems/skew2.pdf",width=8,height=3)
L<-split(data.frame(tab),tab[,1])
par(mgp=c(2,1,0),mfrow=c(1,3),mar=c(3,3,1,1))
for (ii in 1:length(L)) {
    zz<-split(L[[ii]],L[[ii]][,2])
    plot(NULL,xlim=c(0,.15),ylim=c(0,1),xlab=expression(alpha),ylab='Power')
    cols<-colorRampPalette(c("red", "blue"))( length(zz))
    for (i in 1:length(zz)) {
        tmp<-zz[[i]]
        lines(tmp[,4],tmp[,5],col=cols[i],lwd=2)
    }
    legend("topleft",bty='n',paste("N=",unique(tmp[,1]),sep=''))
    legend("bottomright",bty='n',names(zz),fill=cols,title=expression(rho))
    abline(h=.8,col='gray')
}
dev.off()

