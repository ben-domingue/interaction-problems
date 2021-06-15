##b3=0
N<-c(250,1000,5000)
rho<-.3 #seq(0,.3,by=.1)
b3<-seq(0,.3,by=.025)
pars<-expand.grid(N=N,rho=rho,b3=b3)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp

simfun<-function(pars,b1=1,b2=1,s2=1) {
    for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
    std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
    library(MASS)
    sig2<-sig<-r2<-numeric()
    for (i in 1:1000) {
        xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
        y<-b1*xz[,1]+b2*xz[,2]+b3*xz[,1]*xz[,2]+rnorm(N,sd=sqrt(s2))
        y<-std(y)
        df<-data.frame(y=y,x=xz[,1],z=xz[,2])
        m<-lm(y~x*z,df)
        pv<-summary(m)$coef[4,4]
        sig[i]<-ifelse(pv<.05,1,0)
        ##
        xz[,2]<-ifelse(xz[,2]>0,1,0)
        y<-b1*xz[,1]+b2*xz[,2]+b3*xz[,1]*xz[,2]+rnorm(N,sd=sqrt(s2))
        y<-std(y)
        df<-data.frame(y=y,x=xz[,1],z=xz[,2])
        m<-lm(y~x*z,df)
        pv<-summary(m)$coef[4,4]
        sig2[i]<-ifelse(pv<.05,1,0)
    }
    c(N,rho,b3,mean(sig),mean(sig2))
}
library(parallel)
out<-mclapply(pars,simfun,mc.cores=25)
tab<-do.call("rbind",out)


pdf("/home/bd/Dropbox/Apps/Overleaf/Interaction_problems/vanilla_binarypredictor.pdf",width=8,height=3)
source("/home/bd/Dropbox/projects/interaction_problems/src/000_functions.R")
L<-split(data.frame(tab),tab[,1])
par(mgp=c(2,1,0),mfrow=c(1,3),mar=c(3,3,3.5,1))
for (ii in 1:length(L)) {
    zz<-split(L[[ii]],L[[ii]][,2])
    plot(NULL,xlim=range(tab[,3]),ylim=c(0,1),xlab=expression(beta[3]),ylab='Power')
    vert()
    for (i in 1:length(zz)) {
        tmp<-zz[[i]]
        lines(tmp[,3],tmp[,4],col='black',lwd=2)
        lines(tmp[,3],tmp[,5],col='red',lwd=2)
    }
    legend("topleft",bty='n',paste("N=",unique(tmp[,1]),sep=''))
    abline(h=.8,col='gray')
    ##
    xx<-L[[ii]]
    mm<-loess(xx[,5]~xx[,3])
    co<-coef(mm)
    vals<-seq(0,max(xx[,3]),by=.1)
    axis(side=3,at=vals,formatC(predict(mm,vals),digits=0))
    mtext(side=3,line=2,expression(r^2),cex=.7)
}
legend("bottomright",bty='n',fill=c("black","red"),c("z-continuous","z-binary"))
dev.off()
