library(sn)
par(mfrow=c(2,3))
N<-5000
rho<-.3
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
for (alpha in c(0,10,50,100,250,500)) {
    b1<-b2<-1
    library(MASS)
    xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
    ey<-b1*xz[,1]+b2*xz[,2]
    alpha2<-ifelse(ey>0,ey*alpha,0)

    ff<-function(alpha) rsn(n=1,xi=0,omega=2,alpha=alpha,tau=0,dp=NULL)
    rsnV<-Vectorize(ff,"alpha")
    ##
    err<-rsnV(alpha=alpha2)

    
    #err<-(err-mean(err))/sd(err)
    y<-err#y<-std(ey+err)
    hist(y)
    print(skewness(y))
}


z<-list()
for (i in 1:100) z[[i]]<-rsn(n=25,xi=0,omega=1,alpha=seq(0,i,length.out=25),tau=0,dp=NULL)
z<-do.call("rbind",z)



##b3=0
N<-c(250,1000,5000)
rho<-seq(0,.3,by=.1)
alpha<-seq(0,20,by=5)
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
    sk<-sig<-numeric()
    for (i in 1:100) {
        xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
        ey<-b1*xz[,1]+b2*xz[,2]
        alpha2<-ifelse(ey>0,ey*alpha,0)
        err<-rsn(n=N, xi=0, omega=2, alpha=alpha2, tau=0, dp=NULL)
        err<-(err-mean(err))/sd(err)
        y<-std(ey+err)
        ##
        df<-data.frame(y=y,x=xz[,1],z=xz[,2])
        m<-lm(y~x*z,df)
        pv<-summary(m)$coef[4,4]
        sig[i]<-ifelse(pv<.05,1,0)
        sk[i]<-mean(skewness(df$y))
    }
    c(N,rho,alpha,mean(sig),mean(sk))
}
library(parallel)
out<-mclapply(pars,simfun,mc.cores=25)
tab<-do.call("rbind",out)


#pdf("/home/bd/Dropbox/Apps/Overleaf/Interaction_problems/skew1.pdf",width=8,height=3)
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
#dev.off()
