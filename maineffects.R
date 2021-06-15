##b3=0
N<-c(1000)
rho<-c(0,.5) #rho<-seq(0,.3,by=.1)
b3<-seq(0,.3,by=.025)
pars<-expand.grid(N=N,rho=rho,b3=b3)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp

simfun<-function(pars,b1=.1,b2=.1,s2=1) {
    for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
    std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
    library(MASS)
    se<-list()
    for (i in 1:100) {
        xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
        y<-b1*xz[,1]+b2*xz[,2]+b3*xz[,1]*xz[,2]+rnorm(N,sd=sqrt(s2))
        y<-std(y)
        df<-data.frame(y=y,x=xz[,1],z=xz[,2])
        ##
        m<-lm(y~x*z,df)
        m0<-lm(y~x+z,df)
        #r2[i]<-summary(m)$r.squared-summary(m0)$r.squared
        se[[i]]<-summary(m)$coef[,2]
    }
    se<-do.call("rbind",se)
    c(N,rho,b3,colMeans(se))
}
library(parallel)
out<-mclapply(pars,simfun,mc.cores=25)
tab<-do.call("rbind",out)

pdf("/home/bd/Dropbox/Apps/Overleaf/Interaction_problems/mainfx.pdf",width=8,height=3)
source("/home/bd/Dropbox/projects/interaction_problems/src/000_functions.R")
L<-data.frame(tab)
par(mgp=c(2,1,0),mfcol=c(1,3),mar=c(3,3,3.5,1))
ncols<-5:7
for (ii in 1:length(ncols)) {
    zz<-split(L,L[,2])
    plot(NULL,xlim=range(tab[,3]),ylim=c(0,.04),xlab=expression(beta[3]),ylab='SE')
    cols<-colorRampPalette(c("red", "blue"))( length(zz))
    for (i in 1:length(zz)) {
        tmp<-zz[[i]]
        lines(tmp[,3],tmp[,ncols[ii]],col=cols[i],lwd=2,lty=1)
    }
    if (ii==1) legend("bottomleft",bty='n',paste("N=",unique(tmp[,1]),sep=''),title=expression(beta[1]))
    if (ii==2) legend("bottomleft",bty='n',paste("N=",unique(tmp[,1]),sep=''),title=expression(beta[2]))
    if (ii==3) legend("bottomleft",bty='n',paste("N=",unique(tmp[,1]),sep=''),title=expression(beta[3]))
    ##
}
legend("bottomright",bty='n',names(zz),fill=cols,title=expression(rho))
dev.off()
