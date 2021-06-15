pdf("/home/bd/Dropbox/Apps/Overleaf/Interaction_problems/transform0.pdf",width=6,height=6)
par(mgp=c(2,1,0))
alpha<-0
lambdalist<-c(0.1,1,2)
cols<-colorRampPalette(c("blue", "red"))( length(lambdalist) ) ## (n)
##
y<-seq(-5,5,length.out=1000)
plot(NULL,xlim=range(y),ylim=c(-2,2),type='l',lwd=2,xlab='y*',ylab='y')
for (i in 1:length(lambdalist)) {
    lambda<-lambdalist[i]
    ys<-1.05^((y-alpha)/lambda)
    ys<-(ys-mean(ys))/sd(ys)
    lines(y,ys,lwd=2,col=cols[i])
    #abline(lm(ys~y),lty=2,col='red',lwd=2)
    }
legend("bottomright",fill=cols,legend=lambdalist,title=expression(lambda))
dev.off()



#b3=0
N<-c(250,1000,5000)
rho<-c(0,.5) #seq(0,.3,by=.1)
lambda<-seq(.1,2,by=.1)
alpha<-0
pars<-expand.grid(N=N,rho=rho,lambda=lambda,alpha=alpha)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp

simfun<-function(pars,b1=1,b2=1,s2=1) {
    for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
    std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
    library(MASS)
    ##inside
    sig<-numeric()
    for (i in 1:1000) {
        xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
        y<-b1*xz[,1]+b2*xz[,2]+rnorm(N,sd=sqrt(s2))
        y<-std(y)
        y<-1.05^((y-alpha)/lambda)
        y<-std(y)
        ##
        df<-data.frame(y=y,x=xz[,1],z=xz[,2])
        m<-lm(y~x*z,df)
        pv<-summary(m)$coef[4,4]
        sig[i]<-ifelse(pv<.05,1,0)
    }
    c(N,rho,lambda,alpha,mean(sig))
}
library(parallel)
out<-mclapply(pars,simfun,mc.cores=25)
tab<-do.call("rbind",out)

pdf("/home/bd/Dropbox/Apps/Overleaf/Interaction_problems/transform1.pdf",width=6,height=2.3)
source("/home/bd/Dropbox/projects/interaction_problems/src/000_functions.R")
L<-split(data.frame(tab),tab[,1])
layout(matrix(c(4,rep(1,3),4,rep(2,3),4,rep(3,3)),byrow=FALSE,nrow=4,ncol=3))
par(mgp=c(2,1,0),mar=c(3,3,.5,.5))
for (ii in 1:length(L)) {
    zz<-split(L[[ii]],L[[ii]][,2])
    plot(NULL,xlim=range(zz[[1]][,3]),ylim=0:1,xlab=expression(lambda),ylab='Discovery Rate')
    background()
    cols<-colorRampPalette(c("red", "blue"))( length(zz))
    for (i in 1:length(zz)) {
        tmp<-zz[[i]]
        lines(tmp[,3],tmp[,5],col=cols[i],lwd=2,lty=2)
    }
    legend("topright",bty='n',paste("N=",unique(tmp[,1]),sep=''))
    abline(h=.05,col='gray')
}
par(mar=rep(0,4))
plot(NULL,xlim=0:1,ylim=0:1,bty='n',xaxt='n',yaxt='n',xlab='',ylab='')
legend(xjust=.5,yjust=.5,x=.3,y=0.5,bty='n',names(zz),fill=cols,title=expression(rho))
legend(xjust=.5,yjust=.5,x=.1,y=0.5,bty='n',lwd=2,lty=c(2),c("Linear"))
dev.off()

tab <-
structure(c(250, 1000, 5000, 250, 1000, 5000, 250, 1000, 5000, 
250, 1000, 5000, 250, 1000, 5000, 250, 1000, 5000, 250, 1000, 
5000, 250, 1000, 5000, 250, 1000, 5000, 250, 1000, 5000, 250, 
1000, 5000, 250, 1000, 5000, 250, 1000, 5000, 250, 1000, 5000, 
250, 1000, 5000, 250, 1000, 5000, 250, 1000, 5000, 250, 1000, 
5000, 250, 1000, 5000, 250, 1000, 5000, 250, 1000, 5000, 250, 
1000, 5000, 250, 1000, 5000, 250, 1000, 5000, 250, 1000, 5000, 
250, 1000, 5000, 250, 1000, 5000, 250, 1000, 5000, 250, 1000, 
5000, 250, 1000, 5000, 250, 1000, 5000, 250, 1000, 5000, 250, 
1000, 5000, 250, 1000, 5000, 250, 1000, 5000, 250, 1000, 5000, 
250, 1000, 5000, 250, 1000, 5000, 250, 1000, 5000, 250, 1000, 
5000, 0, 0, 0, 0.5, 0.5, 0.5, 0, 0, 0, 0.5, 0.5, 0.5, 0, 0, 0, 
0.5, 0.5, 0.5, 0, 0, 0, 0.5, 0.5, 0.5, 0, 0, 0, 0.5, 0.5, 0.5, 
0, 0, 0, 0.5, 0.5, 0.5, 0, 0, 0, 0.5, 0.5, 0.5, 0, 0, 0, 0.5, 
0.5, 0.5, 0, 0, 0, 0.5, 0.5, 0.5, 0, 0, 0, 0.5, 0.5, 0.5, 0, 
0, 0, 0.5, 0.5, 0.5, 0, 0, 0, 0.5, 0.5, 0.5, 0, 0, 0, 0.5, 0.5, 
0.5, 0, 0, 0, 0.5, 0.5, 0.5, 0, 0, 0, 0.5, 0.5, 0.5, 0, 0, 0, 
0.5, 0.5, 0.5, 0, 0, 0, 0.5, 0.5, 0.5, 0, 0, 0, 0.5, 0.5, 0.5, 
0, 0, 0, 0.5, 0.5, 0.5, 0, 0, 0, 0.5, 0.5, 0.5, 0.1, 0.1, 0.1, 
0.1, 0.1, 0.1, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.3, 0.3, 0.3, 0.3, 
0.3, 0.3, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.5, 0.5, 0.5, 0.5, 0.5, 
0.5, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 
0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 1, 
1, 1, 1, 1, 1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.2, 1.2, 1.2, 1.2, 
1.2, 1.2, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.4, 1.4, 1.4, 1.4, 1.4, 
1.4, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.6, 1.6, 1.6, 1.6, 1.6, 1.6, 
1.7, 1.7, 1.7, 1.7, 1.7, 1.7, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 1.9, 
1.9, 1.9, 1.9, 1.9, 1.9, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0.921, 1, 1, 1, 1, 1, 0.532, 0.984, 
1, 0.929, 1, 1, 0.323, 0.829, 1, 0.71, 0.998, 1, 0.198, 0.596, 
0.996, 0.468, 0.973, 1, 0.167, 0.422, 0.973, 0.329, 0.861, 1, 
0.113, 0.335, 0.895, 0.254, 0.719, 1, 0.108, 0.239, 0.819, 0.182, 
0.586, 0.997, 0.091, 0.202, 0.707, 0.175, 0.485, 0.989, 0.077, 
0.15, 0.562, 0.122, 0.401, 0.962, 0.071, 0.165, 0.535, 0.121, 
0.338, 0.946, 0.065, 0.13, 0.442, 0.124, 0.296, 0.887, 0.064, 
0.103, 0.366, 0.104, 0.251, 0.819, 0.068, 0.114, 0.329, 0.09, 
0.209, 0.732, 0.064, 0.084, 0.284, 0.065, 0.176, 0.685, 0.043, 
0.083, 0.251, 0.081, 0.163, 0.609, 0.062, 0.097, 0.239, 0.091, 
0.165, 0.578, 0.042, 0.095, 0.237, 0.065, 0.151, 0.522, 0.05, 
0.072, 0.207, 0.07, 0.135, 0.489, 0.056, 0.075, 0.201, 0.069, 
0.126, 0.433, 0.055, 0.074, 0.181, 0.065, 0.103, 0.402), .Dim = c(120L, 
5L))


## #b3!=0
## N<-c(250,1000,5000)
## rho<-c(0,.5) #seq(0,.3,by=.1)
## lambda<-1#seq(.1,2,by=.1)
## alpha<-0
## b3<-seq(-.15,.15,by=.01)
## pars<-expand.grid(N=N,rho=rho,lambda=lambda,alpha=alpha,b3=b3)
## tmp<-list()
## for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
## pars<-tmp

## simfun<-function(pars,b1=1,b2=1,s2=1) {
##     for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
##     std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
##     library(MASS)
##     ##inside
##     sig<-numeric()
##     for (i in 1:1000) {
##         xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
##         y<-b1*xz[,1]+b2*xz[,2]+rnorm(N,sd=sqrt(s2))+b3*xz[,1]*xz[,2]
##         y<-std(y)
##         y<-1.05^((y-alpha)/lambda)
##         y<-std(y)
##         ##
##         df<-data.frame(y=y,x=xz[,1],z=xz[,2])
##         m<-lm(y~x*z,df)
##         pv<-summary(m)$coef[4,4]
##         sig[i]<-ifelse(pv<.05,1,0)
##     }
##     c(N,b3,rho,lambda,alpha,mean(sig))
## }
## library(parallel)
## out<-mclapply(pars,simfun,mc.cores=25)
## tab<-do.call("rbind",out)


## pdf("/home/bd/Dropbox/Apps/Overleaf/Interaction_problems/transform2.pdf",width=8,height=3)
## source("/home/bd/Dropbox/projects/interaction_problems/src/000_functions.R")
## L<-split(data.frame(tab),tab[,1])
## par(mgp=c(2,1,0),mfrow=c(1,3),mar=c(3,3,1,1))
## for (ii in 1:length(L)) {
##     zz<-split(L[[ii]],L[[ii]][,3])
##     plot(NULL,xlim=range(tab[,2]),ylim=0:1,xlab=expression(b[3]),ylab='Power')
##     vert()
##     cols<-colorRampPalette(c("red", "blue"))( length(zz))
##     for (i in 1:length(zz)) {
##         tmp<-zz[[i]]
##         lines(tmp[,2],tmp[,6],col=cols[i],lwd=2)
##     }
##     legend("bottomleft",bty='n',paste("N=",unique(tmp[,1]),sep=''))
##     legend("bottomright",bty='n',names(zz),fill=cols,title=expression(rho))
##     abline(h=.8,col='gray')
## }
## dev.off()

