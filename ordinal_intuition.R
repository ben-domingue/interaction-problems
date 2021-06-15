##b3=0
N<-1000
rho<-0 #.3#rho<-c(0,.5) #seq(0,.3,by=.1)
J<-c(5,10,15,20)
pars<-expand.grid(N=N,rho=rho,J=J)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp


simfun<-function(pars,b1=.25,b2=.25) {
    for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
    std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
    library(MASS)
    sig<-r2<-numeric()
    xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
    theta<-sort(rnorm(J-1))
    y<-b1*xz[,1]+b2*xz[,2]
    J<-(length(theta)+1)
    p<-matrix(0,nrow=N,ncol=J)
    sigma<-function(z) 1/(1+exp(-z))
    for (j in 1:(J-1)) {
        tmp<-theta[j]-y
        p[,j]<-sigma(tmp)
    }
    cum.p<-p
    for (j in 2:(J-1))  p[,j]<-cum.p[,j]-cum.p[,(j-1)]
    p[,J]<-1-rowSums(p)
    ystar<-y
    for (ii in 1:N) {
        v<-rmultinom(1,1,p[ii,])
        y[ii]<-which(v==1)
    }
    ##
    df<-data.frame(y=y,x=xz[,1],z=xz[,2],ystar=ystar)
}
L<-lapply(pars,simfun)


par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(3,3,1,1))
library(ordinal)
for (i in 1:length(L)) {
    z<-L[[i]]
    cc<-col2rgb("lightgray")
    c1<-rgb(cc[1],cc[2],cc[3],max=255,alpha=45)
    plot(z$x,z$ystar,pch=19,cex=.575,col=c1,xlab='x',ylab='y')
    cc<-col2rgb("red")
    c2<-rgb(cc[1],cc[2],cc[3],max=255,alpha=45)
    #points(z$x,z$y,pch=19,cex=.5,col=c2)
    xv<-seq(-3,3,length.out=100)
    m0<-clm(factor(y)~x+z+x*z,data=z)
    beta<-m0$beta
    zz<- -1
    y<-beta[1]*xv+beta[2]*zz+beta[3]*xv*zz #y<-predict(m0,data.frame(x=xv,z=-1),type='linear.predictor')
    lines(xv,y,col='lightgray',lty=1,lwd=3)
    #y<-predict(m0,data.frame(x=xv,z=1),type='response')
    zz<- 1
    y<-beta[1]*xv+beta[2]*zz+beta[3]*xv*zz #y<-predict(m0,data.frame(x=xv,z=-1),type='linear.predictor')
    lines(xv,y,col='lightgray',lty=2,lwd=3)
    m<-lm(y~x*z,z)
    y<-predict(m,data.frame(x=xv,z=-1))
    lines(xv,y,col='red',lty=1,lwd=2)
    y<-predict(m,data.frame(x=xv,z=1))
    lines(xv,y,col='red',lty=2,lwd=2)
    ## if (i==1) {
    ##     legend("topleft",bty='n',ncol=2,c("z=-1","z=+1","y","ystar"),lty=c(1,2,NA,NA),pch=c(NA,NA,19,19),col=c("black","black","red","gray"),title=paste("b0=",round(pars[[i]]$b0,2)))
    ## } else {
    ##     legend("topleft",bty='n',legend=NA,title=paste("b0=",round(pars[[i]]$b0,2)))
    ## }        
    ## legend("bottomright",bty='n',fill=c("red","lightgray"),c(paste("pv_lm=",round(summary(m)$coef[4,4],2)),paste("pv_glm=",round(summary(m0)$coef[4,4],2))))
}
