##b3=0
N<-1000
rho<-0 #.3#rho<-c(0,.5) #seq(0,.3,by=.1)
cc<-seq(-3,-1,length.out=3)
pars<-expand.grid(N=N,rho=rho,cc=cc)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp


simfun<-function(pars,b1=1,b2=1,s2=1) {
    for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
    std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
    library(MASS)
    sig<-r2<-numeric()
    xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
    y<-b1*xz[,1]+b2*xz[,2]+rnorm(N,sd=sqrt(s2))
    y<-std(y)
    ystar<-y
    y<-ifelse(y>cc,y,cc)
    ##
    df<-data.frame(y=y,x=xz[,1],z=xz[,2],ystar=ystar)
}
L<-lapply(pars,simfun)

pdf("/home/bd/Dropbox/Apps/Overleaf/Interaction_problems/floor_intuition.pdf",width=6,height=2.3)
par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1.8,0.2))
for (i in 1:length(L)) {
    z<-L[[i]]
    cc<-col2rgb("lightgray")
    c2<-rgb(cc[1],cc[2],cc[3],max=255,alpha=105)
    plot(z$x*z$z,z$y,pch=19,cex=.75,col=c2,ylim=c(-4,4),xlab='x*z',ylab='y')
    z2<-z[z$y!=z$ystar,]
    cc<-col2rgb("red")
    c1<-rgb(cc[1],cc[2],cc[3],max=255,alpha=105)
    points(z2$x*z2$z,z2$ystar,pch=19,cex=.75,col=c1)
    m<-lm(y~x*z,z)
    #legend("bottomleft",bty='n',legend=paste("pv=",round(summary(m)$coef[4,4],3)))
    cc<-round(pars[[i]]$cc,2)
    mtext(side=3,line=0,bquote("c="~.(cc)))
    c1<-cor(z$x*z$z,z$y)
    c2<-cor(z$x*z$z,z$ystar)
    legend("topleft",bty='n',fill=c("red","lightgray"),c(paste("r(y,x*z)=",round(c1,2)),paste("r(ystar,x*z)=",round(c2,2))))
}
dev.off()

## par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(3,3,1,1))
## for (i in 1:length(L)) {
##     z<-L[[i]]
##     cc<-col2rgb("lightgray")
##     c1<-rgb(cc[1],cc[2],cc[3],max=255,alpha=45)
##     plot(z$x,z$ystar,pch=19,cex=.75,col=c1,xlab='x',ylab='y')
##     cc<-col2rgb("red")
##     c2<-rgb(cc[1],cc[2],cc[3],max=255,alpha=45)
##     points(z$x,z$y,pch=19,cex=.5,col=c2)
##     xv<-seq(-3,3,length.out=100)
##     m<-lm(ystar~x*z,z)
##     y<-predict(m,data.frame(x=xv,z=-1))
##     lines(xv,y,col='lightgray',lty=1,lwd=3)
##     y<-predict(m,data.frame(x=xv,z=1))
##     lines(xv,y,col='lightgray',lty=2,lwd=3)
##     m<-lm(y~x*z,z)
##     y<-predict(m,data.frame(x=xv,z=-1))
##     lines(xv,y,col='red',lty=1,lwd=2)
##     y<-predict(m,data.frame(x=xv,z=1))
##     lines(xv,y,col='red',lty=2,lwd=2)
##     print(summary(m)$coef[4,])
##     legend("topleft",bty='n',title=paste("floor=",round(pars[[i]]$cc,2)),legend=paste("est=",round(summary(m)$coef[4,1],2)))
##     if (i==1) {
##         legend("bottomright",bty='n',ncol=2,c("z=-1","z=+1","y","ystar"),lty=c(1,2,NA,NA),pch=c(NA,NA,19,19),col=c("black","black","red","gray"))
##     }
## }

