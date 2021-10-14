##b3=0
N<-1000
rho<-0 #.3#rho<-c(0,.5) #seq(0,.3,by=.1)
b0<-c(0,1,2,4) #seq(0,3,length.out=3)
pars<-expand.grid(N=N,rho=rho,b0=b0)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp


simfun<-function(pars,b1=1,b2=1,s2=1) {
    for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
    std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
    library(MASS)
    sig<-r2<-numeric()
    xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
    y<-b0+b1*xz[,1]+b2*xz[,2]
    sigma<-function(z) 1/(1+exp(-z))
    p<-sigma(y)
    y<-rbinom(N,1,p)
    ##
    df<-data.frame(y=y,x=xz[,1],z=xz[,2],ystar=p)
}
L<-lapply(pars,simfun)

pdf("/home/bd/Dropbox/Apps/Overleaf/Interaction_problems/binary_intuition.pdf",width=6,height=2.3)
par(mfrow=c(1,4),mgp=c(2,1,0),mar=c(3,3,1.8,0.2))
for (i in 1:length(L)) {
    z<-L[[i]]
    col.test<-ifelse(abs(z$z-1)<.5,2,1)
    col.test<-ifelse(abs(z$z+1)<.5,3,col.test)
    cc<-col2rgb("lightgray")
    c1<-rgb(cc[1],cc[2],cc[3],max=255,alpha=75)
    cc<-col2rgb("blue")
    c2<-rgb(cc[1],cc[2],cc[3],max=255,alpha=75)
    cc<-col2rgb("red")
    c3<-rgb(cc[1],cc[2],cc[3],max=255,alpha=75)
    col<-c(c1,c2,c3)[col.test]
    plot(z$x,z$ystar,pch=19,cex=.375,col=col,xlab='x',ylab='E(y)',ylim=c(0,1))
    xv<-seq(-3,3,length.out=100)
    m0<-glm(y~x*z,z,family='binomial')
    y<-predict(m0,data.frame(x=xv,z=-1),type='response')
    lines(xv,y,col='red',lty=1,lwd=3)
    y<-predict(m0,data.frame(x=xv,z=1),type='response')
    lines(xv,y,col='blue',lty=1,lwd=3)
    m<-lm(y~x*z,z)
    print(coef(m))
    y<-predict(m,data.frame(x=xv,z=-1))
    lines(xv,y,col='red',lty=2,lwd=3)
    y<-predict(m,data.frame(x=xv,z=1))
    lines(xv,y,col='blue',lty=2,lwd=3)
    if (i==1) {
        legend("topleft",bty='n',ncol=,c("z=-1","z=+1"),fill=c("red","blue"))
    } 
    b0<-as.character(round(pars[[i]]$b0,2))
    mtext(side=3,line=0,bquote(beta[0]~"="~.(b0)))
    ## legend("bottomright",bty='n',lty=c(2,1),lwd=3,
    ##        c(paste("p(lm)=",round(summary(m)$coef[4,4],2)),
    ##          paste("p(glm)=",round(summary(m0)$coef[4,4],2)))
    ##        )
}
dev.off()


pdf("/home/bd/Dropbox/Apps/Overleaf/Interaction_problems/binary_kidney.pdf",width=6,height=2.3)
par(mfrow=c(1,4),mgp=c(2,1,0),mar=c(3,3,1.8,0.2))
for (i in 1:length(L)) {
    z<-L[[i]]
    cc<-col2rgb("lightgray")
    c2<-rgb(cc[1],cc[2],cc[3],max=255,alpha=105)
    plot(z$x*z$z,z$y,pch=19,cex=0,col=c2,ylim=c(0,1.3),xlab='x*z',ylab='E(y)',yaxt='n')
    axis(side=2,at=c(0,.5,1))
    z2<-z
    cc<-col2rgb("red")
    c1<-rgb(cc[1],cc[2],cc[3],max=255,alpha=105)
    points(z2$x*z2$z,z2$ystar,pch=19,cex=.75,col=c1)
    m<-lm(y~x*z,z)
    #legend("bottomleft",bty='n',legend=paste("pv=",round(summary(m)$coef[4,4],3)))
    #cc<-round(pars[[i]]$cc,2)
    #mtext(side=3,line=0,bquote("c="~.(cc)))
    c1<-cor(z$x*z$z,z$y)
    c2<-cor(z$x*z$z,z$ystar)
    legend("topleft",bty='n',#fill=c("red","lightgray"),
           c(paste("r=",round(c1,2))
                                                        #,paste("r(ystar,x*z)=",round(c2,2))
                                                         ))
    b0<-as.character(round(pars[[i]]$b0,2))
    mtext(side=3,line=0,bquote(beta[0]~"="~.(b0)))
}
dev.off()


## par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(3,3,1,1))
## for (i in 1:length(L)) {
##     z<-L[[i]]
##     col.test<-ifelse(abs(z$z-1)<.5,2,1)
##     col.test<-ifelse(abs(z$z+1)<.5,3,col.test)
##     cc<-col2rgb("lightgray")
##     c1<-rgb(cc[1],cc[2],cc[3],max=255,alpha=75)
##     cc<-col2rgb("blue")
##     c2<-rgb(cc[1],cc[2],cc[3],max=255,alpha=75)
##     cc<-col2rgb("red")
##     c3<-rgb(cc[1],cc[2],cc[3],max=255,alpha=75)
##     col<-c(c1,c2,c3)[col.test]
##     plot(z$x*z$z,z$ystar,pch=19,cex=.375,col=col,xlab='x',ylab='E(y)')
##     xv<-seq(-3,3,length.out=100)
##     m0<-glm(y~x*z,z,family='binomial')
##     y<-predict(m0,data.frame(x=xv,z=-1),type='response')
##     #lines(xv,y,col='red',lty=1,lwd=3)
##     y<-predict(m0,data.frame(x=xv,z=1),type='response')
##     #lines(xv,y,col='blue',lty=1,lwd=3)
##     m<-lm(y~x*z,z)
##     y<-predict(m,data.frame(x=xv,z=-1))
##     #lines(xv,y,col='red',lty=2,lwd=3)
##     y<-predict(m,data.frame(x=xv,z=1))
##     #lines(xv,y,col='blue',lty=2,lwd=3)
##     if (i==1) {
##         #legend("topleft",bty='n',ncol=2,c("z=-1","z=+1","y","ystar"),lty=c(1,2,NA,NA),pch=c(NA,NA,19,19),col=c("black","black","red","gray"),title=paste("b0=",round(pars[[i]]$b0,2)))
##         legend("topleft",bty='n',ncol=,c("z=-1","z=+1"),fill=c("red","blue"),title=paste("b0=",round(pars[[i]]$b0,2)))
##     } else {
##         legend("topleft",bty='n',legend=NA,title=paste("b0=",round(pars[[i]]$b0,2)))
##     }        
##     legend("bottomright",bty='n',lty=c(2,1),lwd=3,c(paste("p(lm)=",round(summary(m)$coef[4,4],2)),paste("p(glm)=",round(summary(m0)$coef[4,4],2))))
## }








