set.seed(83601031)
nrep<-250
ncore<-10
##
N<-log10(c(100,1e4))
N<-seq(N[1],N[2],length.out=40)
N<-round(10^N)
##
rho<-0
b0<-seq(0,3.5,by=.025)
b3<-0 #seq(0,0.3,by=.025)
pars<-expand.grid(N=N,rho=rho,b0=b0,b3=b3)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp

simfun<-function(pars,b1=1,b2=1) {
    for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
    library(MASS)
    r2<-sig<-sig.lm<-numeric()
    xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
    y<-b0+b1*xz[,1]+b2*xz[,2]+b3*xz[,1]*xz[,2]
    sigma<-function(z) 1/(1+exp(-z))
    p<-sigma(y)
    y<-rbinom(N,1,p)
    df<-data.frame(y=y,x=xz[,1],z=xz[,2])
    ##
    m.lm<-lm(y~x*z,
             #model=FALSE,x=FALSE,y=FALSE,qr=FALSE,
             df)
    pv<-summary(m.lm)$coef[4,4]
    sig.lm<-ifelse(pv<.05,1,0)
    c(N,b0,b3,mean(sig.lm))
}

library(parallel)
out<-list()
for (i in 1:nrep) {
    print(i/nrep)
    z<-mclapply(pars,simfun,mc.cores=ncore)
    out[[i]]<-do.call("rbind",z)
}
tab<-do.call("rbind",out)
z<-data.frame(tab)
names(z)<-c("N","b0","b3","p")
z$prev<-exp(tab[,2])/(1+exp(tab[,2]))
L<-split(z,paste(z$N,z$b0))
z<-data.frame(do.call("rbind",lapply(L,colMeans)))
z1<-z

###############different b1,b2
library(parallel)
out<-list()
for (i in 1:nrep) {
    print(i/nrep)
    z<-mclapply(pars,simfun,mc.cores=ncore,
                b1=.25,b2=.75)
    out[[i]]<-do.call("rbind",z)
}
tab<-do.call("rbind",out)
z<-data.frame(tab)
names(z)<-c("N","b0","b3","p")
z$prev<-exp(tab[,2])/(1+exp(tab[,2]))
L<-split(z,paste(z$N,z$b0))
z<-data.frame(do.call("rbind",lapply(L,colMeans)))
z2<-z

z<-list(z1,z2)
save(z,file="biasVpower.Rdata")

########################################################
load("biasVpower.Rdata")
z1<-z[[1]]
z2<-z[[2]]

cols1<-colorRampPalette(c("blue", "white"))(50)
cols2<-rev(colorRampPalette(c("red", "white"))(950))
cols<-c(cols1,cols2)
cols<-data.frame(col=cols,val=seq(0,1,length.out=length(cols)))
getcol<-function(val,cols) {
    del<-abs(val-cols$val)
    if (!all(is.na(del))) {
        index<-which.min(del)
        cols$col[index]
    } else NA
}
getcol<-Vectorize(getcol,"val")


pdf("/home/bd/Dropbox/Apps/Overleaf/Interaction_problems/biasVpower.pdf",height=3,width=6)
layout(matrix(c(1,1,1,2,2,2,
                1,1,1,2,2,2,
                1,1,1,2,2,2,
                3,3,3,3,3,3)
             ,nrow=4,ncol=6,byrow=TRUE))
par(mgp=c(2,1,0),mar=c(3,3,1.5,1),oma=rep(.5,4))
pf<-function(z) {
    z$col<-getcol(z$p,cols)
    plot(log10(z$N),z$prev,ylim=c(.5,1),
         type="n",xlab="N (log scale)",ylab="Prevalence",xaxt='n',yaxt='n',bty='n')
    points(log10(z$N),z$prev,pch=19,col=z$col,cex=.94)
    axis(side=2,at=c(.5,.75,1))
    xx<-c(100,1000,10000,100000,1e6)
    axis(side=1,at=log10(xx),xx)
}
pf(z1)
mtext(side=3,line=0,bquote(beta[1]~"="~beta[2]~"=1"))
pf(z2)
mtext(side=3,line=0,bquote(beta[1]~"=0.25, "~beta[2]~"=0.75"))
##color legend
par(mar=c(1,3,1,.5))
plot(ylim=c(-.4,.2),cols$val,rep(0,nrow(cols)),col=cols$col,pch=19,cex=.5,xaxt="n",yaxt="n",ylab="",xlab="",bty="n")
mtext(side=4,at=0,
      cex=.8,"1",las=2)
mtext(side=2,at=0,0,las=2)
mtext(side=1,"Type 1 error rate",line=0)
text(.05,0,pos='3',".05")
dev.off()
