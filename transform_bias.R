library(parallel)
simfun<-function(pars,s2=1) {
    for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
    std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
    library(MASS)
    ##inside
    out<-list()
    for (i in 1:1000) {
        xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
        y<-b1*xz[,1]+b2*xz[,2]+rnorm(N,sd=sqrt(s2))
        y<-std(y)
        y<-1.05^((y-alpha)/lambda)
        y<-std(y)
        ##
        df<-data.frame(y=y,x=xz[,1],z=xz[,2])
        m.lm<-lm(y~x*z,df)
        pv<-summary(m.lm)$coef[4,4]
        est.lm<-summary(m.lm)$coef[4,1]
        sig.lm<-ifelse(pv<.05,1,0)
        out[[i]]<-c(N=N,rho=rho,b0=b0,b3=b3,
                    #sig=sig,
                    sig.lm=sig.lm,
                    #est=est,
                    est.lm=est.lm,alpha=alpha,lambda=lambda)
    }
    do.call("rbind",out)
}

N<-c(250,1000) #round(seq(250,5000,length.out=25)) #c(250,500,1000,2000,4000,5000)
rho<-0 #c(0,.5) #rho<-seq(0,.3,by=.1)
b0<-0
b3<-0 #seq(0,0.3,by=.025)
b1<-b2<-1
lambda<-seq(.1,2,by=.1)
alpha<-0
pars<-expand.grid(N=N,rho=rho,b0=b0,b3=b3,alpha=alpha,lambda=lambda,b1=b1,b2=b2)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp
out<-mclapply(pars,simfun,mc.cores=25)
df<-data.frame(do.call("rbind",out))
save(df,file="/tmp/df_transform_bias.Rdata")


N<-round(seq(250,1000,length.out=50)) #c(250,500,1000,2000,4000,5000)
rho<-0 #c(0,.5) #rho<-seq(0,.3,by=.1)
b0<-0 #c(0,2)#seq(0,8,by=0.25)
b3<-0 #seq(0,0.3,by=.025)
b1<-b2<-1
lambda<-c(.5,1)  #seq(.1,2,by=.1)
alpha<-0
pars<-expand.grid(N=N,rho=rho,b0=b0,b3=b3,alpha=alpha,lambda=lambda,b1=b1,b2=b2)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp
out<-mclapply(pars,simfun,mc.cores=25)
df<-data.frame(do.call("rbind",out))
save(df,file="/tmp/df_transform_power.Rdata")


pdf("/home/bd/Dropbox/Apps/Overleaf/Interaction_problems/transform_bias.pdf",width=7,height=3.3)
par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(2,1,0),oma=rep(.5,4))
##
load("df_transform_bias.Rdata")
pf<-function(df,var,...) {
    df$yv<-df[[var]]
    L<-split(df,df$N)
    plot(NULL,xlim=c(0,2),ylim=range(df$yv),xlab=expression(lambda),...)
    cols<-colorRampPalette(c("red", "blue"))( length(unique(df$N)))
    for (i in 1:length(L)) {
        x<-L[[i]]
        cc<-col2rgb(cols[i])
        c1<-rgb(cc[1],cc[2],cc[3],max=255,alpha=45)
        #points(x$N,x$est.lm,pch=19,col=c1)
        l<-split(x,x$lambda)
        zz<-sapply(l,function(x) quantile(x$yv,c(.1,.5,.9)))
        nn<-as.numeric(colnames(zz))
        polygon(c(nn,rev(nn)),c(zz[1,],rev(zz[3,])),col=c1)
        lines(nn,zz[2,],col=cols[i],lwd=2)
        abline(h=0)
    }
    cols
}
cols<-pf(df,'est.lm',ylab=expression("LM estimate"~beta[3]))
legend("topleft",bty='n',legend=unique(df$N),fill=cols,title="N")
##
load("df_transform_power.Rdata")
df<-df[df$rho==0,]
lambdavals<-unique(df$lambda)
cols<-colorRampPalette(c("blue", "red"))( length(lambdavals))
plot(NULL,xlim=c(0,max(df$N)),ylim=c(0,1),xlab="N",ylab="Type 1 error rate"); abline(h=0.05,col='gray')
for (ii  in 1:length(lambdavals)) {
    lambda<-lambdavals[ii]
    y<-df[df$lambda==lambda,]
    L<-split(y,y$rho)
    for (i in 1:length(L)) {
        x<-L[[i]]
        #pow<-by(x$sig,x$N,mean)
        #lines(as.numeric(names(pow)),as.numeric(pow),lty=1,lwd=2,col=cols[ii])
        pow<-by(x$sig.lm,x$N,mean)
        lines(as.numeric(names(pow)),as.numeric(pow),lty=2,lwd=2,col=cols[ii])
    }
}
legend("topleft",bty='n',lty=c(2,2),lwd=2,col=rep(cols,1),c(
                                                                expression("LM estimate"~lambda~"=0.5"),
                                                                expression("LM estimate"~lambda~"=1")
                                                               )
       ,cex=.7)
mtext(side=4,line=0.1,expression(alpha),at=0.05,las=2)
dev.off()
