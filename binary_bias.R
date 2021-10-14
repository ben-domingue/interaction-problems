##You might consider flipping the x-axis on your plots for sample size. You could then choose three values of \beta_3 to plot, including one in which \beta_3 = 0, so you’d have one type-I error plot and then power plots for a couple of effect sizes. A benefit of doing this would be that the curves would now be power curves, which are familiar and intuitive, and the type-I error plots would immediately reveal conditions in which increasing N only makes the problem worse.




library(parallel)
simfun<-function(pars,b1=1,b2=1) {
    for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
    library(MASS)
    out<-list()
    for (i in 1:1000) {
        xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
        y<-b0+b1*xz[,1]+b2*xz[,2]+b3*xz[,1]*xz[,2]
        sigma<-function(z) 1/(1+exp(-z))
        p<-sigma(y)
        y<-rbinom(N,1,p)
        df<-data.frame(y=y,x=xz[,1],z=xz[,2])
        ##
        m<-glm(y~x*z,df,family="binomial")
        m0<-glm(y~x+z,df,family="binomial")
        pv<-summary(m)$coef[4,4]
        est<-summary(m)$coef[4,1]
        sig<-ifelse(pv<.05,1,0)
        ##
        m.lm<-lm(y~x*z,df)
        pv<-summary(m.lm)$coef[4,4]
        est.lm<-summary(m.lm)$coef[4,1]
        sig.lm<-ifelse(pv<.05,1,0)
        #oos r2
        Noos<-1000
        xz<-mvrnorm(Noos,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
        y<-b0+b1*xz[,1]+b2*xz[,2]+b3*xz[,1]*xz[,2]
        p<-sigma(y)
        y<-rbinom(Noos,1,p)
        df<-data.frame(y=y,x=xz[,1],z=xz[,2])
        yhat<-predict(m,df,type='response')
        y0hat<-predict(m0,df,type='response')
        r2<- (var(y0hat-y)-var(yhat-y))/var(y)
        out[[i]]<-c(N=N,rho=rho,b0=b0,b3=b3,sig=sig,sig.lm=sig.lm,r2=r2,est=est,est.lm=est.lm)
    }
    do.call("rbind",out)
}


N<-c(250,1000) #round(seq(250,5000,length.out=25)) #c(250,500,1000,2000,4000,5000)
rho<-0 #c(0,.5) #rho<-seq(0,.3,by=.1)
b0<-seq(0,8,by=0.25)
b3<-0 #seq(0,0.3,by=.025)
pars<-expand.grid(N=N,rho=rho,b0=b0,b3=b3)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp
out<-mclapply(pars,simfun,mc.cores=25)
df<-data.frame(do.call("rbind",out))
save(df,file="/tmp/df_binary_bias.Rdata")

N<-round(seq(250,1000,length.out=50)) #c(250,500,1000,2000,4000,5000)
rho<-c(0) #rho<-seq(0,.3,by=.1)
b0<-c(0,2)#seq(0,8,by=0.25)
b3<-0 #seq(0,0.3,by=.025)
pars<-expand.grid(N=N,rho=rho,b0=b0,b3=b3)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp
out<-mclapply(pars,simfun,mc.cores=25)
df<-data.frame(do.call("rbind",out))
save(df,file="/tmp/df_binary_power.Rdata")



pdf("/home/bd/Dropbox/Apps/Overleaf/Interaction_problems/binary_bias.pdf",width=7,height=3.3)
par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(2,1,0),oma=rep(.5,4))
##
load("df_binary_bias.Rdata")
pf<-function(df,var,...) {
    df$yv<-df[[var]]
    L<-split(df,df$N)
    plot(NULL,xlim=c(0,max(df$b0)),ylim=range(df$yv),xlab=expression(beta[0]),...)
    cols<-colorRampPalette(c("red", "blue"))( length(unique(df$N)))
    for (i in 1:length(L)) {
        x<-L[[i]]
        cc<-col2rgb(cols[i])
        c1<-rgb(cc[1],cc[2],cc[3],max=255,alpha=45)
        #points(x$N,x$est.lm,pch=19,col=c1)
        l<-split(x,x$b0)
        zz<-sapply(l,function(x) quantile(x$yv,c(.1,.5,.9)))
        nn<-as.numeric(colnames(zz))
        polygon(c(nn,rev(nn)),c(zz[1,],rev(zz[3,])),col=c1)
        lines(nn,zz[2,],col=cols[i],lwd=2)
        abline(h=0)
    }
    cols
}
cols<-pf(df,'est.lm',ylab=expression("LM estimate"~beta[3]))
legend("topright",bty='n',legend=unique(df$N),fill=cols,title="N")
##
load("df_binary_power.Rdata")
df<-df[df$rho==0,]
b0vals<-unique(df$b0)
cols<-colorRampPalette(c("blue", "red"))( length(b0vals))
plot(NULL,xlim=c(0,max(df$N)),ylim=c(0,1),xlab="N",ylab="Type 1 error rate"); abline(h=0.05,col='gray')
for (ii  in 1:length(b0vals)) {
    b0<-b0vals[ii]
    y<-df[df$b0==b0,]
    L<-split(y,y$rho)
    for (i in 1:length(L)) {
        x<-L[[i]]
        pow<-by(x$sig,x$N,mean)
        lines(as.numeric(names(pow)),as.numeric(pow),lty=1,lwd=2,col=cols[ii])
        pow<-by(x$sig.lm,x$N,mean)
        lines(as.numeric(names(pow)),as.numeric(pow),lty=2,lwd=2,col=cols[ii])
    }
}
legend(250,.6,bty='n',lty=c(1,1,2,2),lwd=2,col=rep(cols,2),cex=.8,c(
                                                                   expression("GLM estimate"~beta[0]~"=0"),
                                                                   expression("GLM estimate"~beta[0]~"=2"),
                                                                   expression("LM estimate"~beta[0]~"=0"),
                                                                   expression("LM estimate"~beta[0]~"=2")
                                                               )
       )
mtext(side=4,line=0.1,expression(alpha),at=0.05,las=2)
dev.off()
