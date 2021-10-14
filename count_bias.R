library(parallel)
simfun<-function(pars) {
    for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
    library(MASS)
    out<-list()
    for (i in 1:1000) {
        xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
        y<-b0+b1*xz[,1]+b2*xz[,2]+b3*xz[,1]*xz[,2]
        y<-exp(y)
        y<-rpois(N,y)
        df<-data.frame(y=y,x=xz[,1],z=xz[,2])
        ##
        m<-glm(y~x*z,df,family="poisson")
        pv<-summary(m)$coef[4,4]
        sig<-ifelse(pv<.05,1,0)
        est<-summary(m)$coef[4,1]
        m0<-glm(y~x+z,df,family="poisson")
        m.lm<-lm(y~x*z,df)
        pv<-summary(m.lm)$coef[4,4]
        sig.lm<-ifelse(pv<.05,1,0)
        est.lm<-summary(m.lm)$coef[4,1]
        ##
        Noos<-1000
        xz<-mvrnorm(Noos,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
        y<-b1*xz[,1]+b2*xz[,2]+b3*xz[,1]*xz[,2]
        y<-exp(y)
        y<-rpois(Noos,y)
        df<-data.frame(y=y,x=xz[,1],z=xz[,2])
        yhat<-predict(m,df,type='response')
        y0hat<-predict(m0,df,type='response')
        r2<- (var(y0hat-y)-var(yhat-y))/var(y)
        out[[i]]<-c(N=N,rho=rho,b0=b0,b1=b1,b2=b2,sig=sig,sig.lm=sig.lm,r2=r2,est=est,est.lm=est.lm)
    }
    do.call("rbind",out)
}


##b3!=0
N<-c(250,1000)
rho<-0 #c(0,.5) #rho<-seq(0,.3,by=.1)
b0<-0#c(0,2)
b1<-0.5
b2<-seq(0,1,by=.25)
b3<-0
pars<-expand.grid(N=N,rho=rho,b0=b0,b1=b1,b3=b3,b2=b2)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp
out<-mclapply(pars,simfun,mc.cores=25)
df<-data.frame(do.call("rbind",out))
save(df,file="/tmp/df_count_bias.Rdata")

N<-round(seq(250,1000,length.out=50)) #c(250,500,1000,2000,4000,5000)
rho<-0 #c(0,.5) #rho<-seq(0,.3,by=.1)
b0<-0 #c(0,2)#seq(0,8,by=0.25)
b2<-c(0,.25)#c(0,.5) #seq(0,1,by=.25)
b1<-0.5
b3<-0
pars<-expand.grid(N=N,rho=rho,b0=b0,b1=b1,b3=b3,b2=b2)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp
out<-mclapply(pars,simfun,mc.cores=25)
df<-data.frame(do.call("rbind",out))
save(df,file="/tmp/df_count_power.Rdata")

pdf("/home/bd/Dropbox/Apps/Overleaf/Interaction_problems/count_bias.pdf",width=7,height=3.3)
par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(2,1,0),oma=rep(.5,4))
##
load("df_count_bias.Rdata")
pf<-function(df,var,...) {
    df$yv<-df[[var]]
    L<-split(df,df$N)
    plot(NULL,xlim=c(0,max(df$b2)),ylim=c(-.5,2),xlab=expression(beta[2]),...)
    cols<-colorRampPalette(c("red", "blue"))( length(unique(df$N)))
    for (i in 1:length(L)) {
        x<-L[[i]]
        cc<-col2rgb(cols[i])
        c1<-rgb(cc[1],cc[2],cc[3],max=255,alpha=45)
        #points(x$N,x$est.lm,pch=19,col=c1)
        l<-split(x,x$b2)
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
#legend("topleft",bty='n',legend=expression(beta[1]))#~unique(df$b1)))
##
load("df_count_power.Rdata")
df<-df[df$rho==0,]
b2vals<-unique(df$b2)
cols<-colorRampPalette(c("blue", "red"))( length(b2vals))
plot(NULL,xlim=c(0,max(df$N)),ylim=c(0,1),xlab="N",ylab="Type 1 error rate"); abline(h=0.05,col='gray')
for (ii  in 1:length(b2vals)) {
    b2<-b2vals[ii]
    y<-df[df$b2==b2,]
    L<-split(y,y$rho)
    for (i in 1:length(L)) {
        x<-L[[i]]
        pow<-by(x$sig,x$N,mean)
        lines(as.numeric(names(pow)),as.numeric(pow),lty=1,lwd=2,col=cols[ii])
        pow<-by(x$sig.lm,x$N,mean)
        lines(as.numeric(names(pow)),as.numeric(pow),lty=2,lwd=2,col=cols[ii])
    }
}
legend(200,.5,bty='n',lty=c(1,1,2,2),lwd=2,col=rep(cols,2),cex=.8,c(
                                                                   expression("GLM estimate"~beta[2]~"=0"),
                                                                   expression("GLM estimate"~beta[2]~"=0.25"),
                                                                   expression("LM estimate"~beta[2]~"=0"),
                                                                   expression("LM estimate"~beta[2]~"=0.25")
                                                               )
)
mtext(side=4,line=0.1,expression(alpha),at=0.05,las=2)
dev.off()
