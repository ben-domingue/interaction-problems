

library(parallel)
simfun<-function(pars,b1=1,b2=1) {
    for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
    library(MASS)
    out<-list()
    for (i in 1:500) {
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
        out[[i]]<-c(N=N,rho=rho,b0=b0,b3=b3,sig=sig,sig.lm=sig.lm,r2=r2,est=est,est.lm=est.lm,b1=b1,b2=b2)
    }
    do.call("rbind",out)
}


N<-c(5000) #round(seq(250,5000,length.out=25)) #c(250,500,1000,2000,4000,5000)
rho<-0 #c(0,.5) #rho<-seq(0,.3,by=.1)
b0<-seq(0,8,by=0.25)
b3<-0 #seq(0,0.3,by=.025)
pars<-expand.grid(N=N,rho=rho,b0=b0,b3=b3)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp
out<-mclapply(pars,simfun,mc.cores=2,b1=.25,b2=.5)
df<-data.frame(do.call("rbind",out))
df1<-df

##
N<-c(1000) #round(seq(250,5000,length.out=25)) #c(250,500,1000,2000,4000,5000)
rho<-0 #c(0,.5) #rho<-seq(0,.3,by=.1)
b0<-seq(0,8,by=0.25)
b3<-0 #seq(0,0.3,by=.025)
pars<-expand.grid(N=N,rho=rho,b0=b0,b3=b3)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp
out<-mclapply(pars,simfun,mc.cores=2,b1=.75,b2=2)
df<-data.frame(do.call("rbind",out))
df2<-df


##
N<-c(500) #round(seq(250,5000,length.out=25)) #c(250,500,1000,2000,4000,5000)
rho<-0 #c(0,.5) #rho<-seq(0,.3,by=.1)
b0<-seq(0,8,by=0.25)
b3<-0 #seq(0,0.3,by=.025)
pars<-expand.grid(N=N,rho=rho,b0=b0,b3=b3)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp
out<-mclapply(pars,simfun,mc.cores=2,b1=-1,b2=1)
df<-data.frame(do.call("rbind",out))
df3<-df


par(mfrow=c(1,3),mar=c(3,3.5,1,1),mgp=c(2,1,0),oma=rep(.5,4))
##
pf<-function(df,var,...) {
    df$yv<-df[[var]]
    L<-split(df,df$N)
    plot(NULL,xlim=c(0,max(df$b0)),ylim=c(-.1,.1),xlab=expression(beta[0]),...)
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
    legend("topright",bty='n',legend=c(paste("N=",unique(df$N),sep=''),
                                       paste("beta_1=",unique(df$b1),sep=''),
                                       paste("beta_2=",unique(df$b2),sep='')
                                       )
       )
}
##
cols<-pf(df1,'est.lm',ylab=expression("LM estimate"~beta[3]^LM))
cols<-pf(df2,'est.lm',ylab=expression("LM estimate"~beta[3]^LM))
cols<-pf(df3,'est.lm',ylab=expression("LM estimate"~beta[3]^LM))
