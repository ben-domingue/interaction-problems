library(parallel)
simfun<-function(pars,sd.theta=1) {
    for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
    library(MASS)
    library(ordinal)
    std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
    out<-list()
    for (i in 1:1000) {
        theta<-sort(rnorm(J-1,sd=sd.theta))
        xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
        y<-b1*xz[,1]+b2*xz[,2]+b3*xz[,1]*xz[,2]
        #J<-(length(theta)+1)
        p<-matrix(0,nrow=N,ncol=J)
        sigma<-function(z) 1/(1+exp(-z))
        for (j in 1:(J-1)) {
            tmp<-theta[j]-y
            p[,j]<-sigma(tmp)
        }
        cum.p<-p
        for (j in 2:(J-1))  p[,j]<-cum.p[,j]-cum.p[,(j-1)]
        p[,J]<-1-rowSums(p)
        for (ii in 1:N) {
            v<-rmultinom(1,1,p[ii,])
            y[ii]<-which(v==1)
        }
        ##
        df<-data.frame(y=y,x=xz[,1],z=xz[,2])
        m<-clm("factor(y)~x*z",data=df)
        co<-summary(m)$coef
        iii<-grep("x:z",rownames(co))
        est<-co[iii,1]
        pv<-co[iii,4]
        sig<-ifelse(pv<.05,1,0)
        se<-co[iii,2]
        ##
        m<-lm(y~x*z,df)
        m.lm<-lm(y~x*z,df)
        pv<-summary(m.lm)$coef[4,4]
        sig.lm<-ifelse(pv<.05,1,0)
        est.lm<-summary(m.lm)$coef[4,1]
        se.lm<-summary(m.lm)$coef[4,2]
        out[[i]]<-c(N=N,rho=rho,b0=b0,b1=b1,b2=b2,sig=sig,sig.lm=sig.lm,est=est,est.lm=est.lm,J=J,se=se,se.lm=se.lm)
    }
    do.call("rbind",out)
}

##b3!=0
N<-c(250,1000,5000)
rho<-0 #c(0,.5) #rho<-seq(0,.3,by=.1)
b0<-0#c(0,2)
b1<-0.5
b2<-0.5 #seq(0,1,by=.25)
J<-seq(5,25,by=5)
b3<-0
pars<-expand.grid(N=N,rho=rho,b0=b0,b1=b1,b3=b3,b2=b2,J=J)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp
out<-mclapply(pars,simfun,mc.cores=25)
df<-data.frame(do.call("rbind",out))
save(df,file="/tmp/df_ordinal_bias.Rdata")

N<-round(seq(250,5000,length.out=25)) #c(250,500,1000,2000,4000,5000)
rho<-0 #c(0,.5) #rho<-seq(0,.3,by=.1)
b0<-0 #c(0,2)#seq(0,8,by=0.25)
b2<-0.5 #c(0,.25)#c(0,.5) #seq(0,1,by=.25)
b1<-0.5
b3<-0
J<-c(5,25) #5:25
pars<-expand.grid(N=N,rho=rho,b0=b0,b1=b1,b3=b3,b2=b2,J=J)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp
out<-mclapply(pars,simfun,mc.cores=25)
df<-data.frame(do.call("rbind",out))
save(df,file="/tmp/df_ordinal_power.Rdata")

#pdf("/home/bd/Dropbox/Apps/Overleaf/Interaction_problems/ordinal_bias.pdf",width=7,height=3.3)
par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(2,1,0),oma=rep(.5,4))
##
load("df_ordinal_bias.Rdata")
df$t.lm<-abs(df$est.lm)/df$se.lm
df$t<-abs(df$est)/df$se
df<-df[df$N!=1000,]
pf<-function(df,var,...) {
    df$yv<-df[[var]]
    L<-split(df,df$N)
    plot(NULL,xlim=c(5,25),ylim=c(0,3),xlab='J',...)
    cols<-colorRampPalette(c("red", "blue"))( length(unique(df$N)))
    for (i in 1:length(L)) {
        x<-L[[i]]
        cc<-col2rgb(cols[i])
        c1<-rgb(cc[1],cc[2],cc[3],max=255,alpha=45)
        #points(x$N,x$est.lm,pch=19,col=c1)
        l<-split(x,x$J)
        zz<-sapply(l,function(x) quantile(x$yv,c(.1,.5,.9)))
        nn<-as.numeric(colnames(zz))
        polygon(c(nn,rev(nn)),c(zz[1,],rev(zz[3,])),col=c1)
        lines(nn,zz[2,],col=cols[i],lwd=2)
        ##
        zz<-sapply(l,function(x) quantile(x$t,c(.1,.5,.9)))
        nn<-as.numeric(colnames(zz))
        lines(nn,zz[2,],lwd=2,col='black')
    }
    cols
}
cols<-pf(df,'t.lm',ylab=expression("LM SE"~beta[3]))
legend("topleft",bty='n',legend=unique(df$N),fill=cols,title="N")
#legend("topleft",bty='n',legend=expression(beta[1]))#~unique(df$b1)))
##
load("df_ordinal_power.Rdata")
df<-df[df$rho==0,]
df<-df[df$J %in% c(5,25),]
Jvals<-unique(df$J)
cols<-colorRampPalette(c("blue", "red"))( length(Jvals))
plot(NULL,xlim=c(0,max(df$N)),ylim=c(0,1),xlab="N",ylab="Type 1 error rate"); abline(h=0.05,col='gray')
for (ii  in 1:length(Jvals)) {
    J<-Jvals[ii]
    y<-df[df$J==J,]
    L<-split(y,y$rho)
    for (i in 1:length(L)) {
        x<-L[[i]]
        pow<-by(x$sig,x$N,mean)
        lines(as.numeric(names(pow)),as.numeric(pow),lty=1,lwd=2,col=cols[ii])
        pow<-by(x$sig.lm,x$N,mean)
        lines(as.numeric(names(pow)),as.numeric(pow),lty=2,lwd=2,col=cols[ii])
    }
}
legend(400,.8,bty='n',lty=c(1,1,2,2),lwd=2,col=rep(cols,2),c(
                                                                   expression("GLM estimate"~J~"=5"),
                                                                   expression("GLM estimate"~J~"=25"),
                                                                   expression("LM estimate"~J~"=5"),
                                                                   expression("LM estimate"~J~"=5")
                                                               )
)
#dev.off()
