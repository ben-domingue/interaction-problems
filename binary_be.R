
##b3!=0
N<-c(1000)
rho<-.3#<-seq(0,.3,by=.1)
#b0<-c(0,2)
#b3<-seq(0,0.3,by=.025)
pars<-expand.grid(N=N,rho=rho,b0=seq(0,2,by=.5),b1=seq(0,1,by=.5),b2=seq(0,1,by=.5))
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp

simfun<-function(pars) {
    for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
    library(MASS)
    r2<-sig<-sig.lm<-numeric()
    b3<-0
    M<-0
    while (M<0) {
        for (i in 1:10) {
            xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
            y<-b0+b1*xz[,1]+b2*xz[,2]+b3*xz[,1]*xz[,2]
            sigma<-function(z) 1/(1+exp(-z))
            p<-sigma(y)
            y<-rbinom(N,1,p)
            df<-data.frame(y=y,x=xz[,1],z=xz[,2])
            ##
            m<-glm(y~x*z,df,family="binomial")
        #oos r2
        Noos<-10000
        xz<-mvrnorm(Noos,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
        y<-b0+b1*xz[,1]+b2*xz[,2]+b3*xz[,1]*xz[,2]
        p<-sigma(y)
        y<-rbinom(Noos,1,p)
        df<-data.frame(y=y,x=xz[,1],z=xz[,2])
        yhat<-predict(m,df,type='response')
        m0<-glm(y~x+z,df,family="binomial")
        y0hat<-predict(m0,df,type='response')
        r2[i]<- (var(y0hat-y)-var(yhat-y))/var(y)
    }
    c(N,rho,b0,b3,mean(sig),mean(sig.lm),mean(r2))
}
library(parallel)
out<-mclapply(pars,simfun,mc.cores=25)
tab<-do.call("rbind",out)
