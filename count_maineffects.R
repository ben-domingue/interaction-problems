library(parallel)
simfun<-function(pars) {
    for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
    library(MASS)
    out<-list()
    for (i in 1:100) {
        xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
        y<-b0+b1*xz[,1]+b2*xz[,2]+b3*xz[,1]*xz[,2]
        y<-exp(y)
        y<-rpois(N,y)
        df<-data.frame(y=y,x=xz[,1],z=xz[,2])
        ##
        m<-glm(y~x*z,df,family="poisson")
        m.lm<-lm(y~x*z,df)
        out[[i]]<-c(coef(m),coef(m.lm))
    }
    c(b0,b1,b2,b3,colMeans(do.call("rbind",out)))
}


##b3!=0
N<-250
rho<-0 #c(0,.5) #rho<-seq(0,.3,by=.1)
b0<-0#c(0,2)
b1<-0.5
b2<-seq(0,1,by=.25)
b3<-c(0,0.25)
pars<-expand.grid(N=N,rho=rho,b0=b0,b1=b1,b3=b3,b2=b2)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp
out<-mclapply(pars,simfun,mc.cores=2)


df<-data.frame(do.call("rbind",out))
par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
L<-split(df,df[,4])
for (i in 1:length(L)) {
    x<-L[[i]]
    plot(x[,3],x[,7],type='l',col='blue')
    lines(x[,3],x[,11],col='red')
}
