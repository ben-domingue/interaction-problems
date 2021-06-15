N<-c(250)#,1000)#,5000)
rho<-c(0,.5) #rho<-seq(0,.3,by=.1)
pars<-expand.grid(N=N,rho=rho,b1=0,s2=c(1,4,9))
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp


simfun<-function(b3,pars,
                 #b1=1,
                 b2=1,
                 #s2=1,
                 niter=200) {
    set.seed(10103101)
    for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
    std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
    library(MASS)
    sig<-numeric()
    for (i in 1:niter) {
        xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
        y<-b1*xz[,1]+b2*xz[,2]+b3*xz[,1]*xz[,2]+rnorm(N,sd=sqrt(s2))
        y<-std(y)
        df<-data.frame(y=y,x=xz[,1],z=xz[,2])
        ##
        m<-lm(y~x*z,df)
        pv<-summary(m)$coef[4,4]
        sig[i]<-ifelse(pv<.05,1,0)
    }
    #print(sig)
    mean(sig)
}


get.min<-function(pars) {
    pow<-0
    b3<-0
    del<-.1
    while(pow<.8 | del>.00001) {
        pow<-simfun(b3,pars=pars)
        print(c(b3,pow))
        if (pow>.8) {
            b3<-b3-del
            del<-del/10
        }
        b3<-b3+del
    }
    b3
}

tab<-lapply(pars,get.min)

tab0<-do.call("rbind",pars)
tab<-cbind(tab0,unlist(tab))
tab<-data.frame(tab)
names(tab)[5]<-'b3'

par(mar=c(3,3,1,5),mgp=c(2,1,0))
plot(NULL,xlim=c(0,max(tab$s2)),ylim=c(0,max(tab$b3)),bty='n',xlab='s2',ylab='b3')
L<-split(tab,paste(tab$rho))
for (i in 1:length(L)) {
    z<-L[[i]]
    z<-z[order(z$s2),]
    lines(z$s2,z$b3)
    n<-nrow(z)
    text(z$s2[n],z$b3[n],names(L)[i],pos=4,xpd=TRUE)
}
