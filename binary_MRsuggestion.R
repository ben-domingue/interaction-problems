##You might consider flipping the x-axis on your plots for sample size. You could then choose three values of \beta_3 to plot, including one in which \beta_3 = 0, so you’d have one type-I error plot and then power plots for a couple of effect sizes. A benefit of doing this would be that the curves would now be power curves, which are familiar and intuitive, and the type-I error plots would immediately reveal conditions in which increasing N only makes the problem worse.




##b3!=0
N<-seq(250,5000,by=250) #c(250,500,1000,2000,4000,5000)
rho<-0 #c(0,.5) #rho<-seq(0,.3,by=.1)
b0<-c(0,2)
b3<-0 #seq(0,0.3,by=.025)
pars<-expand.grid(N=N,rho=rho,b0=b0,b3=b3)
tmp<-list()
for (i in 1:nrow(pars)) tmp[[i]]<-pars[i,]
pars<-tmp

simfun<-function(pars,b1=1,b2=1) {
    for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
    library(MASS)
    r2<-sig<-sig.lm<-numeric()
    for (i in 1:100) {
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
        sig[i]<-ifelse(pv<.05,1,0)
        ##
        m.lm<-lm(y~x*z,df)
        pv<-summary(m.lm)$coef[4,4]
        sig.lm[i]<-ifelse(pv<.05,1,0)
        #oos r2
        Noos<-1000
        xz<-mvrnorm(Noos,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
        y<-b0+b1*xz[,1]+b2*xz[,2]+b3*xz[,1]*xz[,2]
        p<-sigma(y)
        y<-rbinom(Noos,1,p)
        df<-data.frame(y=y,x=xz[,1],z=xz[,2])
        yhat<-predict(m,df,type='response')
        y0hat<-predict(m0,df,type='response')
        r2[i]<- (var(y0hat-y)-var(yhat-y))/var(y)
    }
    c(N,rho,b0,b3,mean(sig),mean(sig.lm),mean(r2))
}
library(parallel)
out<-mclapply(pars,simfun,mc.cores=25)
tab<-do.call("rbind",out)
dump("tab","")

tab <-
structure(c(250, 500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 
2500, 2750, 3000, 3250, 3500, 3750, 4000, 4250, 4500, 4750, 5000, 
250, 500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 
3000, 3250, 3500, 3750, 4000, 4250, 4500, 4750, 5000, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 2, 2, 
2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.04, 0.05, 0.07, 0.03, 0.06, 
0.05, 0.05, 0.03, 0.04, 0.07, 0.05, 0.07, 0.03, 0.07, 0.06, 0.06, 
0.03, 0.08, 0.04, 0.05, 0.06, 0.05, 0.06, 0.04, 0.01, 0.07, 0.02, 
0.06, 0.08, 0.03, 0.05, 0.07, 0.06, 0.05, 0.06, 0.06, 0.06, 0.03, 
0.05, 0.06, 0.02, 0.04, 0.04, 0.03, 0.04, 0.01, 0.01, 0.05, 0.03, 
0.04, 0.03, 0.05, 0.02, 0.03, 0.04, 0.04, 0.01, 0.04, 0.03, 0.06, 
0.66, 0.81, 0.96, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
1, 1, -0.00352480102995835, -0.00145262130094164, -0.00093567026354852, 
-0.000439501511300971, -0.00069642452873323, -0.000571428689272975, 
-0.000140606304276818, -0.000330649238714459, -0.000326482404241878, 
-0.000316318996364668, -0.000168432748008737, -0.000147034932893529, 
-0.000292324664175885, -0.000214723267188706, -0.000153639335129222, 
-0.000112843975496508, -0.000277199527271654, -0.000176878700201699, 
-0.000232330216981293, -0.000219601943840969, -0.00456033064563261, 
-0.00237060046030083, -0.00239090603643837, -0.00076942836828049, 
-0.000510027273345904, -0.000767561381958232, -0.000846824263049982, 
-0.000615739726508871, -0.000447570565385626, -0.000554673405610322, 
-0.000140085412349277, -0.000435536691915398, -0.000528242695466321, 
-0.000326152316031624, -0.000441717296182869, -0.000121556506594582, 
-0.000308594168858668, -0.000280746160566165, -0.000447339303244048, 
-2.30953887264334e-05), .Dim = c(40L, 7L))

source("/home/bd/Dropbox/projects/interaction_problems/src/000_functions.R")
par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1.5,.5))
##
## plot(NULL,xlim=range(tab[,4]),ylim=0:1,xlab=expression(beta[3]),ylab='Discovery Rate',bty='n')
## Nvals<-c(250,1000,5000)
## cols<-colorRampPalette(c("red", "blue"))( length(Nvals))
## for (i in 1:length(Nvals)) {
##     N<-Nvals[i]
##     zz<-tab[tab[,1]==N,]
##     lines(zz[,4],zz[,5],col=cols[i],lwd=2)
## }
## legend("topleft",legend=Nvals,fill=cols,bty='n',title="N")
## plot(NULL,xlim=range(tab[,4]),ylim=0:1,xlab=expression(beta[3]),ylab='Discovery Rate',bty='n')
## for (i in 1:length(Nvals)) {
##     N<-Nvals[i]
##     zz<-tab[tab[,1]==N,]
##     lines(zz[,4],zz[,6],col=cols[i],lwd=2,lty=2)
## }
##
hold<-tab
zz<-tab[,1]
tab[,1]<-tab[,4]
tab[,4]<-zz
cols<-'red'
plot(NULL,xlim=c(0,max(tab[,4])),ylim=0:1,xlab='N',ylab='False Discovery Rate',bty='n',main="b3=0")
legend("topleft",bty='n',lty=c(1,2),legend=c("Logistic","Linear"),lwd=2)
abline(h=0.05,lwd=.7)
zz<-tab[tab[,3]==0,]
lines(zz[,4],zz[,5],col=cols,lwd=2)
lines(zz[,4],zz[,6],col=cols,lwd=2,lty=2)
plot(NULL,xlim=c(0,max(tab[,4])),ylim=0:1,xlab='N',ylab='False Discovery Rate',bty='n',main="b3=2")
abline(h=0.05,lwd=.7)
zz<-tab[tab[,3]==2,]
lines(zz[,4],zz[,5],col=cols,lwd=2)
lines(zz[,4],zz[,6],col=cols,lwd=2,lty=2)

