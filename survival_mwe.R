ff<-function(pars,b1,b2,b3) {
    for (i in 1:length(pars)) assign(names(pars)[i],pars[[i]][1])
    ##
    h<-function(t,k,lambda) (k/lambda)*((t/lambda)^(k-1)) ##baseline hazard, see failure rate here https://en.wikipedia.org/wiki/Weibull_distribution#Cumulative_distribution_function
    S<-function(t,gamma,...) {
        ##integrating the hazard
        t0<-seq(0,t,length.out=1000)
        del<-mean(diff(t0))
        t0<-t0+del
        H0<-h(t0,k=k,lambda=lambda)*exp(gamma)
        H<-sum(H0*del)
        ##
        exp(-1*H) #see expression for survival function at top of p2 here http://people.stat.sfu.ca/~raltman/stat402/402L32.pdf
    }
    Sv<-Vectorize(S,"t")
    sample.time<-function(gamma,Sv,...) { #this will sample the observed time (with censoring) from the survival distribution
        qu<-runif(1)
        ff<-function(t,qu,...) abs(qu-S(t,gamma,k=k,lambda=lambda)) 
        t<-optim(10,ff,qu=qu,gamma=gamma,k=k,lambda=lambda,S=S,
                 method="Brent",
                 lower=0,upper=150)$par
        ifelse(t>100,100,t) #censoring
    }
    library(MASS)
    xz<-mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),2,2))
    gamma<-b1*xz[,1]+b2*xz[,2]+b3*xz[,1]*xz[,2]
    df<-data.frame(x=xz[,1],z=xz[,2])
    NA->df$t
    for (i in 1:length(gamma)) df$t[i]<-sample.time(gamma[i],
                                                    S=S,
                                                    #Sv=Sv,
                                                    k=k,lambda=lambda
                                                    )
    ifelse(df$t==100,0,1)->df$dead
    #print(table(df$dead))
    library("survival")
    m <- coxph(Surv(t, dead) ~ x*z, data = df)
    summary(m)$coef
}

N<-1000
rho<-0.3
b1<-.5
b2<-0
b3<-0
##baseline hazard parameters
k<-2
lambda<-60
#
pars<-c(N=1000,rho=rho)
est<-ff(pars,b1=b1,b2=b2,b3=b3) #note that coefficients in est should match b1,b2,b3
cbind(c(b1,b2,b3),est[,1])
