

demand<-function(p,lamb,k){k*exp(-lamb*p)/(1+k*exp(-lamb*p))}
surplus<-function(p,lamb,k){log(1+k*exp(-lamb*p))/lamb}
npv<-function(p,lamb,k){(log(1+k)/lamb-surplus(p,lamb,k)-p*demand(p,lamb,k))/(1-demand(p,lamb,k))}
profit<-function(p,lamb,k){(p-cost)*demand(p,lamb,k)}

demandroot<-function(p,lamb,gam,k){demand(p,lamb,k)-gam}
surplusroot<-function(p,lamb,gam,k){surplus(p,lamb,k)-gam}
npvroot<-function(p,lamb,gam,k){npv(p,lamb,k)-gam}

#compute optimal price
popt=numeric()
qopt=numeric()
suropt=numeric()
npvopt=numeric()
ropt=numeric()
expectation=numeric()
for (i in 1:N){
  popt[i]=optimize(profit,interval=c(0,100),lamb=lambda[i],k=ki[i],maximum=T)[[1]]
  qopt[i]=demand(popt[i],lambda[i],ki[i])
  suropt[i]=surplus(popt[i],lambda[i],ki[i])
  npvopt[i]=npv(popt[i],lambda[i],ki[i])
  ropt[i]=profit(popt[i],lambda[i],ki[i])
  expectation[i]=surplus(0,lambda[i],ki[i])
}

#price fairness
dif=max(popt)-min(popt)
a=seq(0,1,0.01)
result=data.frame()
for (alpha in a){
  delta=(1-alpha)*dif
  sollb=seq(round(min(popt),5),round(max(popt)-delta,5),0.001)
  solub=sollb+delta
  pftlist=rep(0,length(sollb))
  surlist=rep(0,length(sollb))
  price=matrix(nrow = length(sollb),ncol=N)
  for (j in 1:length(sollb)){
    g1=popt<sollb[j]
    g2=popt>solub[j]
    for (i in 1:N){
      if (g1[i]==T){price[j,i]=sollb[j]}
      else if (g2[i]==T){price[j,i]=solub[j]}
      else {price[j,i]=popt[i]}
      pftlist[j]=pftlist[j]+d[i]*profit(price[j,i],lambda[i],ki[i])
      surlist[j]=surlist[j]+d[i]*surplus(price[j,i],lambda[i],ki[i])
    }
  }
  jopt=which.max(pftlist)
  result=rbind(result, c(price[jopt,],pftlist[jopt],surlist[jopt],pftlist[jopt]+surlist[jopt]))
}
priceresult=result
#demand fairness
dif=max(qopt)-min(qopt)
a=seq(0,1,0.01)
result=data.frame()
for (alpha in a){
  delta=(1-alpha)*dif
  sollb=seq(round(min(qopt),5),round(max(qopt)-delta,5),0.001)
  solub=sollb+delta
  pftlist=rep(0,length(sollb))
  surlist=rep(0,length(sollb))
  price=matrix(nrow = length(sollb),ncol=N)
  for (j in 1:length(sollb)){
    g1=qopt<sollb[j]
    g2=qopt>solub[j]
    for (i in 1:N){
      if (g1[i]==T){price[j,i]=uniroot(demandroot,interval=c(0,100),lamb=lambda[i],gam=sollb[j],k=ki[i])[[1]]}
      else if (g2[i]==T){price[j,i]=uniroot(demandroot,interval=c(0,100),lamb=lambda[i],gam=solub[j],k=ki[i])[[1]]}
      else {price[j,i]=popt[i]}
      pftlist[j]=pftlist[j]+d[i]*profit(price[j,i],lambda[i],ki[i])
      surlist[j]=surlist[j]+d[i]*surplus(price[j,i],lambda[i],ki[i])
    }
  }
  jopt=which.max(pftlist)
  result=rbind(result, c(price[jopt,],pftlist[jopt],surlist[jopt],pftlist[jopt]+surlist[jopt]))
}
demandresult=result
#Surplus Fairness
dif=max(suropt)-min(suropt)
a=seq(0,1,0.01)
result=data.frame()
for (alpha in a){
  delta=(1-alpha)*dif
  sollb=seq(round(min(suropt),5),round(min(max(suropt)-delta,min(expectation)),5),0.001)
  solub=sollb+delta
  pftlist=rep(0,length(sollb))
  surlist=rep(0,length(sollb))
  price=matrix(nrow = length(sollb),ncol=N)
  for (j in 1:length(sollb)){
    g1=suropt<sollb[j]
    g2=suropt>solub[j]
    for (i in 1:N){
      if (g1[i]==T){price[j,i]=uniroot(surplusroot,interval=c(0,100),lamb=lambda[i],gam=sollb[j],k=ki[i])[[1]]}
      else if (g2[i]==T){price[j,i]=uniroot(surplusroot,interval=c(0,100),lamb=lambda[i],gam=solub[j],k=ki[i])[[1]]}
      else {price[j,i]=popt[i]}
      pftlist[j]=pftlist[j]+d[i]*profit(price[j,i],lambda[i],ki[i])
      surlist[j]=surlist[j]+d[i]*surplus(price[j,i],lambda[i],ki[i])
    }
  }
  jopt=which.max(pftlist)
  result=rbind(result, c(price[jopt,],pftlist[jopt],surlist[jopt],pftlist[jopt]+surlist[jopt]))
}
surplusresult=result
#NPV Fairness
dif=max(npvopt)-min(npvopt)
a=seq(0,1,0.01)
result=data.frame()
for (alpha in a){
  delta=(1-alpha)*dif
  sollb=seq(round(min(npvopt),5),round(min(max(npvopt)-delta,min(expectation)),5),0.001)
  solub=sollb+delta
  pftlist=rep(0,length(sollb))
  surlist=rep(0,length(sollb))
  price=matrix(nrow = length(sollb),ncol=N)
  for (j in 1:length(sollb)){
    g1=npvopt<sollb[j]
    g2=npvopt>solub[j]
    for (i in 1:N){
      if (g1[i]==T){price[j,i]=uniroot(npvroot,interval=c(0.000001,100),lamb=lambda[i],gam=sollb[j],ki[i])[[1]]}
      else if (g2[i]==T){price[j,i]=uniroot(npvroot,interval=c(0.000001,100),lamb=lambda[i],gam=solub[j],ki[i])[[1]]}
      else {price[j,i]=popt[i]}
      pftlist[j]=pftlist[j]+d[i]*profit(price[j,i],lambda[i],ki[i])
      surlist[j]=surlist[j]+d[i]*surplus(price[j,i],lambda[i],ki[i])
    }
  }
  jopt=which.max(pftlist)
  result=rbind(result, c(price[jopt,],pftlist[jopt],surlist[jopt],pftlist[jopt]+surlist[jopt]))
}
npvresult=result
