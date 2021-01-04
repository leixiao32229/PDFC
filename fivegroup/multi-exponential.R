

demand<-function(p,lamb){exp(-lamb*p)}
surplus<-function(p,lamb){exp(-lamb*p)/lamb}
npv<-function(p,lamb){(1/lamb-surplus(p,lamb)-p*demand(p,lamb))/(1-demand(p,lamb))}
profit<-function(p,lamb){(p-cost)*demand(p,lamb)}

demandroot<-function(p,lamb,gam){demand(p,lamb)-gam}
surplusroot<-function(p,lamb,gam){surplus(p,lamb)-gam}
npvroot<-function(p,lamb,gam){npv(p,lamb)-gam}

#compute optimal price
popt=numeric()
qopt=numeric()
suropt=numeric()
npvopt=numeric()
ropt=numeric()
expectation=numeric()
for (i in 1:N){
  popt[i]=optimize(profit,interval=c(0,10),lamb=lambda[i],maximum=T)[[1]]
  qopt[i]=demand(popt[i],lambda[i])
  suropt[i]=surplus(popt[i],lambda[i])
  npvopt[i]=npv(popt[i],lambda[i])
  ropt[i]=profit(popt[i],lambda[i])
  expectation[i]=surplus(0,lambda[i])
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
      pftlist[j]=pftlist[j]+d[i]*profit(price[j,i],lambda[i])
      surlist[j]=surlist[j]+d[i]*surplus(price[j,i],lambda[i])
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
      if (g1[i]==T){price[j,i]=uniroot(demandroot,interval=c(0,100),lamb=lambda[i],gam=sollb[j])[[1]]}
      else if (g2[i]==T){price[j,i]=uniroot(demandroot,interval=c(0,100),lamb=lambda[i],gam=solub[j])[[1]]}
      else {price[j,i]=popt[i]}
      pftlist[j]=pftlist[j]+d[i]*profit(price[j,i],lambda[i])
      surlist[j]=surlist[j]+d[i]*surplus(price[j,i],lambda[i])
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
  
  sollb=seq(round(min(suropt),5),round(min(max(suropt)-delta,min(expectation)),5),by=0.001)
  solub=sollb+delta
  pftlist=rep(0,length(sollb))
  surlist=rep(0,length(sollb))
  price=matrix(nrow = length(sollb),ncol=N)
  for (j in 1:length(sollb)){
    g1=suropt<sollb[j]
    g2=suropt>solub[j]
    for (i in 1:N){
      if (g1[i]==T){price[j,i]=uniroot(surplusroot,interval=c(0,100),lamb=lambda[i],gam=sollb[j])[[1]]}
      else if (g2[i]==T){price[j,i]=uniroot(surplusroot,interval=c(0,100),lamb=lambda[i],gam=solub[j])[[1]]}
      else {price[j,i]=popt[i]}
      pftlist[j]=pftlist[j]+d[i]*profit(price[j,i],lambda[i])
      surlist[j]=surlist[j]+d[i]*surplus(price[j,i],lambda[i])
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
      if (g1[i]==T){price[j,i]=uniroot(npvroot,interval=c(0.000001,100),lamb=lambda[i],gam=sollb[j])[[1]]}
      else if (g2[i]==T){price[j,i]=uniroot(npvroot,interval=c(0.000001,100),lamb=lambda[i],gam=solub[j])[[1]]}
      else {price[j,i]=popt[i]}
      pftlist[j]=pftlist[j]+d[i]*profit(price[j,i],lambda[i])
      surlist[j]=surlist[j]+d[i]*surplus(price[j,i],lambda[i])
    }
  }
  jopt=which.max(pftlist)
  result=rbind(result, c(price[jopt,],pftlist[jopt],surlist[jopt],pftlist[jopt]+surlist[jopt]))
}
npvresult=result



