#exponential
N=5
cost=0.4

for (iter in 1:20) {
  set.seed(iter)
  lambda=runif(5,0.1,1)
  set.seed(iter+10000)
  d=runif(5)
  source('multi-exponential.R')
  filename=paste('./multiexp/',iter,'.png',sep='')
  png(filename=filename, width = 1440, height = 764)
  par(mfrow=c(2,4))
  matplot(priceresult[,1:5], type = c("l"),pch=1,lwd=2)
  matplot(demandresult[,1:5], type = c("l"),pch=1,lwd=2)
  matplot(surplusresult[,1:5], type = c("l"),pch=1,lwd=2)
  matplot(npvresult[,1:5], type = c("l"),pch=1,lwd=2)
  matplot(priceresult[,6:8], type = c("l"),pch=1,lwd=2)
  matplot(demandresult[,6:8], type = c("l"),pch=1,lwd=2)
  matplot(surplusresult[,6:8], type = c("l"),pch=1,lwd=2)
  matplot(npvresult[,6:8], type = c("l"),pch=1,lwd=2)
  dev.off()
  print(demandresult[1,8]-demandresult[101,8])
}

final=cbind(priceresult,demandresult,surplusresult,npvresult)
write.csv(final,'two-group-exp.csv',row.names = F)
#logistic
N=5
cost=0.5
for (iter in 1:20) {
  set.seed(iter)
  lambda=runif(5,0.1,1)
  set.seed(iter+10000)
  d=runif(5)
  set.seed(iter+100000)
  ki=runif(5,3,10)
  source('multi-logistic.R')
  filename=paste('./multilogistic/',iter,'.png',sep='')
  png(filename=filename, width = 1440, height = 764)
  par(mfrow=c(2,4))
  matplot(priceresult[,1:5], type = c("l"),pch=1,lwd=2)
  matplot(demandresult[,1:5], type = c("l"),pch=1,lwd=2)
  matplot(surplusresult[,1:5], type = c("l"),pch=1,lwd=2)
  matplot(npvresult[,1:5], type = c("l"),pch=1,lwd=2)
  matplot(priceresult[,6:8], type = c("l"),pch=1,lwd=2)
  matplot(demandresult[,6:8], type = c("l"),pch=1,lwd=2)
  matplot(surplusresult[,6:8], type = c("l"),pch=1,lwd=2)
  matplot(npvresult[,6:8], type = c("l"),pch=1,lwd=2)
  dev.off()
  print(demandresult[1,8]-demandresult[101,8])
}

final=cbind(priceresult,demandresult,surplusresult,npvresult)
write.csv(final,'two-group-logistic.csv',row.names = F)


#loglog
N=5
cost=2
for (iter in 1:20) {
  set.seed(iter+10000)
  d=runif(5)
  set.seed(iter+100000)
  beta=runif(5,1.5,5)
  set.seed(iter)
  coef=runif(5,0.3,0.9)
  lambda=coef*(cost*beta/(beta-1))
  source('multi-loglog.R')
  filename=paste('./multiloglog/',iter,'.png',sep='')
  png(filename=filename, width = 1440, height = 764)
  par(mfrow=c(2,4))
  matplot(priceresult[,1:5], type = c("l"),pch=1,lwd=2)
  matplot(demandresult[,1:5], type = c("l"),pch=1,lwd=2)
  matplot(surplusresult[,1:5], type = c("l"),pch=1,lwd=2)
  matplot(npvresult[,1:5], type = c("l"),pch=1,lwd=2)
  matplot(priceresult[,6:8], type = c("l"),pch=1,lwd=2)
  matplot(demandresult[,6:8], type = c("l"),pch=1,lwd=2)
  matplot(surplusresult[,6:8], type = c("l"),pch=1,lwd=2)
  matplot(npvresult[,6:8], type = c("l"),pch=1,lwd=2)
  dev.off()
  print(demandresult[1,8]-demandresult[101,8])
}

final=cbind(priceresult,demandresult,surplusresult,npvresult)
write.csv(final,'two-group-loglog.csv',row.names = F)
