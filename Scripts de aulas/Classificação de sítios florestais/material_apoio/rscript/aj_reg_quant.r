rm(list=ls()); graphics.off();

library(cmrinvflor)
library(quantreg)

#Uso da regressão quantílica para classificação de sítios
ifc<-read.csv2(file.path("..","dados","ifc.csv"),fileEncoding = 'latin1')
tail(ifc)

ifc$x<-ifc$idade; ifc$y<-ifc$hdom

#Idades de predição para visualização das curvas
xpred<-0:ceiling(max(ifc$x)) 


graphics.off(); x11()
with(ifc,plot(x,y,pch='*',col='green',xlim=c(0,1.1*max(xpred)),ylim=c(0,40)))

chap_rich<-function(x,b0,b1,b2) b0*(1-exp(b1*x))^b2

#Parâmetros iniciais
b0=33.78
b1=-0.02825
b2=1.338

#Regressão não linear clássica
initial<-list(b0=b0, b1=b1, b2=b2)
classica<-nls(y~chap_rich(x,b0,b1,b2), data=ifc, start=initial)
lines(xpred, predict(classica, newdata = list(x=xpred)), lty=2, col=1)
summary(classica)

#Regressão não linear quantílica. Exemplo para mediana (tau=0.5)
quantilica<-nlrq(y~chap_rich(x,b0,b1,b2), data=ifc, start=initial, tau=0.5)
lines(xpred, predict(quantilica, newdata = list(x=xpred)), lty=1, col=2)
summary(quantilica)

graphics.off(); x11();
with(ifc,plot(x,y,pch='*',col='green',xlim=c(0,1.1*max(xpred)),ylim=c(0,40)))

qs<-seq(0.01,0.99,0.01)
parms<-data.frame()
for(i in 1:length(qs)){
  res<-nlrq(y~chap_rich(x,b0,b1,b2), data=ifc, start=initial, tau=qs[i])
  lines(xpred, predict(res, newdata = list(x=xpred)), lty=1, col=2)
  #print(summary(res))
  parms<-rbind(parms, cbind(sqs=qs[i],as.data.frame(t(coef(res)))))
}

equation<-list(
  desc_model='Chapman & Richards',
  required_variables=c('hdom','idade'),
  interest_variable='hdom',
  fpred=function(x,ps) ps$b0*(1-exp(ps$b1*x$idade))^ps$b2,
  parms=parms
)
saveRDS(equation, file.path("..","dados",'equation_regressao_quantilica.rds'))

