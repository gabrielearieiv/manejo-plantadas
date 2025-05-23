ifc<-read.csv2("ifc.csv")
tail(ifc)

cor(ifc[,2:5])

x11(); par(mfrow=c(3,1))
with(ifc, plot(idade,hdom, pch="*", col='green'))
with(ifc, plot(idade,vtcc, pch="*", col='green'))
with(ifc, plot(hdom,vtcc, pch="*", col='green'))

ifc$x<-ifc$idade

##Modelo/funções
##Schumacher
sch<-function(x,b0, b1) b0*exp(b1*(1/x))
parms_ini<-list(b0=exp(3.6), b1=-27)

##Bailey e Clutter
bc_geral<-function(x, b0, b1, b2) exp(b0+b1*(1/x)^b2)
parms_ini<-list(b0=4, b1=-11, b2=0.62)

ajuste<-nls(
  formula=hdom~bc_geral(x, b0, b1, b2),
  data=ifc,
  start = parms_ini
)
options(OutDec=",")
coefs<-coef(ajuste)

graphics.off();
x11();
with(ifc, plot(idade,hdom, pch="*", col='green'))

curve(
  expr = bc_geral(x, b0=coefs[1], b1=coefs[2], b2=coefs[3]),
  from = min(ifc$idade),
  to = max(ifc$idade),
  add=T,
  lwd=2
)



