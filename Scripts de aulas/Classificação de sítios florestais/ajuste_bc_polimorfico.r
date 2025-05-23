##Ajuste do modelo de Bailey & Clutter polimórfico
# hdom2~b0*(hdom1/b0)^((idade1/idade2)^b2)

ifc<-read.csv2("ifc.csv")
View(ifc)
names(ifc)

library(cmrinvflor)

ifc<-parear_seqmed(ifc[,c('parcela','idade','hdom')])
View(ifc)

ajuste<-nls(
  formula=hdom2~b0*(hdom1/b0)^((idade1/idade2)^b2),
  data=ifc,
  start = list(b0=51, b2=0.8)
)
summary(ajuste)

options(OutDec=",")
coefs<-coef(ajuste)


