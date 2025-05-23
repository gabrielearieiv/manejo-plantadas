library(openxlsx)
ifc<-read.xlsx("ifc.xlsx", sheet=1)
View(ifc)

ifc<-cmrinvflor::parear_seqmed(ifc)
View(ifc)
ifc$s<-ifc$s2
names(ifc)<-tolower(names(ifc))

ajuste<-lm(
  formula='I(log(vtcc2))~I(1/idade2)+
          I(1/s)+I((idade1/idade2)*log(ab1))+
          I(1-idade1/idade2)+I((1-idade1/idade2)*s)',
  data = ifc
)
sumario<-summary(ajuste)

d_log_vtcc2<-1/ifc$vtcc2

med_geometrica<-exp(mean(log(d_log_vtcc2)))

ind_furnival<-1/med_geometrica*sumario$sigma #m³

ind_furnival_perc<-round(ind_furnival/mean(ifc$vtcc2)*100,2)

dput(structure(
  coef(ajuste)),
  .Names=c('b0','b1','b2','b3','b4','b5')
))

