library(cmrinvflor)

#Chapman & Richards Polimórfico
modelo<-"hdom2~b0*((hdom1/b0)^((idade1/idade2)^b2))"
parms_start<-list(b0=51.58351, b2=0.77542)

ifc<-read.csv2(file.path("..","dados","ifc.csv"), fileEncoding = 'latin1')
tail(ifc)

ifc<-ifc[,c('parcela','idade','hdom')]

ifc_pareado<-parear_seqmed(ifc)
tail(ifc_pareado)

aj<-nls(formula = modelo, data=ifc_pareado, start=parms_start)
summary(aj)

parms=as.list(coef(aj))
equation<-list(
  desc_model='Chapman & Richards Polimórfico',
  required_variables=c('hdom2','hdom1','idade1','idade2'),
  interest_variable='hdom2',
  fpred=function(x,ps) ps$b0*((x$hdom1/ps$b0)^((x$idade1/x$idade2)^ps$b2)),
  parms=parms
)
saveRDS(equation, file.path("..","dados",'equation_diferenca_algebrica.rds'))

