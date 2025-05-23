rm(list=ls()); graphics.off();

library(cmrinvflor);

equation<-readRDS(file.path("..","dados",'equation_regressao_quantilica.rds'))
equation$present_age_variable="idade"

ifc<-read.csv2(file.path("..","dados","ifc2.csv"), fileEncoding = 'latin1')
tail(ifc)

ifc<-ifc[,c('parcela','idade','hdom')]

names(ifc)<-c('id','idade','hdom')

reference_age<-72 #Idade de referência na mesma escala do vetor ifc$idade
n_class<-4
way_counting_migrations=1
plot_curves<-T

graphics.off(); x11();
cls<-siteClassificationQuantileRegression(
  equation = equation,
  continuous_sample_plots = ifc,
  reference_age = reference_age,
  n_class = 4,
  way_counting_migrations = way_counting_migrations,
  plot_curves=plot_curves
)

cls$estabilidade

openxlsx::write.xlsx(cls$continuous_sample_plots, file.path("..","dados",'cls_regressao_quantilica.xlsx'))

