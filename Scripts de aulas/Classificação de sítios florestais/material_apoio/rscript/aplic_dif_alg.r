rm(list=ls()); graphics.off();

library(cmrinvflor);

equation<-readRDS(file.path("..","dados",'equation_diferenca_algebrica.rds'))

equation$present_age_variable="idade1"
equation$future_age_variable="idade2"
equation$present_dominant_height_variable="hdom1"
equation$future_dominant_height_variable="hdom2"

ifc<-read.csv2(file.path("..","dados","ifc.csv"), fileEncoding = 'latin1')
tail(ifc)

ifc<-ifc[,c('parcela','idade','hdom')]

names(ifc)<-c('id','idade1','hdom1')

reference_age<-72  #Idade de referência na mesma escala do vetor ifc$idade1
site_class_limits<-NA
way_counting_migrations=1
plot_curves<-T

graphics.off(); x11();
cls<-siteClassificationAlgebraicDifference(
  equation = equation,
  continuous_sample_plots = ifc,
  reference_age = reference_age,
  site_class_limits = site_class_limits,
  way_counting_migrations = way_counting_migrations,
  plot_curves=plot_curves
)

names(cls)

View(cls$continuous_sample_plots)
cls$estabilidade

openxlsx::write.xlsx(cls$continuous_sample_plots, file.path("..","dados",'cls_diferenca_algebrica.xlsx'))

