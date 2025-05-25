#Ajuste do modelo de projeção de Clutter

dir()

library(cmrinvflor)

ifc<-read.csv2("ifc.csv")
View(ifc)
names(ifc)

ifc<-pairSequence(ifc)
View(ifc)

ifc$s<-ifc$s2

ajclutter<-lm(
  formula = log(vtcc2)~I(1/s)+I(1/idade2)+
    I((idade1/idade2)*log(ab1))+
    I(1-(idade1/idade2))+
    I((1-(idade1/idade2))*s),
  data = ifc
)

summary(ajclutter)

syx<-summary(ajclutter)$sigma

D(expression(log(vtcc2)),'vtcc2')
der_vtcc2<-1/ifc$vtcc2
medgeo<-exp(mean(log(der_vtcc2)))
ind_furnival<-1/medgeo*syx

#Indice de Furnival (%)
ind_furnival_perc<-round(ind_furnival/mean(ifc$vtcc2)*100,2)

graphics.off(); x11();
plot(
  x=predict(ajclutter),
  y= residuals(ajclutter),
  xlab='log(m³)',
  ylab='log(m³)',
  pch="*",
  col = "red"
)

graphics.off(); x11();
par(mfrow=c(1,2));
plot(
  x=predict(ajclutter),
  y=residuals(ajclutter),
  ylim=c(-0.4,0.4),
  xlab="vtcc estimado [log(m³)]",
  ylab="resíduo [log(m³)]",
  pch="*",
  col="red",
  abline(h=0, lwd=1.5),
)

library(fBasics)
qqnormPlot(x = as.vector(residuals(ajclutter)))

nortest::ad.test(as.vector(residuals(ajclutter)))

shapiro.test(as.vector(residuals(ajclutter)))

#(a) parâmetros ajustados para o modelo 1

betas<-structure(
  coef(ajclutter),
  .Names=c('b0','b1','b2','b3','b4','b5'))

betas<-as.data.frame(t(as.matrix(betas)))
