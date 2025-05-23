b0 = 2.95949757908642 
b1 = -24.5373686241002
b2 = -15.224252935247 
b3 = 1.12033839741921
b4 = 2.54772391334182
b5 = 0.0644269914554016

#Dados da última medição
parcela=37
id1<-41.52 #meses
ab1=18.9   #m²/ha
s=29       #m

eqvol<-expression(
  exp(b0 + b1/x + b2/s + b3*(id1/x)*log(ab1)+
  b4*(1-id1/x) + b5*((1-id1/x)*s))
)

#D(eqvol, 'x')

der.eqvol<-expression(
  exp(b0 + b1/x + b2/s + b3 * (id1/x) * log(ab1) + b4 * (1 - id1/x) + 
  b5 * ((1 - id1/x) * s)) * (b4 * (id1/x^2) - 
  (b3 * (id1/x^2) * log(ab1) + b1/x^2) + b5 * (id1/x^2 * s))  
)

vtcc<-function(x) eval(eqvol)
vtcc(seq(36,120,12))

icm<-function(x) eval(der.eqvol)
imm<-function(x) eval(eqvol)/x

vtcc5=vtcc(5*12)
ima5=imm(5*12)*12
ica5=icm(5*12)*12

graphics.off();x11();
par(mfrow=c(2,1))
curve(vtcc(x),12,120,xlab='idade(meses)', ylab='vtcc[m³]')
curve(
  icm(x)*12,12,120,
  xlab='idade(meses)', 
  ylab='incremento[m³/ha.ano]',
  col='green'
)
curve(
  imm(x)*12,12,120,
  xlab='idade(meses)', 
  ylab='incremento[m³/ha.ano]',
  col='blue',
  add=T
)
legend(
  'topright',c('ica','ima'), 
  text.col=c('green','blue'),
  lwd=c(1,1),
  col=c('green','blue'),
  horiz=T,
  bty='n'
)

#Definição da idade ótima de corte silvicultural

#Opção 01(
op<-optimize(f=imm, interval = c(12,120), maximum = T)
iocs<-round(op$maximum/12,2) #Anos

#Opção 02
fo<-function(x) abs(imm(x)-icm(x))
op<-optimize(f=fo, interval = c(12,120), maximum = F)
iocs<-round(op$minimum/12,2) #Anos

#Opção 03
# eqimm<-expression(
#   exp(b0 + b1/x + b2/s + b3*(id1/x)*log(ab1)+
#         b4*(1-id1/x) + b5*((1-id1/x)*s))/x
# )
# D(eqimm,'x')
# der.eqimm<-expression(
#   exp(b0 + b1/x + b2/s + b3 * (id1/x) * log(ab1) + b4 * (1 - id1/x) + 
#   b5 * ((1 - id1/x) * s)) * (b4 * (id1/x^2) - (b3 * (id1/x^2) * 
#   log(ab1) + b1/x^2) + b5 * (id1/x^2 * s))/x - exp(b0 + b1/x + 
#   b2/s + b3 * (id1/x) * log(ab1) + b4 * (1 - id1/x) + b5 * 
#   ((1 - id1/x) * s))/x^2
# )

#No programa wxmaxima
#solve([der.imm], [x]);

iocs<-round((b4*id1-b3*id1*log(ab1)-b1+b5*id1*s)/12,2)

abline(v=iocs*12)
text(
  x = iocs*12, 
  y = imm(iocs*12)*12, 
  labels = paste(iocs,'anos'), 
  cex=0.8, 
  col='red',
  pos=4
)
