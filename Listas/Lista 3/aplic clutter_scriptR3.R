# Aplicação de equação de projeção Clutter

library(cmrinvflor)

ifc<-read.csv2("ifc.csv")
View(ifc)
names(ifc)

ifc<-pairSequence(ifc)
View(ifc)

ifc$s<-ifc$s2

b0<- 2.364101
b1<- -3.87348
b2<- -19.87908
b3<- 1.158108
b4<- 0.6168037
b5<- 0.1304589


eqvol<-expression(
  exp(b0+b1/s+b2/idade2+b3*(idade1/idade2)*log(ab1)+
        b4*(1-idade1/idade2)+b5*(1-idade1/idade2)*s)
)

eqvol<-expression(
  exp(b0+b1/s+b2/x+b3*(idade1/x)*log(ab1)+
        b4*(1-idade1/x)+b5*(1-idade1/x)*s)
)

eqimm<-expression(
  exp(b0+b1/s+b2/x+b3*(idade1/x)*log(ab1)+
        b4*(1-idade1/x)+b5*(1-idade1/x)*s)/x
)

D(eqvol,"x")

eqicm<-expression(
  exp(b0 + b1/s + b2/x + b3 * (idade1/x) * log(ab1) + 
  b4 * (1-idade1/x) + 
  b5 * (1-idade1/x) * s) * (b4 * (idade1/x^2) -
 (b3* (idade1/x^2) * log (ab1) + b2/x^2) + b5 * (idade1/x^2) *
  s))


vtcc<-function(x)eval(eqvol)
imm<-function(x)eval(eqimm)
icm<-function(x)eval(eqicm)

##Dados da ultima medição

idade1<-49.71
ab1<-17.93
s<-29
vtcc(60) 
#vtcc(60)/3 #volume dividido pelos anos
imm(60)*12
icm(60)*12

#b-c-d-e-f-g média estimada para x anos de incremento médio com casca
#(imm(idade em meses)*12)

#3 anos
vtcc(36) 
imm(36)*12 

#4 anos
vtcc(48) 
imm(48)*12

#5 anos
vtcc(60) 
imm(60)*12

#6 anos
vtcc(72) 
imm(72)*12

#7 anos
vtcc(84) 
imm(84)*12

#8 anos
vtcc(96) 
imm(96)*12


graphics.off(); x11();
par(mfrow=c(2,1))

curve(
  vtcc(x),
  from=12,
  to=120,
  xlab="idade[meses]",
  ylab="vtcc [m³/ha.ano]",
  col="darkblue"
)

curve(
  icm(x)*12,
  from=12,
  to=120,
  xlab="idade [meses]",
  ylab='incremento [m³/ha.ano]',
  col = "darkred"
)

curve(
  imm(x)*12,
  from=12,
  to=120,
  col="darkgreen",
  add = T)

legend(
  'topright',
  legend = c('ica', 'ima'),
  text.col = c('darkred','darkgreen'),      
  box.lty = 0,                         
  bty = 'n',
  lwd=c(1,1),
)
  
##Estimativa da idade ótima de corte silvicultural

#Opção 01: ICA = IMA
fo<-function(x) abs(icm(x)-imm(x))
op<-optimize(f= fo, interval = c(12, 120), maximum = F)
iocs<-op$minimum/12 #idade ótima de corte silvicultural (anos)

#Opção 02: Máximo IMA
op2 <- optimize(f=imm,interval=c(12,120), maximum=T)
iocs2 <- op2$maximum/12 # idade ótima de corte silvicultural (anos)


## Idade máxima de ICA ou idade em que a aceleração de crescimento é igual a zero

f_idade_max_ica <- function(idade1, ab1, s, b2, b3, b4, b5){
  (b5*idade1*s+(b4-log(ab1)*b3)*idade1-b2)/2
}

idade_max_ica<- f_idade_max_ica(idade1,ab1,s,b2,b3,b4,b5)/12

# Opção 2: máximo ICA
op <- optimize(f=icm,interval=c(12,120),maximum=T)
idade_max_ica <- op$maximum/12


# idade máxima de ICA ocorre na metade da idade de máximo IMA
# idade_max_ica = iocs/2


#linhas que cortam os dois gráficosr

par(xpd=NA)
abline(v=iocs*12,lty=2)
abline(v=idade_max_ica*12,lty=2)

