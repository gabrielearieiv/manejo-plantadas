#Lista 2
setwd("C:/Users/Gabriele/Desktop/Manejo de Florestas Plantadas/Lista 2")

#Ajuste de 5º grau
cub <- read.csv2('cubagem.csv')
cub$dap <- cub$cap/pi #criando coluna DAP
cub$disc <- cub$dicc-(2*cub$espcasca) #criando coluna do d sem casca
View(cub)

#Analisando os dados (COM CASCA)

x11();par(mfrow=c(1,2))
with(cub,plot(dicc~hi,pch=20,col='orange')) #ver variação dos dados no grafico  #variaçao dos di em funcao de hi
with(cub,plot(hi/ht,dicc,pch=20,col='yellow'))


#AJUSTE COM CASCA

#Ajuste linear
ajlin<-lm(formula <- '(dicc/dap) ~ 1 + I((hi)/ht) + I(((hi)/ht)^2) + I(((hi)/ht)^3) + 
  I(((hi)/ht)^4) + I(((hi)/ht)^5)',
          data=cub
)

#coeficientes ajuste linear com casca (parametros iniciais)
coef(ajlin) 
#ou
dput(structure
     (as.vector(coef(ajlin)),
       .Names=(c('b0', 'b1','b2','b3','b4','b5'))
     ))


#EstatÃƒÂ­stica ajuste linear com casca

summary(ajlin); # R? de modelo de afilamento normalmente ? 98,99....%, bem melhor do que de volume, com esse de pinus abandonado fica ruim. Todos os betas foram altamente significativos
vest=cub$dap*(predict(ajlin)); #predic=di/dap   *valor estimado do modelo do dap
sqerro=sum((cub$dicc-vest)^2);  # valor real- valor estimado
narvore=nrow(cub);
coefs=as.vector(coef(ajlin));
glreg=length(coefs)-1;
qme=sqerro/(narvore-glreg-1);
erropadres=sqrt(qme); # syx ? este valor que tem que comparar com o valor do erro do ajuste de baixo
erropadres;#2,67%
erropadres_perc=erropadres/mean(cub$dicc)*100; erropadres_perc ;#11,10%

#-----------------------------------------

#Ajuste não-linear com casca
bs <- coef(ajlin)
ajnlin<-nls(
  formula='(dicc/dap) ~ b0 + b1*((hi)/ht) + b2*(((hi)/ht)^2) + b3*(((hi)/ht)^3) + 
  b4*(((hi)/ht)^4) + b5*(((hi)/ht)^5)',
  data=cub,
  start<-list(b0=1.171239,
              b1=-3.191133,
              b2=14.334017,
              b3=-34.581619,
              b4=36.929241,
              b5=-14.686476))

#coeficientes ajuste não linear com casca
coef(ajnlin)
format(as.vector(coef(ajnlin)),decimal.mark=',')


#Estatisitca ajuste não-linear com casca

summary(ajnlin); # R? de modelo de afilamento normalmente ? 98,99....%, bem melhor do que de volume, com esse de pinus abandonado fica ruim. Todos os betas foram altamente significativos
vest=cub$dap*(predict(ajnlin)); #predic=di/dap   *valor estimado do modelo do dap
sqerro=sum((cub$dicc-vest)^2);  # valor real- valor estimado
narvore=nrow(cub);
coefs=as.vector(coef(ajnlin));
glreg=length(coefs)-1;
qme=sqerro/(narvore-glreg-1);
erropadres2=sqrt(qme); # syx ? este valor que tem que comparar com o valor do erro do ajuste de baixo
erropadres2;
erropadres_perc=erropadres2/mean(cub$dicc)*100; 
erropadres_perc;


#-------------------------------------------------------------

#SEM CASCA
#Analisando os dados (SEM CASCA)

x11();par(mfrow=c(1,2))
with(cub,plot(disc~hi,pch=20,col='red')) #ver varia??o dos dados no gr?fico #varia??o dos di em fun??o de hi
with(cub,plot(hi/ht,disc,pch=20,col='pink'))

#AJUSTE SEM CASCA
#Ajuste linear
ajlinsc<-lm(formula <- '(disc/dap) ~ 1 + I((hi)/ht) + I(((hi)/ht)^2) + I(((hi)/ht)^3) + 
  I(((hi)/ht)^4) + I(((hi)/ht)^5)',
            data=cub
)

#coeficientes ajuste linear sem casca
coef(ajlinsc) #parÃƒÂ¢metros iniciais
dput(structure
     (as.vector(coef(ajlinsc)),
       .Names=(c('b0','b1','b2','b3','b4','b5'))
     ))


#Ajuste não-linear sem casca
bs <- coef(ajlinsc)
ajnlinsc<-nls(
  formula='(dicc/dap) ~ b0 + b1*((hi)/ht) + b2*(((hi)/ht)^2) + b3*(((hi)/ht)^3) + 
  b4*(((hi)/ht)^4) + b5*(((hi)/ht)^5)',
  data=cub,
  start<-list(b0=1.047542,
              b1=-2.407283,
              b2=11.036698,
              b3=-27.575551,
              b4=29.850836,
              b5=-11.977325))

#coeficientes ajuste não linear sem casca
coef(ajnlinsc)
format(as.vector(coef(ajnlinsc)),decimal.mark=',')

#EstatÃƒÂ­stica do ajuste nÃƒÂ£o linear sem casca
summary(ajnlinsc); # R? de modelo de afilamento normalmente ? 98,99....%, bem melhor do que de volume, com esse de pinus abandonado fica ruim. Todos os betas foram altamente significativos
vest=cub$dap*(predict(ajnlinsc)); #predic=di/dap   *valor estimado do modelo do dap
sqerro=sum((cub$disc-vest)^2);  # valor real- valor estimado
narvore=nrow(cub);
coefs=as.vector(coef(ajnlinsc));
glreg=length(coefs)-1;
qme=sqerro/(narvore-glreg-1);
erropadres=sqrt(qme); # syx ? este valor que tem que comparar com o valor do erro do ajuste de baixo
erropadres;
erropadres_perc=erropadres2/mean(cub$disc)*100; 
erropadres_perc;


#Erro padrão residual
syx <- summary(ajnlinsc)$sigma;syx
syxperc <- syx/mean(cub$disc)*100;syxperc

#------------------------------------------

#calculando os volumes preditos
cub$diccest <- predict(ajnlin)
cub$discest <- predict(ajnlinsc)

#Calculando os residuos
cub$rescc <- residuals(ajnlin)
cub$ressc <- residuals(ajnlinsc)

View (cub)

#----------------------------------------------------

#GRAFICOS


#------------------------------------------------------

library (openxlsx)
library (cmrinvflor)

#VOLUME COM CASCA

fustes <- read.xlsx("prodcc.xlsx", sheet = 'fustes')
coefs <- read.xlsx("prodcc.xlsx", sheet = 'coeficientes')
produtos <- read.xlsx("prodcc.xlsx", sheet = 'produtos')
fustes$dap <- fustes$cap/pi #criando a coluna dap

#Considerar ou não a subsequencia dos produtos
subseq <- 1

nomprod <- as.matrix(produtos[,1])
vprod <- as.matrix(produtos[,2:5])

procafil <- as.data.frame(
  multprodarvtb5grau(fustes,coefs,vprod,nomprod,subseq)
)

View (procafil)
write.csv2(procafil, "resprocafilcc.csv", row.names=F)

#-------------------------------------------------

#VOLUME SEM CASCA

fustes <- read.xlsx('prodsc.xlsx',sheet='fustes')
coefs <- read.xlsx('prodsc.xlsx',sheet='coeficientes')
produtos <- read.xlsx('prodsc.xlsx',sheet='produtos')
fustes$dap <- fustes$cap/pi

#Considerar ou não a subsequencia dos produtos
subseq <- 1

nomprod <- as.matrix(produtos[,1])
vprod <- as.matrix(produtos[,2:5])

procafil <- as.data.frame(
  multprodarvbt5grau(fustes,coefs,vprod,nomprod,subseq)
)
View(procafil)
write.csv2(procafil, "resprocafilsc.csv", row.names=F)

fustes <- read.csv2('fustes1.csv')
procafilcc <- read.csv2( "resprocafilcc.csv" )
procafilsc <- read.csv2( "resprocafilsc.csv" )

fustes$vti <- procafilcc$vti
fustes$vprodisc <- procafilsc$vprodiproduto.1
fustes$vprodicc <- procafilcc$vprodiproduto.2
names(fustes)

#--------------------------------------------------------------

# Produto 1

parcela <- aggregate(list(vprod1=fustes$vprodisc),
                     list(talhao=fustes$talhao,parcela=fustes$parcela),sum);

media <- mean(parcela$vprod1)
media #[m³/parcela]  


View(parcela)

#area da parcela  
areaparc <- mean(((fustes$raio^2)*pi))

# mÃ‚Â³/parc para mÃ‚Â³/ha.
parcela$vprod1_ha<-parcela$vprod1*10000/areaparc

#para calcular o volume medio mÃ‚Â³/ha  
(vmed<-mean(parcela$vprod1_ha)) 

(vvar<-var(parcela$vprod1_ha)) #variancia do volume m6/ha  

(vdesv<-sd(parcela$vprod1_ha)) #desvio padrao do volume m?/ha 

(n<-length(parcela$vprod1_ha))

(talhao<-subset(fustes,duplicated(talhao)==F,
                c('talhao','area')));
#area da fazenda em ha
(area_fazenda<-sum(talhao$area))  

#numero de parcelas que cabem na fazenda 
(N<-area_fazenda*10000/areaparc)  

(erro_padrao_media<-sqrt(vvar/n)*(1-n/N)) 

#erro do inv
(erro_inv_unid<-qt(0.975,n-1)*erro_padrao_media) 

#erro percentual
(erro_inv_perc<-erro_inv_unid/vmed*100) 

#Intervalo de confiança em m³/ha

cat(paste(round(vmed-erro_inv_unid,2),' a ',round(vmed+erro_inv_unid,2),'mÃ‚Â³/ha com 95% de confianÃƒÂ§a para produto 1',sep=''))



#em m³/povoamento
cat(paste(round((vmed-erro_inv_unid)*area_fazenda,0),
          ' a ',
          round((vmed+erro_inv_unid)*area_fazenda,0),
          'm³/fazenda com 95% de confiança para produto 1',sep=''))

round(vmed*area_fazenda,0)