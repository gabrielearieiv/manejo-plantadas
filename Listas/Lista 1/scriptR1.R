#ajuste do polinimio de 5 grau--------------------------------------------------
cub <- read.csv2('cubagem.csv')

tail(cub)

x11(); par(mfrow=c(2,1))
with(cub,plot(hi,dicc,pch="*", col='green'))
with(cub,plot(hi/ht,dicc,pch="*", col='green')) 


#modelo linear
ajlin<-lm(formula <- '(dicc/dap) ~ -1 + I((ht-hi)/ht) + I(((ht-hi)/ht)^2) + 
          I(((ht-hi)/ht)^3) + I(((ht-hi)/ht)^4) + I(((ht-hi)/ht)^5)',data=cub)
coef(ajlin)

dput(structure
     (as.vector(coef(ajlin)),
       .Names=(c('b1','b2','b3','b4','b5'))
     ))

#modelo nao linear
bs <- coef(ajlin)
ajnlin=nls('dicc~dap*(b1*((ht-hi)/ht)+
b2*(((ht-hi)/ht)^2)+
b3*(((ht-hi)/ht)^3)+
b4*(((ht-hi)/ht)^4)+
b5*(((ht-hi)/ht)^5))',
           cub,start<-list(b1=bs[1],
                           b2=bs[2],
                           b3=bs[3],
                           b4=bs[4],
                           b5=bs[5]))

summary(ajnlin)

coef(ajnlin)

format(as.vector(coef(ajnlin)),decimal.mark=',')

#planilha-----------------------------------------------------------------------
library(openxlsx)
library(cmrinvflor)

subseq<-1

#PROCESSAMENTO


fustes <- read.xlsx('fustes_gabis.xlsx', sheet = 'fustes')
tail(fustes)

coeficientes <- read.xlsx('fustes_gabis.xlsx', sheet = 'coeficientes')
tail(coeficientes)

produtos <- read.xlsx('fustes_gabis.xlsx', sheet = 'produtos')
tail(produtos)

nomprod <-as.matrix(produtos[,1])
vprod <-as.matrix(produtos[,2:5])

procafil <- multprodarvtb5grau(
  dfarv = fustes,
  dfcoef = coeficientes,
  vprod = vprod,  
  nomprod = nomprod,
  subseq = TRUE
)


View(procafil)

procafil <- merge(fustes,procafil, by = "idfustemed")
head(procafil)

write.xlsx(procafil,"resultado.xlsx")
names(procafil)

?multprodarvtb5grau
###Soma do número de toras para serraria
(ntser<- sum(procafil$ntorasiser))

##Soma do número de toras para pallets
(ntpal<- sum(procafil$ntorasipa))

##Soma do número de toras para cavaco energético
(nten<- sum(procafil$ntorasien))

##Soma do volume com casca das toras de serraria
(vtser<- sum(procafil$vprodiser))

##Soma do volume com casca de toras para pallets
(vtpal<- sum(procafil$vprodipa))

##Soma do volume com casca de toras para cavaco energético
(vten<- sum(procafil$vprodien))

##Soma simples do volume total com casca de todas as árvores
(vtcc<- sum(procafil$vti))

