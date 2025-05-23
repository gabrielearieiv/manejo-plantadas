library(cmrinvflor)

#?cmrinvflor

fustes<-read.csv2('fustes.csv')
tail(fustes)

coeficientes<-read.csv2('coeficientes.csv')
tail(coeficientes)

prod<-read.csv2('produtos.csv')
tail(prod)

#Considerar ou não a subsequência dos produtos
subseq<-1

##Processamento

nomprod<-as.matrix(prod[,1])
vprod<-as.matrix(prod[,2:5])

procafil<-as.data.frame(
  multprodarvbt5grau(
    dfarv = fustes,
    dfcoef = coeficientes,
    vprod = vprod,
    nomprod = nomprod,
    subseq = subseq
  )
)
View(procafil)




