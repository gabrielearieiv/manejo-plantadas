library(cmrinvflor)
library(openxlsx)

wb<-loadWorkbook('proc_afil.xlsx')

fustes<-read.xlsx(wb, sheet='fustes')
tail(fustes)

coeficientes<-read.xlsx(wb, sheet='coeficientes')
tail(coeficientes)

prod<-read.xlsx(wb, sheet='produtos')
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

addWorksheet(wb, "resultado")
writeData(wb,'resultado',procafil)
saveWorkbook(wb, 'proc_afil.xlsx', overwrite = T)
rm(wb)

