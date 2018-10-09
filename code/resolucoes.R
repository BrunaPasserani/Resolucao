#Nome: Bruna dos Santos Passerani

#Resolu��o Lista de Exercicios
#install.packages("packrat")

#packrat::status()
#packrat::restore()
#packrat::snapshot()

#Exercicio 1
#Media

#install.packages("openxlsx")


#library("openxlsx")

install.packages("dplyr")


install.packages("readxl")

library(readxl)

ma <- read_excel("data/exercicio1.xls")
head(ma)


xma = mean(ma$`Taxas de juros`)
xma

#Mediana
md <- read_excel("data/exercicio1.xls")
head(md)

xmd = median(md$`Taxas de juros`)
xmd

#Desvio Padrão

dvp <- read_excel("data/exercicio1.xls")

head(dvp)

desvio = sd(dvp$`Taxas de juros`)

desvio


#Variância

vr <- read_excel("data/exercicio1.xls")
head(vr)

variancia = var(vr$`Taxas de juros`)
variancia


#Minimo

mini <- read_excel("data/exercicio1.xls")
head(mini)

minimo = min(mini$`Taxas de juros`)
minimo

#Máximo

maxi <- read_excel("data/exercicio1.xls")

head(maxi)

maximo = max(maxi$`Taxas de juros`)
maximo

#Quartis

quartil <- read_excel("data/exercicio1.xls")

listaq <- sort(quartil$`Taxas de juros`)
min(listaq)


max(listaq)

q1 = (length(listaq) + 1) / 4

listaq[q1]

q3 <- 3 * (length(listaq) + 1) / 4

listaq[q3]



quantile(listaq)

png(filename = "graphics/graficoquartil.png", width = 480, height = 480)
boxplot(quantile(listaq) , pch=15, main="Quartis" , col = "lightblue", pars = list(boxwex = 1))
dev.off()

#Exercicio 2

#Letra a) Tabela de Frequências

#install.packages("dplyr")

library(dplyr)
dados <- read_excel("data/exercicio2.xls")
tabela <- data.frame(t(table(dados)))[,-1]
tabela$dados <- as.numeric(levels(tabela$dados))
tabela <- tabela %>% 
  mutate(Fr =Freq/sum(Freq))
tabela

#Letra b) e c)

#Média


ma2 <- read_excel("data/exercicio2.xls")

head(ma2)

xma2 = mean(ma2$Casas)
xma2

#Mediana
md2 <- read_excel("data/exercicio2.xls")
head(md2)

xmd2 = median(md2$Casas)
xmd2

#Desvio Padrão

dvp2 <- read_excel("data/exercicio2.xls")
head(dvp2)

desvio2 = sd(dvp2$Casas)
desvio2

#Variancia

vr2 <- read_excel("data/exercicio2.xls")
head(vr2)

variancia2 = var(vr2$Casas)
variancia2


#Manimo

mini2 <- read_excel("data/exercicio2.xls")
head(mini2)

minimo2 = min(mini2$Casas)
minimo2




#Maximo

maxi2 <- read_excel("data/exercicio2.xls")
head(maxi2)

maximo2 = max(maxi2$Casas)
maximo2

#Quartis

quartil2 <- read_excel("data/exercicio2.xls")


listaq <- sort(quartil2$Casas)
min(listaq)

max(listaq)

q1 = (length(listaq) + 1) / 4

listaq[q1]


q3 <- 3 * (length(listaq) + 1) / 4

listaq[q3]

quantile(listaq)

png(filename = "graphics/graficoquartil2.png", width = 480, height = 480)
boxplot(quantile(listaq) , pch=15, main="Quartis" , col = "lightblue", pars = list(boxwex = 1))
dev.off()
#Exercicio 3

#Mediana
med <- read_excel("data/exercicio3.xls")
head(med)

xmed = median(med$Familias)
xmed


#Moda
moda <- read_excel("data/exercicio3.xls")
head(moda)


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

result <- getmode(moda$Familias)


print(result)

png(filename = "graphics/familiaxfilhos.png", width = 480, height = 480)
barplot(moda$Familias, names.arg = moda$`Num de filhos`, col = c("blue","green", "red", "lavender", "black","white","pink"),
        legend.text = moda$`Num de filhos`, sub = "Grafico de Barras", main =
          "Numero de Filhos por Familia")

dev.off()


#Exercicio 4

#Distribuição de Frequência
dados <- read_excel("data/exercicio4.xls")

tabela <- data.frame(t(table(dados)))[,-1]
tabela$dados <- as.numeric(levels(tabela$dados))

tabela <- tabela %>% 
  mutate(Fr =Freq/sum(Freq))
tabela

#Histograma

eh <- read_excel("data/exercicio4.xls")
head(eh)
png(filename = "graphics/histoexercicio4.png", width = 480, height = 480)
hist(eh$Sal�rios, main = "Histograma", labels = TRUE,
     col = c("blue", "green", "red", "lavender", "mistyrose",
             "cornsilk", "purple"),
     ylab = "Frequência",
     xlab = "Dados")


dev.off()

#Exercicio 5

ex5 <- read_excel("data/exercicio5.xls")

head(ex5)


png(filename = "graphics/barrasexercicio5.png", width = 480, height = 480)

barplot(ex5$Pessoas, names.arg = ex5$Marcas, col = c("blue","green", "red", "lavender", "black"),
        legend.text = ex5$Marcas, sub = "Gráfico de Barras", main ="Antitermico Preferido")
dev.off()

#Exercio 6

install.packages("qcc")

library(qcc)
ex6 <- read_excel("data/exercicio6.xls")
ex6

attach(ex6)
names(ex6)
Tipo <- `N� pessoas`
names(Tipo) <- Qualidade
Tipo

png(filename = "graphics/diagramaparetoex6.png", width = 480, height = 480)
pareto.chart(Tipo)
dev.off()

Tabela <- pareto.chart(Tipo)

#Exercicio 8


eh <- read_excel("data/exercicio4.xls")
head(eh)
png(filename = "graphics/histoexercicio4.png", width = 480, height = 480)
hist(eh$Sal�rios, main = "Histograma", labels = TRUE,
     col = c("blue", "green", "red", "lavender", "mistyrose",
             "cornsilk", "purple"),
     ylab = "Frequencia",
     xlab = "Dados")

dev.off()

#Exercicio 7

ex7 <- read_excel("data/exercicio7.xls")

head(ex7)


png(filename = "graphics/barrasexercicio7.png", width = 480, height = 480)

barplot(ex7$Atendimento, names.arg = ex7$�reas, col = c("blue","green", "red", "lavender", "black"),
        legend.text = ex7$�reas, sub = "Gráfico de Barras", main ="Número de Atendimentos")
dev.off()

#Exercicio 8

#Distribuição de Frequência
dados <- read_excel("data/exercicio8.xls")

tabela <- data.frame(t(table(dados)))[,-1]
tabela$dados <- as.numeric(levels(tabela$dados))

tabela <- tabela %>% 
  mutate(Fr =Freq/sum(Freq))
tabela



ex8 <- read_excel("data/exercicio8.xls")
head(ex8)
png(filename = "graphics/histoexercicio8.png", width = 480, height = 480)
hist(ex8$`Altura dos pacientes`, main = "Histograma", labels = TRUE,
     col = c("blue", "green", "red", "lavender", "mistyrose",
             "cornsilk", "purple", "pink","green", "red", "lavender", "mistyrose",
             "cornsilk", "purple", "pink"),
     ylab = "Frequência",
     xlab = "Dados")
dev.off()

#Exercicio 9
ex9 <- read_excel("data/exercicio9.xls")
ex9
#e <- as.vector(ex9)

#summary(e)

#brk<-seq(4,24,2);

#brk
#e
#classes<-c("4-5","6-7","8-9","10-11","12-13","14-15","16-17","18-20","21-22","23-24")

#tabela <- table(cut(e, breaks=brk,right=FALSE,labels=classes))

#plot(table(cut(e,breaks=brk,right=FALSE,labels=classes)),ylab="Freq.")


png(filename = "graphics/histoexercicio9.png", width = 600, height = 600)
hist(ex9$Sal�rios, main = "Histograma", labels = TRUE, nclass = 12,
     col = c("blue", "green", "red", "lavender", "mistyrose",
             "cornsilk", "purple", "pink","green", "red", "lavender", "mistyrose",
             "cornsilk", "purple", "pink"),
     ylab = "Frequencia",
     xlab = "Dados")
dev.off()

