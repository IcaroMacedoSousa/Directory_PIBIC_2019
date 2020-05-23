#####   Sintaxe PIBIC (2019)   #####

### Pacotes Psicométricos ###
install.packages("psychometric")
library(psychometric)

### Análise de TRI ###
install.packages("psych")
install.packages("GPArotation")
library(psych)
library(GPArotation)

### Chamando o banco 
install.packages("readr")
library(readr)
ECB <- read_delim("C:/Users/IcaroMacedo/Desktop/ICs/2019-IC-UFPI/Directory_PIBIC_2019/ECB_PIBIC_2019.dat", 
                  "\t", escape_double =FALSE, col_names =FALSE, 
                  trim_ws = TRUE)
View(ECB)

### Criando banco para fator Bullying Físico ###

vars_BF<-c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X24") #selecionando os itens do BF 
BF_fator<-ECB[vars_BF] #criando um novo banco para o fator BF

install.packages("mirt") 
library(mirt)

mod1 <-mirt(BF_fator, 1, itemtype = 'graded') #Criando o modelo
coef(mod1, simplify=TRUE, IRTpars=TRUE) # gerando parametros dos itens

plot(mod1, type='trace')
plot(mod1, type='infotrace')
plot(mod1, type='info')

areainfo(mod1, c(-4,4)) ### all items (total test information)
areainfo(mod1, c(-4,4), which.items = 1) ### items 1 to 11
areainfo(mod1, c(-4,4), which.items = 2)
areainfo(mod1, c(-4,4), which.items = 3)
areainfo(mod1, c(-4,4), which.items = 4)
areainfo(mod1, c(-4,4), which.items = 5)
areainfo(mod1, c(-4,4), which.items = 6) 
areainfo(mod1, c(-4,4), which.items = 7)
areainfo(mod1, c(-4,4), which.items = 8)
areainfo(mod1, c(-4,4), which.items = 9)
areainfo(mod1, c(-4,4), which.items = 10)
areainfo(mod1, c(-4,4), which.items = 11)



###  Criando banco para fator Bullying Verbal  ###

vars_BV<-c("X11","X12","X13","X14","X15","X16") #selecionando os itens do fator BV
BV_fator<-ECB[vars_BV] #criando um novo banco para o fator BR

mod2 <-mirt(BV_fator, 1, itemtype = 'graded') #Criando o modelo
coef(mod2, simplify=TRUE, IRTpars=TRUE) # gerando os parametros dos itens

plot(mod2, type='trace')
plot(mod2, type='infotrace')
plot(mod2, type='info')

areainfo(mod2, c(-4,4)) # all items (total test information)
areainfo(mod2, c(-4,4), which.items = 1) #items 1 to 6
areainfo(mod2, c(-4,4), which.items = 2)
areainfo(mod2, c(-4,4), which.items = 3)
areainfo(mod2, c(-4,4), which.items = 4)
areainfo(mod2, c(-4,4), which.items = 5)
areainfo(mod2, c(-4,4), which.items = 6)

###  Criando banco para fator Bullying Relacional  ###

vars_BR<-c("X17","X18","X19","X20","X29") # selecionando itens do fator BR
BR_fator<-ECB[vars] #criando um novo banco para o fator BR

mod3 <-mirt(BR_fator, 1, itemtype = 'graded') #Criando o modelo
coef(mod3, simplify=TRUE, IRTpars=TRUE) # verificando os parametros dos itens

plot(mod3, type='trace')
plot(mod3, type='infotrace')
plot(mod3, type='info')

areainfo(mod3, c(-4,4)) ## all items (total test information)
areainfo(mod3, c(-4,4), which.items = 1) #items 1 to 5
areainfo(mod3, c(-4,4), which.items = 2)
areainfo(mod3, c(-4,4), which.items = 3)
areainfo(mod3, c(-4,4), which.items = 4)
areainfo(mod3, c(-4,4), which.items = 5)

###  Criando banco para fator Cyberbullying  ###

vars_CB <-c("X21","X22","X23","X25","X26","X27","X28","X30") # selecionando dos itens do fator CB
CB_fator<-ECB[vars_CB] #criando um novo banco para o fator CB

mod4 <-mirt(CB_fator, 1, itemtype = 'graded') #Criando o modelo
coef(mod4, simplify=TRUE, IRTpars=TRUE) # verificando os parametros dos itens

plot(mod4, type='trace')
plot(mod4, type='infotrace')
plot(mod4, type='info')

areainfo(mod4, c(-4,4)) # all items (total test information)
areainfo(mod4, c(-4,4), which.items = 1) #items 1 to 8
areainfo(mod4, c(-4,4), which.items = 2)
areainfo(mod4, c(-4,4), which.items = 3)
areainfo(mod4, c(-4,4), which.items = 4)
areainfo(mod4, c(-4,4), which.items = 5)
areainfo(mod4, c(-4,4), which.items = 6) 
areainfo(mod4, c(-4,4), which.items = 7)
areainfo(mod4, c(-4,4), which.items = 8)


### Análise Fatorial Confirmatória EVB ###
install.packages("lavaan")
library(lavaan)

## configurando o modelo


names(ECB) [1] <- "f1"
comp_bullying <- 'Físico =~ f1 + X3 + X4 + X9
					Verbal =~ X12 + X14 + X11 + X15
					Relacional =~ X17 + X20 + X19 + X18
					Cyberbullying =~ X26 + X21 + X27 + X28'

## rodando o modelo
comp_bullying <- cfa(model = comp_bullying, estimator = "WLSMV", orthogonal = FALSE, data = ECB, se = "standard",test = "standard") 
summary(comp_bullying, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)


## gerarando figura CFA
install.packages("semPlot")
library(semPlot)
semPaths(comp_bullying, what='std',layout = "circle", title = "TRUE", curvePivot = "TRUE", edge.label.cex=1 , cut = 0.8 , residuals=FALSE, intercepts = FALSE, nCharNodes=0)


## Alfa e Omega 
install.packages("semTools")
library(semTools)
reliability(comp_bullying)

### Invariancia via Lavaan por equaltesteMI
install.packages("RItools")
install.packages("equaltestMI")
library(equaltestMI)

names(ECB)
View(ECB)

Comp_B <- 'fisico =~ x1 + X3 + X4 + X9
				verbal =~ X12 + X14 + X11 + X15
				relacional =~ X17 + X20 + X19 + X18
				cyberbullying =~ X26 + X21 + X27 + X28' 

run.sem1 <- eqMI.semtest(model = Comp_B, data = ECB, estimator = "WLSMV", 
                       orthogonal = FALSE, group = "Sexo", meanstructure = TRUE, 
                     ordered=c("f1","X3","X4", "X9", "X12", "X14", "X11", "X15", "X17", "X20", "X19", "X18", "X26", "X21", "X27", "X28")
                   )
