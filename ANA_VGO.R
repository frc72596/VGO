#Instalar paquetes para hacer analisis
install.packages("readxl") #leer documentos de excel en R
install.packages("psych") # analisis psicometricos- alpha consistencia
install.packages("EFAtools") # Analisis en paralelo -Nfactors 
install.packages("lavaan") # hacer el analisis factorial confirmatorio CFA
install.packages("lavaanPlot") # graficar el CFA
install.packages("mirt") #analisis de teoria de respuesta al item
devtools::install_github("masurp/ggmirt") # graficos de tri
install.packages("forcats") # complementario
#Llamar los paquetes
library(readxl)
library(psych)
library(EFAtools)
library(lavaan)
library(lavaanPlot)
library(mirt)
library(ggmirt)
#Cargar la base de datos y las sub-bases de analisis
VGO_VO <- read_excel("VGO_VO.xlsx") #base completa
VG_IT<-VGO_VO[c(28,29,30,31,32,33,34,35,36,37,38,39,40)] #items de Violencia Gine
VG_IT$VG_2_I<-6-VG_IT$VG_2 # Inversion del item 2
VG_IT$VG_12_I<-6-VG_IT$VG_12 # Inversión del item 12
VG_F<-VG_IT[c(-2,-12)]
VO_BAS<-subset(VGO_VO,VGO_VO$RVO==1) # base de solo mujeres con VO
VO_IT<-VO_BAS[c(43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59)] #tems de Violencia O
#resultados de la escala de VG completa consistencia interna
alpha(VG_F)
reliability(VG_F)
# suspuestos del analisis factorial y analis en paralelo para VG
N_FACTORS(VG_F)
#ANALISIS FACTORIAL CONFIRMATORIO
VG_Mod<-"D1=~ VG_1+VG_2_I+VG_3+VG_6+VG_7 
          D2=~ VG_4+VG_5+VG_8+VG_10+VG_11
          D3=~ VG_9+VG_12_I+VG_13"
VG_A<- cfa(VG_Mod,data =VG_F)
summary(VG_A, fit.measures = TRUE)
lavaanPlot(model = VG_A, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, stand = TRUE,covs = TRUE, stars = "latent")
#analisis por escala de VG- CON TRI
D1_VG<-VG_IT[c(1,3,6,7,14)]
D2_VG<-VG_IT[c(4,5,8,10,11)]
D3_VG<-VG_IT[c(9,15,13)]
#Dimension 1
alpha(D1_VG)
reliability(D1_VG)
D1_VG_TRI<-mirt(D1_VG,itemtype = "graded")
plot(D1_VG_TRI,type='trace')
plot(D1_VG_TRI, type = 'infoSE', theta_lim = c(-3, 3), 
     main="")
#Dimension 2
alpha(D2_VG)
reliability(D2_VG)
D2_VG_TRI<-mirt(D2_VG,itemtype = "graded")
plot(D2_VG_TRI,type='trace')
plot(D2_VG_TRI, type = 'infoSE', theta_lim = c(-3, 3), 
     main="")
#Dimension 3
alpha(D3_VG)
reliability(D3_VG)
D3_VG_TRI<-mirt(D3_VG,itemtype = "graded")
plot(D3_VG_TRI,type='trace')
plot(D3_VG_TRI, type = 'infoSE', theta_lim = c(-3, 3), 
     main="")
#ANALISIS PARA LA ESCALA DE VIOLENCIA OBSTRETICA
alpha(VO_IT,check.keys = TRUE)
reliability(VO_IT)
#ANALISIS FACTORIAL CONFIRMATORIO
VO_Mod<-"VO=~ VO_1+VO_2+VO_3+VO_4+VO_5+VO_6+VO_7+VO_8+VO_9+VO_10+VO_11+VO_12+VO_13+VO_14+VO_15+VO_16+VO_17"
V0_A<- cfa(VO_Mod,data =VO_IT)
summary(V0_A, fit.measures = TRUE)
lavaanPlot(model = V0_A, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, stand = TRUE,covs = TRUE, stars = "latent")
VO_TRI<-mirt(VO_IT,itemtype = "graded")
plot(VO_TRI,type='trace')
plot(VO_TRI, type = 'infoSE', theta_lim = c(-3, 3), 
     main="")
