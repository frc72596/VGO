#Instalar paquetes para hacer analisis
install.packages("readxl") #leer documentos de excel en R
install.packages("psych") # analisis psicometricos- alpha consistencia
install.packages("EFAtools") # Analisis en paralelo -Nfactors 
install.packages("lavaan") # hacer el analisis factorial confirmatorio CFA
install.packages("lavaanPlot") # graficar el CFA
install.packages("mirt") #analisis de teoria de respuesta al item
devtools::install_github("masurp/ggmirt") # graficos de tri
install.packages("forcats") # complementario
library(readxl)
library(psych)
library(EFAtools)
library(lavaan)
library(lavaanPlot)
library(mirt)
library(ggmirt)
VGO_VO <- read_excel("VGO_VO.xlsx")
VG_IT<-VGO_VO[c(28,29,30,31,32,33,34,35,36,37,38,39,40)]
VG_IT$VG_2_I<-6-VG_IT$VG_2
VG_IT$VG_12_I<-6-VG_IT$VG_12
VO_BAS<-subset(VGO_VO,VGO_VO$RVO==1)
VO_IT<-VO_BAS[c(43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59)]
alpha(VG_IT)
alpha(VO_IT)
N_FACTORS(VG_IT)
N_FACTORS(VO_IT)
VG_Mod<-"D1=~ VG_1+VG_2_I+VG_3+VG_6+VG_7 
          D2=~ VG_4+VG_5+VG_8+VG_10+VG_11
          D3=~ VG_9+VG_12_I+VG_13"
VG_A<- cfa(VG_Mod,data =VG_IT)
lavaanPlot(model = VG_A, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, stand = TRUE,covs = TRUE, stars = "latent")
D1_VG<-VG_IT[c(1,3,6,7,14)]
D2_VG<-VG_IT[c(4,5,8,10,11)]
D3_VG<-VG_IT[c(9,15,13)]
alpha(D1_VG)
alpha(D2_VG)
alpha(D3_VG)
reliability(D1_VG)
reliability(D2_VG)
reliability(D3_VG)
D1_VG_TRI<-mirt(D1,itemtype = "graded")
plot(D1_VG_TRI,type='trace')
plot(D1_VG_TRI, type='infotrace')
plot(D1_VG_TRI, type = 'infoSE', theta_lim = c(-3, 3), 
     main="")
summary(VG_A, fit.measures = TRUE)

