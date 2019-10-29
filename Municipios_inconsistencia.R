#     rm(list = ls()) 

# carregamento ----
source(file = "../FUNA_Eventos_RRR_RFP.R")
source(file = "../Funcoes_Consumidores.R")
source(file = "../Funcoes_de_Formatacao_Estilo.R")
source(file = "../Funcoes_Producao.R")
source(file = "../Funcoes_Reserva.R")
source(file = "../Funcoes_VPM.R")
source(file = "./carregamento_Bases_AMB_outras.r")
#_______________________________________________________________________________________________----



geocod <- 
  read.table("D:/Users/humberto.serna/Desktop/CSV_Data/geocod.csv",header = TRUE, sep = ";",quote = "" )[,c(1,2)][,c(1,as.integer(2))]

geocod[,1] <- FUNA_minusculas(FUNA_removeAcentos(geocod[,1]))

geocod_processo <- 
  spread(left_join(
    unique(reserva_AMB[!reserva_AMB$substancia.amb %in% c("areia", "saibro", "brita e cascalho"), c("processo", "ano", "municipio")]),
    geocod,
    by = c("municipio" = "NM_MUNICIP_STRING")),key = "ano",value = "CD_GEOCMU_STRING")




inconsistencia_municipio <-
  left_join(filter(data.frame(table(
    geocod_processo$processo
  )), Freq > 1),
  geocod_processo,
  by = c("Var1" = "processo"))





