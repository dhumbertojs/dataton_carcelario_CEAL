#Dataton anticorrupción
##Equipo CEAL

library(readxl)
library(dplyr)
library(foreign)

fold_cuenta <- "/Users/Emmanuel RB/Documents/CORRUPCIÓN/Cuenta Publica"
#mejores que contratos
#Empezar por aqui

#gasto_15 <- "/Users/Emmanuel RB/Documents/CORRUPCIÓN/Gasto Federalizado/2015"
#gasto_16 <- "/Users/Emmanuel RB/Documents/CORRUPCIÓN/Gasto Federalizado/2016"
#gasto_17 <- "/Users/Emmanuel RB/Documents/CORRUPCIÓN/Gasto Federalizado/2017"
#gasto_18 <- "/Users/Emmanuel RB/Documents/CORRUPCIÓN/Gasto Federalizado/2018"

fold_obra <- "/Users/Emmanuel RB/Documents/CORRUPCIÓN/Obras"
perc_cndh <- "/Users/Emmanuel RB/Documents/CORRUPCIÓN/Percepción/CNDH DNSP"

setwd("C:\\Users\\Emmanuel RB\\Documents\\CORRUPCIÓN\\Percepción\\INEGI\\ENPOL") 
enpol2_3_4 <- read-dbf("ENPOL_SEC2_3_4.dbf")
enpol5_6 <- read.dbf("ENPOL_SEC_5_6.dbf")
enpol7_1 <- read.dbf("ENPOL_SEC7_1.dbf")
enpol7_2 <- read.dbf("ENPOL_SEC7_2.dbf")
enpol8_9_10 <- read.dbf("ENPOL_SEC8_9_10.dbf")

percepcion <- read_excel(paste(perc_cndh, "CNDH DNSP 2012-2017.xlsx", sep="/"))

cuenta12 <- read.csv(paste(fold_cuenta, "Cuenta_Publica_2012.csv", sep="/"))
