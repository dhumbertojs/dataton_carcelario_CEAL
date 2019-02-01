#Dataton anticorrupción
##Equipo CEAL
#Aida Bustos
#David H. Jimenez

setwd("C:\\Users\\Aida\\Documents\\Dataton\\Inp\\CORRUPCIÓN\\Percepción\\INEGI\\ENPOL")

inp = "C:\\Users\\Aida\\Documents\\Dataton\\Inp"
out = "C:\\Users\\Aida\\Documents\\Dataton\\Out"
graf = "C:\\Users\\Aida\\Documents\\Dataton\\Grafs"

#install.packages("foreign")
require(foreign)
require(dplyr)
require(readxl)
require(ggplot2)

enpol7 = read.dbf("ENPOL_SEC7_2.dbf")

enpol_7 <- enpol7 %>% 
  dplyr::select("ID_PER", "CVE_ENT", "NOM_ENT", "FUERO", "P7_51_1", "P7_51_2") %>% 
  filter(FUERO==2) %>% 
  mutate(
    custodio_malt = ifelse(P7_51_1 == 1, 1, 0),
    interno_malt = ifelse(P7_51_2 == 1, 1, 0)
  ) %>% 
  group_by(CVE_ENT) %>%
  summarize(
    custodio_malt = mean(custodio_malt, na.rm =T),
    interno_malt = mean(interno_malt, na.rm =T)
  )


colnames(enpol_7) <- c("clave_inegi", "custodio_malt", "interno_malt")

enpol8_9 = read.dbf("ENPOL_SEC8_9_10.dbf")
#write.csv(enpol8_9, paste(out, "ENPOL_SEC8_9_10.csv"), row.names = F)




enpol8_9 <- enpol8_9 %>% 
  dplyr::select("ID_PER", "CVE_ENT", "NOM_ENT", "FUERO", "P8_1_1", "P8_4_1", "P8_9_1", "P8_9_2", "P8_9_3","P8_9_4", "P8_9_5", "P8_9_6", "P9_1") %>%
  filter(FUERO==2) %>%
  mutate(
    autoridad_c = ifelse(P8_1_1 == 1, 1, 0),
    autoridad_noc = ifelse(P8_1_1 == 2, 1, 0),
    recluso_c = ifelse(P8_4_1== 1, 1, 0),
    recluso_noc = ifelse(P8_4_1== 0, 1, 0),
    pag_comida = ifelse(P8_9_1==1, 1, 0),
    pag_ropa = ifelse(P8_9_2==1, 1, 0),
    pag_objetos = ifelse(P8_9_3==1, 1, 0),
    pag_llamar = ifelse(P8_9_4==1, 1, 0),
    pag_entrar = ifelse(P8_9_5==1, 1, 0),
    pag_visitac = ifelse(P8_9_6==1, 1, 0),
    juzgado_si = ifelse(P9_1 == 1, 1, 0),
    juzgado_no = ifelse(P9_1 == 2, 1, 0)
  ) %>% 
  group_by(CVE_ENT, NOM_ENT) %>%
  summarize(
    autoridad_c = mean(autoridad_c),
    autoridad_noc = mean(autoridad_noc),
    recluso_c = mean(recluso_c, na.rm = T),
    recluso_noc = mean(recluso_noc, na.rm = T),
    pag_comida = mean(pag_comida, na.rm = T),
    pag_ropa = mean(pag_ropa, na.rm = T),
    pag_objetos = mean(pag_objetos, na.rm = T),
    pag_llamar = mean(pag_llamar, na.rm = T),
    pag_entrar = mean(pag_entrar, na.rm = T),
    pag_visitac = mean(pag_visitac, na.rm = T),
    juzgado_si = mean(juzgado_si, na.rm = T),
    juzgado_no = mean (juzgado_no, na.rm =T)
  )



enpol_soc <- enpol %>% 
  dplyr::select("ID_PER", "CVE_ENT", "NOM_ENT", "SEXO", "FUERO", "P1_1", "P1_7", "P1_12") %>% 
  filter(FUERO==2) %>% 
  mutate(
    hombre = ifelse(SEXO == 1, 1, 0),
    mujer = ifelse(SEXO == 2, 1, 0),
    centro_varonil = ifelse(P1_1== 1, 1, 0),
    centro_femenil = ifelse(P1_1== 2, 1, 0),
    hijes_fuera = ifelse(P1_7== 1, 1, 0),
    nones_fuera = ifelse(P1_7== 2, 1, 0),
    hijes_dentro = ifelse(P1_12== 1, 1, 0),
    nones_dentro = ifelse(P1_12== 2, 1, 0)
  ) %>% 
  group_by(CVE_ENT) %>%
  summarize(
    hombre = mean(hombre),
    mujer = mean(mujer),
    centro_varonil = mean(centro_varonil, na.rm = T),
    centro_femenil = mean(centro_femenil, na.rm = T),
    hijes_fuera = mean(hijes_fuera, na.rm = T),
    nones_fuera = mean(nones_fuera, na.rm = T),
    hijes_dentro = mean(hijes_dentro, na.rm = T),
    nones_dentro = mean(nones_dentro, na.rm = T)
  )


#JUNTAR BASES SOC Y 8_9

base = left_join(enpol_soc, enpol8_9, by="CVE_ENT") 

colnames(base) <- c("clave_inegi", "hombre", "mujer", "centro_varonil", "centro_femenil", "hijes_fuera","nones_fuera", "hijes_dentro", "nones_dentro", "nom_ent", "autoridad_c", "autoridad_noc",  "recluso_c", "recluso_noc", "pag_comida", "pag_ropa", "pag_objetos", "pag_llamar","pag_entrar", "pag_visitac", "juzgado_si", "juzgado_no")


#LIMPIAR POBREZA

pobreza = read_excel(paste(inp, "Evolución de pobreza y pobreza extrema nacional y en entidades_2010-2016.xlsx", sep="\\") , range = "b9:t41")

pobreza = select(pobreza, "X_1", "2016", "2016_2")

nombres_futuros = c("entidad", "pobreza", "pobreza_extrema")
names(pobreza)[1:3] = nombres_futuros

pobreza <- pobreza %>% mutate(clave_inegi = ifelse(entidad=="Aguascalientes", "01", ifelse( entidad=="Baja California", "02", ifelse(entidad== "Baja California Sur", "03", ifelse(entidad=="Campeche", "04", ifelse(entidad=="Chihuahua", "08", ifelse(entidad== "Chiapas","07", ifelse(entidad=="Coahuila", "05", ifelse(entidad=="Colima", "06", ifelse(entidad=="Distrito Federal", "09", ifelse(entidad=="Durango", "10", ifelse(entidad=="México", "15", ifelse(entidad=="Guanajuato", "11", ifelse(entidad== "Guerrero", "12", ifelse(entidad== "Hidalgo", "13", ifelse(entidad=="Jalisco", "14", ifelse(entidad== "Michoacán","16", ifelse(entidad=="Morelos","17", ifelse(entidad=="Nayarit", "18", ifelse(entidad=="Oaxaca", "20", ifelse(entidad=="Nuevo León", "19", ifelse(entidad== "Puebla", "21", ifelse(entidad=="Querétaro", "22", ifelse(entidad=="Quintana Roo", "23", ifelse(entidad=="San Luis Potosí", "24", ifelse(entidad== "Sinaloa", "25", ifelse(entidad=="Sonora", "26", ifelse(entidad=="Tabasco", "27", ifelse(entidad=="Tamaulipas", "28", ifelse(entidad=="Tlaxcala", "29", ifelse(entidad=="Veracruz", "30", ifelse(entidad=="Yucatán", "31", "32" ))))))))))))))))))))))))))))))))

#JUNTAR BASES ENPOL Y POBREZA

base1 = left_join(base, pobreza, by="clave_inegi") 


#CNDH

perc_cndh <- "C:\\Users\\Aida\\Documents\\Dataton\\Inp\\CORRUPCIÓN\\Percepción\\CNDH DNSP"
percepcion <- read_excel(paste(perc_cndh, "CNDH DNSP 2012-2017.xlsx", sep="\\"))

cndh <- percepcion %>%
  dplyr::select( "year", "entidad", "tipo", "sobrepoblacion", "autogobierno", "presencia_menores", "pago_cuota", "cuota_custodios", "cuota_internos") %>% 
  filter(tipo == "Sistema Estatal" & year==2016) %>% 
  mutate(
    sobrepob = ifelse(sobrepoblacion=="Sí", 1, 0),
    autogob = ifelse(autogobierno=="Sí", 1, 0),
    menores = ifelse(presencia_menores=="Sí", 1, 0), 
    pago_cuota = ifelse(pago_cuota=="Sí", 1, 0),
    cuota_custodios = ifelse(cuota_custodios=="Sí", 1, 0),
    cuota_internos = ifelse(cuota_internos=="Sí", 1, 0),
    clave_inegi = ifelse(entidad=="Aguascalientes", "01", ifelse( entidad=="Baja California", "02", ifelse(entidad== "Baja California Sur", "03", ifelse(entidad=="Campeche", "04", ifelse(entidad=="Chihuahua", "08", ifelse(entidad== "Chiapas","07", ifelse(entidad=="Coahuila", "05", ifelse(entidad=="Colima", "06", ifelse(entidad=="Distrito Federal", "09", ifelse(entidad=="Durango", "10", ifelse(entidad=="Estado de México", "15", ifelse(entidad=="Guanajuato", "11", ifelse(entidad== "Guerrero", "12", ifelse(entidad== "Hidalgo", "13", ifelse(entidad=="Jalisco", "14", ifelse(entidad== "Michoacán","16", ifelse(entidad=="Morelos","17", ifelse(entidad=="Nayarit", "18", ifelse(entidad=="Oaxaca", "20", ifelse(entidad=="Nuevo León", "19", ifelse(entidad== "Puebla", "21", ifelse(entidad=="Querétaro", "22", ifelse(entidad=="Quintana Roo", "23", ifelse(entidad=="San Luis Potosí", "24", ifelse(entidad== "Sinaloa", "25", ifelse(entidad=="Sonora", "26", ifelse(entidad=="Tabasco", "27", ifelse(entidad=="Tamaulipas", "28", ifelse(entidad=="Tlaxcala", "29", ifelse(entidad=="Veracruz", "30", ifelse(entidad=="Yucatán", "31", "32" )))))))))))))))))))))))))))))))) %>% 
  group_by(clave_inegi) %>% 
  summarise(pago_cuota = mean(pago_cuota),
            cuota_custodios = mean(cuota_custodios),
            cuota_internos = mean(cuota_internos),
            sobrepob = mean(sobrepob),
            autogob = mean(autogob),
            menores = mean(menores))

#JUNTAR BASES ENPOL/POBREZA Y CNDH

completa = left_join(base1, cndh, by="clave_inegi") 

completa$nom_ent = NULL

completa = left_join(completa, enpol_7, by="clave_inegi")

completa <- completa %>% mutate(
  hombre = hombre*100,
  mujer = mujer*100          ,
  centro_varonil = centro_varonil*100 , 
  centro_femenil = centro_femenil*100,  
  hijes_fuera = hijes_fuera*100    ,
  nones_fuera = nones_fuera*100    ,
  hijes_dentro = hijes_dentro*100    ,
  nones_dentro = nones_dentro*100   ,
  autoridad_c = autoridad_c*100     ,
  autoridad_noc = autoridad_noc*100,   
  recluso_c = recluso_c*100      ,
  recluso_noc = recluso_noc*100   ,  
  pag_comida = pag_comida*100    ,  
  pag_ropa = pag_ropa*100       ,
  pag_objetos = pag_objetos*100    , 
  pag_llamar = pag_llamar*100     , 
  pag_entrar = pag_entrar*100    , 
  pag_visitac = pag_visitac*100 ,   
  juzgado_si = juzgado_si*100  ,    
  juzgado_no = juzgado_no*100 ,    
  pago_cuota = pago_cuota*100,
  cuota_custodios = cuota_custodios*100, 
  cuota_internos = cuota_internos*100, 
  sobrepob = sobrepob*100,        
  autogob = autogob*100,         
  menores = menores*100,
  custodio_malt = custodio_malt*100,   
  interno_malt = interno_malt*100
)

write.csv(completa, paste(out, "completa_dataton.csv"), row.names = F)


#completa = completa [,c]


#DESCRIPTIVOS(ISH)

ggpairs(completa[,c("autoridad_c", "recluso_c", "pag_comida", "pag_ropa", "juzgado_si", "pobreza", "pobreza_extrema", "sobrepob", "autogob")])
ggsave(paste(graf, "corr01.png", sep="\\"), width=12, height=12)

#SCATTERPLOTS

ggplot(completa) +
  geom_point(aes(x = pobreza, y = autoridad_c, size = autoridad_c)) +
  geom_smooth(aes(x= pobreza, y= autoridad_c), method="lm", formula=y ~ x + I(x^2), se=F, color="blue") +
  labs(title ="Pobreza y corrupción", 
       subtitle="Relación entre pobreza estatal y corrupción de autoridades", 
       y = "% custodio corruptos") +
  theme_bw() 
ggsave(paste(graf, "scatter01.png", sep="\\"), width=12, height=9)


ggplot(completa) +
  geom_point(aes(x = pobreza, y = juzgado_si, size = juzgado_si)) +
  geom_smooth(aes(x= pobreza, y= juzgado_si), method="lm", formula=y ~ x + I(x^2), se=F, color="blue") +
  labs(title ="Pobreza y reinsidencia", 
       subtitle="Relación entre pobreza estatal y reinsidencia", 
       y = "% de reincidir") +
  theme_bw() 
ggsave(paste(graf, "scatter02.png", sep="\\"), width=12, height=9)

ggplot(completa) +
  geom_point(aes(x = pobreza, y = sobrepob, size = sobrepob)) +
  geom_smooth(aes(x= pobreza, y= sobrepob), method="lm", formula=y ~ x + I(x^2), se=F, color="blue") +
  labs(title ="Pobreza y sobrepoblación carcelaria", 
       subtitle="Relación entre pobreza estatal y sobrepoblación", 
       y = "% de sobrepoblación en prisiones") +
  theme_bw() 
ggsave(paste(graf, "scatter03.png", sep="\\"), width=12, height=9)

ggplot(completa) +
  geom_point(aes(x = pobreza, y = autogob, size = autogob)) +
  geom_smooth(aes(x= pobreza, y= autogob), method="lm", formula=y ~ x + I(x^2), se=F, color="blue") +
  labs(title ="Pobreza y autogobierno carcelario", 
       subtitle="Relación entre pobreza estatal y autogobierno", 
       y = "% de prisiones en autogobierno") +
  theme_bw() 
ggsave(paste(graf, "scatter04.png", sep="\\"), width=12, height=9)


ggplot(completa) +
  geom_point(aes(x = custodio_malt, y = autogob, size = autogob)) +
  geom_smooth(aes(x= custodio_malt, y= autogob), method="lm", formula=y ~ x + I(x^2), se=F, color="blue") +
  labs(title ="Maltrato de custudios y autogob", 
       subtitle="Relación entre maltrato y autogobierno", 
       y = "% de prisiones en autogobierno") +
  theme_bw() 
ggsave(paste(graf, "scatter05.png", sep="\\"), width=12, height=9)

ggplot(completa) +
  geom_point(aes(x = interno_malt, y = autogob, size = autogob)) +
  geom_smooth(aes(x= interno_malt, y= autogob), method="lm", formula=y ~ x + I(x^2), se=F, color="blue") +
  labs(title ="Maltrato de internos y autogob", 
       subtitle="Relación entre maltrato y autogobierno", 
       y = "% de prisiones en autogobierno") +
  theme_bw() 
ggsave(paste(graf, "scatter06.png", sep="\\"), width=12, height=9)

ggplot(completa) +
  geom_point(aes(x = interno_malt, y = sobrepob, size = sobrepob)) +
  geom_smooth(aes(x= interno_malt, y= sobrepob), method="lm", formula=y ~ x + I(x^2), se=F, color="blue") +
  labs(title ="Maltrato de internos y sobrepoblación", 
       subtitle="Relación entre maltrato y sobrepoblación", 
       y = "% de prisiones con sobrepoblación") +
  theme_bw() 
ggsave(paste(graf, "scatter07.png", sep="\\"), width=12, height=9)

ggplot(completa) +
  geom_point(aes(x = custodio_malt, y = sobrepob, size = sobrepob)) +
  geom_smooth(aes(x= custodio_malt, y= sobrepob), method="lm", formula=y ~ x + I(x^2), se=F, color="blue") +
  labs(title ="Maltrato de custodios y sobrepoblación", 
       subtitle="Relación entre maltrato y sobrepoblación", 
       y = "% de prisiones con sobrepoblación") +
  theme_bw() 
ggsave(paste(graf, "scatter08.png", sep="\\"), width=12, height=9)