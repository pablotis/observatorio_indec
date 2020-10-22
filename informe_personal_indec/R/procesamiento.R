### Extraigo las tablas del pdf vía: pdftables.com / ilovepdf.com, 
# donde cada tabla es una página.

library(tidyverse)
library(eph)
library(readxl)
library(rlist)


### 
secuencia_par<-seq(2,48,2)
secuencia_impar<-seq(1,48,2)
paginas <- NA
### Objeto con la cantidad de páginas
# for (i in 1:48) {
#   paginas[i] <- c(paste0("Page ", i))
# }
for (i in 1:24) {
  paginas[i] <- c(paste0("Table ", i))
}

### Guardo en una lista las páginas en función de la primera fila con datos
base_lista <- list()
# for(x in 1:48) {
#   ifelse(x %in% c(secuencia_par),
#          base_lista[[x]] <- read_excel("Fuente/sinep-art9-contratados_enero2019.perpage.xlsx", 
#                                        sheet=paginas[x], skip = 4), 
#          base_lista[[x]] <- read_excel("Fuente/sinep-art9-contratados_enero2019.perpage.xlsx", 
#                                        sheet=paginas[x], skip = 7))
# }
for(x in 1:48) {
  base_lista[[x]] <- read_excel("Fuente/sinep-art9-contratados_enero2019.xlsx",
                                       sheet=paginas[x])
}
prueba <- base_lista[[1]]
### Cargo nombre de las columnas y limpio valores NA
nombres_9 <- c("nivel", "grado", "sueldo", "dedicacion_funcional", "asig_basica",	"adicional_grado",
               "subtotal", "compensacion_transitoria", "salario_conformado")
nombres_10 <- c("nivel", "grado", "sueldo", "dedicacion_funcional", "asig_basica",	"adicional_grado",
                "subtotal", "compensacion_transitoria", "suma_fija_unica_vez", "salario_conformado")

for (i in 1:24) {
  ifelse(length(base_lista[[i]]) == 9,
         colnames(base_lista[[i]]) <- nombres_9,
         colnames(base_lista[[i]]) <- nombres_10)
  ### Saco últimas filas sin dato
  base_lista[[i]] <- na.omit(base_lista[[i]])
}

# ### Macheo bases que se encuentran separadas (de la A a la B y de la D a la F)
# base_lista_final <- list()
# for (i in unique(secuencia_impar)) {
#   base_lista_final[[i]] <- rbind(base_lista[[i]], base_lista[[i+1]])
# }
# 
# ### Me quedo sólo con los elementos con valores
# base_lista_final <- base_lista_final %>% 
#   discard(is.null)

### Agrego tipo de agrupamiento de las escalas
names(base_lista)[1:8] <- "Universitario"
names(base_lista)[9:16] <- "Terciario"
names(base_lista)[17:24] <- "Operativa"

#agrupamiento <- c("Universitario", "Terciario", "Operativa")

### Agrego mes al que corresponde la escala
# Meses del año
#meses <- format(ISOdate(2019,1:12,1),"%B") %>% 
# meses <- c("Enero", "Febrero", "Abril", "Mayo", "Junio", "Julio", "Agosto", 
#                       "Septiembre")

# meses2 <- c(rep(c("Enero", "Febrero", "Abril", "Mayo", "Junio", "Julio",
#                   "Agosto", "Septiembre"), 3))


base_lista[[1]]$mes <- "Enero"
base_lista[[2]]$mes <- "Febrero"
base_lista[[3]]$mes <- "Abril"
base_lista[[4]]$mes <- "Mayo"
base_lista[[5]]$mes <- "Junio"
base_lista[[6]]$mes <- "Julio"
base_lista[[7]]$mes <- "Agosto"
base_lista[[8]]$mes <- "Septiembre"

base_lista[[9]]$mes <- "Enero"
base_lista[[10]]$mes <- "Febrero"
base_lista[[11]]$mes <- "Abril"
base_lista[[12]]$mes <- "Mayo"
base_lista[[13]]$mes <- "Junio"
base_lista[[14]]$mes <- "Julio"
base_lista[[15]]$mes <- "Agosto"
base_lista[[16]]$mes <- "Septiembre"

base_lista[[17]]$mes <- "Enero"
base_lista[[18]]$mes <- "Febrero"
base_lista[[19]]$mes <- "Abril"
base_lista[[20]]$mes <- "Mayo"
base_lista[[21]]$mes <- "Junio"
base_lista[[22]]$mes <- "Julio"
base_lista[[23]]$mes <- "Agosto"

### Contratados universitarios
contratados_universitario <- list()
for (i in 1:8) {
  contratados_universitario[[i]] <- base_lista[[i]]
}
contratados_universitario <- bind_rows(contratados_universitario) %>% 
  mutate(agrupamiento = "universitario")

### Contratados Terciario
contratados_terciario <- list()
for (i in 9:16) {
  contratados_terciario[[i]] <- base_lista[[i]]
}
contratados_terciario <- bind_rows(contratados_terciario) %>% 
  mutate(agrupamiento = "terciario")

### Contratados Operativo
contratados_operativo <- list()
for (i in 17:23) {
  contratados_operativo[[i]] <- base_lista[[i]]
}
contratados_operativo <- bind_rows(contratados_operativo) %>% 
  mutate(agrupamiento = "operativo")

variables_a_miles <- names(base_lista[[1]][3:9])

base_contratados <- rbind(contratados_operativo, 
                          contratados_terciario, 
                          contratados_universitario) %>% 
  mutate(sueldo = sueldo * 1000,
         dedicacion_funcional = dedicacion_funcional *1000,
         asig_basica = asig_basica * 1000,
         adicional_grado = adicional_grado * 1000,
         subtotal = subtotal * 1000,
         compensacion_transitoria = compensacion_transitoria * 1000,
         salario_conformado = salario_conformado * 1000,
         suma_fija_unica_vez = suma_fija_unica_vez * 1000,
         sueldo_neto = ifelse(!is.na(suma_fija_unica_vez),
                              (salario_conformado - suma_fija_unica_vez) * 0.82,
                              salario_conformado * 0.82))

grado_cero_septiembre <- base_contratados %>% 
  filter(agrupamiento == "universitario" & grado == 0 & mes == "Septiembre")
