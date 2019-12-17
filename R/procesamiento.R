### Extraigo las tablas del pdf vía: pdftables.com, donde cada tabla es una página.

library(tidyverse)
library(eph)
library(readxl)
library(rlist)

### 
secuencia_par<-seq(2,48,2)
secuencia_impar<-seq(1,48,2)
paginas <- NA
### Objeto con la cantidad de páginas
for (i in 1:48) {
  paginas[i] <- c(paste0("Page ", i))
}

### Guardo en una lista las páginas en función de la primera fila con datos
base_lista <- list()
for(x in 1:48) {
  ifelse(x %in% c(secuencia_par),
         base_lista[[x]] <- read_excel("Fuente/sinep-art9-contratados_enero2019.perpage.xlsx", 
                                       sheet=paginas[x], skip = 4), 
         base_lista[[x]] <- read_excel("Fuente/sinep-art9-contratados_enero2019.perpage.xlsx", 
                                       sheet=paginas[x], skip = 7))
}

### Cargo nombre de las columnas y limpio valores NA
nombres_9 <- c("nivel", "grado", "sueldo", "dedicacion_funcional", "asig_basica",	"adicional_grado",
               "subtotal", "compensacion_transitoria", "salario_conformado")
nombres_10 <- c("nivel", "grado", "sueldo", "dedicacion_funcional", "asig_basica",	"adicional_grado",
                "subtotal", "compensacion_transitoria", "suma_fija_unica_vez", "salario_conformado")

for (i in 1:48) {
  ifelse(length(base_lista[[i]]) == 9,
         colnames(base_lista[[i]]) <- nombres_9,
         colnames(base_lista[[i]]) <- nombres_10)
  ### Saco últimas filas sin dato
  base_lista[[i]] <- na.omit(base_lista[[i]])
}

### Macheo bases que se encuentran separadas (de la A a la B y de la D a la F)
base_lista_final <- list()
for (i in unique(secuencia_impar)) {
  base_lista_final[[i]] <- rbind(base_lista[[i]], base_lista[[i+1]])
}

### Me quedo sólo con los elementos con valores
base_lista_final <- base_lista_final %>% 
  discard(is.null)

### Agrego tipo de agrupamiento de las escalas
names(base_lista_final)[1:8] <- "Universitario"
names(base_lista_final)[9:16] <- "Terciario"
names(base_lista_final)[17:24] <- "Operativa"

agrupamiento <- c("Universitario", "Terciario", "Operativa")

base_lista_final[1]
for (i in unique(agrupamiento)) {
  base_lista_final[i] <- base_lista_final[i]
}

### Agrego mes al que corresponde la escala
# Meses del año
#meses <- format(ISOdate(2019,1:12,1),"%B") %>% 
meses <- factor(x = c("Enero", "Febrero", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre"),
                levels = c("Enero", "Febrero", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre"))

for (i_mes in meses) {
  for (i_lista in 1:24) {
    base_lista_final[[i_lista]] <- base_lista_final[[i_lista]] %>% 
      mutate(mes = i_mes)
  }
}
library(rlist)
list.filter(.data = base_lista_final, Name == "Universitario")

prueba <- base_lista_final[[1]]
base_lista_final <- base_lista_final %>% 
  filter(Name == "Universitario")

lapply(base_lista_final, function(x) {
  x$Name
})
