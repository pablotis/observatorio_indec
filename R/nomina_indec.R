### Extraigo las tablas del pdf vía: pdftables.com / ilovepdf.com, 
# donde cada tabla es una página.

library(tidyverse)
library(eph)
library(readxl)
library(rlist)

### 
prueba$CUIL
nomina_tabla1 <- read_excel("Fuente/Nomina_personal_indec.xlsx",
                              sheet = "Table 1", skip = 2) %>% 
  mutate(cuil = CUIL,
         apellido_nombre = case_when(is.na(...2) ~ `APELLIDO Y NOMBRES`,
                                     !is.na(...2) ~ ...2),
         dni = case_when(is.na(...5) ~ TIPO,
                         !is.na(...5) ~ ...5),
         nro_documento = DOCUMENTO,
         convenio = case_when(is.na(...7) ~ CONVENIO,
                         !is.na(...7) ~ ...7),
         categoria = case_when(is.na(...9) ~ CATEGORÍA,
                              !is.na(...9) ~ ...9),
         dependencia = case_when(is.na(...11) ~ DESCRIPCIÓN,
                               !is.na(...11) ~ ...11)) %>% 
  select(-c(1:12))
         
paginas_nomina <- NA

for (i in 2:9) {
  paginas_nomina[i] <- c(paste0("Table ", i))
}

### Guardo en una lista las páginas en función de la primera fila con datos
lista_nomina <- list()

for (i in 2:9) {
  lista_nomina[[i]] <- read_excel("Fuente/Nomina_personal_indec.xlsx",
                                       sheet = paginas_nomina[i], 
                                       col_names = FALSE)
}

lista_nomina[[3]] <- lista_nomina[[3]] %>% 
  mutate(cuil = ...1,
         apellido_nombre = ...2,
         dni = case_when(!is.na(...3) ~ ...3,
                         !is.na(...4) ~ ...4,
                         !is.na(...5) ~ ...5,
                         !is.na(...6) ~ ...6,
                         !is.na(...7) ~ ...7),
         nro_documento = ...8,
         convenio = ...9,
         categoria = case_when(!is.na(...10) ~ ...10,
                               !is.na(...11) ~ ...11),
         dependencia = ...12) %>% 
  select(-c(1:12))

lista_nomina[[7]] <- lista_nomina[[7]] %>% 
  mutate(cuil = ...1,
         apellido_nombre = ...2,
         dni = case_when(!is.na(...3) ~ ...3,
                         !is.na(...4) ~ ...4),
         nro_documento = ...5,
         convenio = ...6,
         categoria = ...7,
         dependencia = ...8) %>% 
  select(-c(1:8))


prueba <- lista_nomina[[9]]



nombres_columnas <- names(nomina_tabla1)
names(prueba2) <- nombres_columnas

names(lista_nomina[[2]]) <- nombres_columnas

prueba <- lista_nomina[[3]]



