##### Librerias necesarias para trabajar

#remotes::install_github("wilkelab/cowplot")
# install.packages("pacman")
library(pacman)
p_load(tidyverse, lubridate, cowplot, openxlsx, directlabels, glue)

# Cargo funciones para armar gráficos
source("Funciones/func_grafico_evol_salario_perdida.R", encoding = "UTF-8")

# Cargo base de datos
base_orig <- read.xlsx("Entrada/base_informe_perdida.xlsx", detectDates = TRUE)

# objeto para gráfico
base_trab <- base_orig %>% 
  select(-c(y_d0_nominal, y_privado_nominal)) %>% 
  pivot_longer(-periodo, names_to = "indicador", values_to = "indice")

# Punto final para etiqueta
etiqueta <- c(glue("Inflación \n ({round(last(base_orig$inflacion_nov15),1)}%)"), 
              glue("Salaraio promedio \n sector privado \n ({round(last(base_orig$y_privado_evol[!is.na(base_orig$y_privado_evol)]),1)}%)"), 
              glue("Sector público \n SINEP \n ({round(last(base_orig$y_d0_evol),1)}%)"))


# Definir la base de trabajo y el mes de comparación en nro
graf_evol_perdida(base = base_trab, mes = 9)

# Definir la base de trabajo y el mes de comparación en nro
graf_evol_salario(base = base_trab, mes = 9)
