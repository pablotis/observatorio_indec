


################################################### Informe pérdida para proyección #################

##### Librerias necesarias para trabajar

#remotes::install_github("wilkelab/cowplot")
# install.packages("pacman")
library(pacman)
p_load(tidyverse, lubridate, cowplot, openxlsx, directlabels, glue, readxl, ggtext)

# Cargo funciones para armar gráficos
source("Funciones/func_grafico_evol_salario_perdida.R", encoding = "UTF-8")



# Cargo base de datos para proyección D5
b_e5 <- read_excel("Entrada/base_informe_perdida.xlsx", sheet = "E5") %>%  mutate(categoria = "E5") %>% 
  select(periodo, y_evolucion = y_e5_evol, perdida, categoria)

b_d5 <- read_excel("Entrada/base_informe_perdida.xlsx", sheet = "D5") %>%  mutate(categoria = "D5") %>% 
  select(periodo, y_evolucion = y_d5_evol, perdida, categoria)

b_c5 <- read_excel("Entrada/base_informe_perdida.xlsx", sheet = "C5") %>%  mutate(categoria = "C5") %>% 
  select(periodo, y_evolucion = y_c5_evol, perdida, categoria)

b_b5 <- read_excel("Entrada/base_informe_perdida.xlsx", sheet = "B5") %>%  mutate(categoria = "B5") %>% 
  select(periodo, y_evolucion = y_b5_evol, perdida, categoria)

b_inflacion <- read_excel("Entrada/base_informe_perdida.xlsx", sheet = "E5") %>%  mutate(categoria = "Inflación") %>% 
  select(periodo, perdida = inflacion_jun19, categoria)

b_total <- bind_rows(b_e5, b_d5, b_c5, b_b5, b_inflacion) %>% 
  mutate(y_evolucion = case_when(categoria == "Inflación" ~ perdida,
                                 TRUE ~ y_evolucion))

# etiqueta <- c(glue("B5 \n ({round(last(b_total$perdida[b_total$categoria == 'B5']),1)})"), 
#               glue("C5 \n ({round(last(b_total$perdida[b_total$categoria == 'C5']),1)})"),
#               glue("D5 \n ({round(last(b_total$perdida[b_total$categoria == 'D5']),1)})"),
#               glue("E5 \n ({round(last(b_total$perdida[b_total$categoria == 'E5']),1)})"))

ultimo_dato <- b_total %>% 
  filter(periodo == ymd("2021-05-01"))

b_total %>% 
  mutate(periodo = ymd(periodo)) %>% 
  ggplot() +
  geom_line(aes(x = periodo, y = as.numeric(perdida), color = categoria),
            size = 2, alpha = 0.9) +
  #geom_point() +
  labs(title = "Evolución del salario real para el sector público*",
       subtitle = "Junio de 2019 = 100. Proyección a mayo 2021",
       caption = "* Corresponde al escalafón de Planta Transitoria y Contratados, niveles E,D,C,B, grado 5. ",
       x = "Período", y = "Índice") +
  scale_x_date(date_labels="%b %Y ",
               #expand = c(0.2, 0), 
               breaks = function(x) seq.Date(from = as.Date(min(b_total$periodo)), 
                                             to = as.Date(max(b_total$periodo)), 
                                             by = "4 months")) +
  geom_text(data=ultimo_dato,
            aes(x = ymd(periodo), y = perdida, color = categoria,
                label = format(perdida, big.mark = ".", decimal.mark = ",", digits = 2),
                vjust = -1),
            size = 5) +
  cowplot::theme_minimal_vgrid() +
  theme(legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        text = element_text(size = 25),
        axis.text = element_text(size = 20),
        plot.caption = element_text(size = 13, vjust = -.1)) +
  colorblindr::scale_color_OkabeIto() +  
  facet_wrap(facets = "categoria", scales = "free") +
  ggsave(filename = "Salida/proyeccion.png", width = 20, height = 10, dpi = 300)




b_total %>% 
  mutate(periodo = ymd(periodo)) %>% 
  ggplot(aes(x = periodo, y = as.numeric(y_evolucion))) +
  geom_line(aes(color = categoria),
            size = 1.8,  alpha = 0.9) +
  geom_point(aes(shape = categoria, color = categoria),
             size = 5) +
  scale_x_date(date_labels="%b \n%Y ",
               #expand = c(0.2, 0), 
               breaks = function(x) seq.Date(from = as.Date(min(b_total$periodo)), 
                                             to = as.Date(max(b_total$periodo)), 
                                             by = "1 months")) +
  scale_color_manual(values = c("Inflación" = "#d7191c",
                                "E5" = "#fdae61",
                                "D5" = "black",
                                "C5" = "#abdda4",
                                "B5" = "#2b83ba")) +
  scale_shape_manual(name = "Referencia",
                     values = c("Inflación" = 8,
                                "E5" = 15,
                                "D5" = 16,
                                "C5" = 17,
                                "B5" = 18)) +
  guides(color = FALSE,
         shape = guide_legend(override.aes = list(color = rev(c("#d7191c", "#fdae61","black",
                                                           "#abdda4", "#2b83ba")),
                                                  size = 6))) +
  geom_text(data=ultimo_dato,
            aes(x = ymd(periodo), y = y_evolucion, color = categoria,
                label = format(y_evolucion, big.mark = ".", decimal.mark = ",", digits = 2),
                vjust = -0.5, hjust = -0.25),
            size = 6) +
  cowplot::theme_minimal_vgrid() +
  theme(
    #legend.position = "none",
    #legend.key.size = unit(1,"line"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    plot.subtitle = element_markdown(),
    text = element_text(size = 25),
    axis.text = element_text(size = 15),
    plot.caption = element_text(size = 13, vjust = -.1)) +
  #colorblindr::scale_color_OkabeIto() +
  #facet_wrap(facets = "categoria", scales = "free") +
  labs(title = "Evolución del salario para el sector público* y la inflación",
       subtitle = "Categorías <span style='color:#fdae61'>E5</span>,
       <span style='color:black'>**D5**</span>,
       <span style='color:#abdda4'>**C5**</span>,
       <span style='color:#2b83ba'>**B5**</span> e 
       <span style='color:#d7191c'>**inflación**</span>. 
       Junio de 2019 = 100. Proyección a mayo 2021",
       caption = "* Corresponde al escalafón de Planta Transitoria y Contratados, niveles E,D,C,B, grado 5. ",
       x = "Período", y = "Índice") +
  ggsave(filename = "Salida/evolucion.png", width = 20, height = 10, dpi = 300)

