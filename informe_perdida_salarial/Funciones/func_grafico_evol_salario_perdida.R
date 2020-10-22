

################################ Gráfico Evolución del índice

graf_evol_salario <- function(base, mes) {

  assertthat::assert_that(is.numeric(mes), msg = 'Por favor, coloque el valor del mes en número')
  
  base %>% 
    filter(indicador != "perdida") %>%
    mutate(etiqueta = case_when(indicador == "y_d0_evol" ~ etiqueta[3],
                                indicador == "y_privado_evol" ~ etiqueta[2],
                                indicador == "inflacion_nov15" ~ etiqueta[1])) %>% 
    ggplot(aes(x = periodo)) +
    geom_line(aes(y = indice, group = indicador, colour = indicador), 
              size = 2, alpha = 1) + 
    #geom_point(aes(y = indice), color = "grey") +
    labs(title    = "Evolución de la inflación, el salario del sector público* y privado registrado",
         subtitle =  "Noviembre de 2015 = 100",
         caption  = "* Corresponde al escalafón de Planta Transitoria y Contratados, Nivel D, grado 0, con adicional por título secundario. \n
       Fuente: SINEP, OEDE-MTEySS e IPCBA hasta mayo de 2016, IPC-GBA hasta diciembre de 2016, IPC-Cobertura Nacional desde enero de 2017. ") +
    #scale_x_discrete(expand = c(0, 0)) +
    scale_x_date(date_labels="%b %Y ", 
                 expand = c(0.15, 0), limits = dmy(c("1/11/2015", glue("1/{mes}/2020"))),
                 breaks = dmy(c("1/11/2015", glue("1/{mes}/2016"), glue("1/{mes}/2017"), 
                                glue("1/{mes}/2018"), glue("1/{mes}/2019"), glue("1/{mes}/2020")))) + 
    #scale_y_continuous(sec.axis = sec_axis(~ ., breaks = d_ends_indice, labels = etiqueta)) +
    geom_dl(aes(y = indice, label = etiqueta, color = indicador), 
            method = list("last.points", cex = 1.8), 
            position = "identity") +
    cowplot::theme_minimal_vgrid() +
    colorblindr::scale_color_OkabeIto() +  
    theme(legend.position = "none",
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
          text = element_text(size = 25),
          axis.text = element_text(size = 20),
          plot.caption = element_text(size = 13, vjust = -.1)) +
    ggsave(filename = "Salida/Evolución infla_D0_priv.png", width = 20, height = 10, dpi = 300)
}

# graf_evol_salario(base = base_trab2, mes = 7)


################################ # Gráfico pérdida salarial

graf_evol_perdida <- function(base, mes) {
  
  assertthat::assert_that(is.numeric(mes), msg = 'Por favor, coloque el valor del mes en número')
  
  base %>% 
    filter(indicador == "perdida") %>% 
    ggplot(aes(x = periodo)) +
    geom_line(aes(y = indice, group = indicador), colour = "red", 
              size = 2, alpha = 0.9) + 
    labs(title = "Evolución del salario real para el sector público*",
         subtitle = "Noviembre - 2015 = 100.",
         caption = "* Corresponde al escalafón de Planta Transitoria y Contratados, Nivel D, grado 0, con adicional por título secundario.") +
    scale_x_date(date_labels="%b %Y ", 
                 expand = c(0.2, 0), limits = dmy(c("1/11/2015", glue("1/{mes}/2020"))),
                 breaks = dmy(c("1/11/2015", glue("1/{mes}/2016"), glue("1/{mes}/2017"), 
                                glue("1/{mes}/2018"), glue("1/{mes}/2019"), glue("1/{mes}/2020")))) + 
    # scale_y_continuous(sec.axis = sec_axis(~ ., breaks = d_ends_perdida, labels = d_ends_perdida)) +
    geom_dl(aes(y = indice, label=round(last(base_orig$perdida),1)), colour= "red", method = list("last.qp", 
                                                                                                  cex = 2, hjust = -.2),
            position = "identity") +
    cowplot::theme_minimal_vgrid() +
    theme(legend.position = "none",
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
          text = element_text(size = 25),
          axis.text = element_text(size = 20),
          plot.caption = element_text(size = 13, vjust = -.1)) +
    ggsave(filename = "Salida/perdida D0.png", width = 20, height = 10, dpi = 300)
}

  
# graf_evol_perdida(base = base_trab2, mes = 7)