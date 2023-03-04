library(pdftools)
library(readr)

#' Basic filtering of data
#'
#' @param dataset 
#'
#' @return
#' @export
#'
#' @examples
uncu_2_df <- function(rawdata)
{
  # Para separar un pdf en páginas:
  # pdftk file.pdf burst 
  # pdftk ingresantes_2023.pdf burst output pg_%02d.pdf
  
  # write.csv( rawdata, file="./text.txt",
  #            col.names = FALSE,
  #            row.names = FALSE )
  
  fileConn<-file("text.txt")
  writeLines(rawdata, fileConn)
  close(fileConn)
  
  data <- readr::read_fwf(
    file = "./text.txt",
    col_positions = fwf_widths( c(16, 50, 35, 15),
                                col_names = c("Preinscripcion",
                                              "Escuela",
                                              "Orientacion",
                                              "Promedio")
    ),
    col_types = c("i", "c", "c", "d") ,
    skip = 3,
  )
  
  data <- data %>% filter (!is.na(Preinscripcion))
  
  data <- data %>%
    # mutate_at("Promedio", str_replace, "," , ".") %>%
    mutate_at("Escuela", str_replace, 
              "Colegio Universitario Central José de San Martin" , "CUC") %>%
    mutate_at("Escuela", str_replace, 
              "Departamento de Aplicación Docente" , "DAD") %>%
    mutate_at("Escuela", str_replace, 
              "Escuela de Comercio Martín Zapata" , "MZ") %>%
    mutate_at("Escuela", str_replace, 
              "Escuela del Magisterio" , "MAG") %>%
    mutate_at("Orientacion", str_replace, 
              "Ciencias Sociales y Humanidades" , "Ciencias Sociales") %>%
    mutate_at("Orientacion", str_replace, 
              "Arte Multimedia" , "Arte") %>%
    mutate_at("Orientacion", str_replace, 
              "Economía y Administración" , "Economía") %>%
    arrange( match(Escuela, c("CUC", "MZ", "MAG", "DAD") ) )
  
  options(digits = 6)
  data <- data %>% mutate ( Promedio = as.double(Promedio) / 1000)
  
  system("rm ./text.txt")
  
  return(data)
}

#' Basic filtering of data
#'
#' @param dataset 
#'
#' @return
#' @export
#'
#' @examples
lae_2_df <- function(text)
{
  # pdftk lae_ingresantes_2023.pdf burst output lae_%02d.pdf

  fileConn<-file("text.txt")
  writeLines(rawdata, fileConn)
  close(fileConn)
  
  data <- readr::read_fwf(
    file = "./text.txt",
    col_positions = fwf_widths( c(10, 27, 15) ,
                                col_names = c("Orden",
                                              "Preinscripcion",
                                              "Promedio") 
    ),
    col_types = c("i", "i", "d") ,
    skip = 3,
  )
  
  data <- data %>% filter (!is.na(Preinscripcion))
  
  data <- data %>%
    # mutate_at("Promedio", str_replace, "," , ".") %>%
    add_column(Escuela = "LAE", .before = "Promedio") %>%
    add_column(Orientacion = "Tecnología Alimentaria", .before = "Promedio") %>%
    select(Escuela, Orientacion, Promedio)
    
  options(digits = 6)
  data <- data %>% mutate ( Promedio = as.double(Promedio) / 1000)
  
  system("rm ./text.txt")
  
  return(data)
}

#' Basic filtering of data
#'
#' @param dataset 
#'
#' @return
#' @export
#'
#' @examples
plot_alu_e <- function(data, text_cap)
{
  data %>%
    
    ggplot( aes(
      axis1 = Etiqueta,
      axis2 = Orientacion, ),
    ) + #
    # geom_alluvium( aes(fill = Rangos, color = Rangos ),
    # width = 1/12 ,
    # alpha = 0.35 ,
    # reverse = FALSE,
    # ) +
    geom_flow(aes(fill = Rangos, color = Rangos ),
              curve_type = "sigmoid",
              width = 1/10,
              alpha = 0.75,
              reverse = FALSE,) + 
    geom_stratum(width = 1/6 , fill = "white",
                 color = "black",
                 reverse = FALSE,
                 # position = 1,
    ) +
    # geom_label(stat = "stratum",
    #            aes(label = after_stat(stratum)),
    #            reverse = FALSE,
    #            size = 3,
    #            color = "black",
    #            ) +
    scale_x_discrete(limits = c("Rangos", "Orientacion"),
                     expand = c(0.2 , 0.2) ) +
    scale_color_viridis(discrete = TRUE, option = "D") +
    scale_fill_viridis( discrete = TRUE, option = "D") +
    #=========================================================
  ggrepel::geom_text_repel(
    aes(label = ifelse(after_stat(x) == 1, as.character(after_stat(stratum)), NA)),
    stat = "stratum", size = 3.65, direction = "y", nudge_x = 0, reverse = FALSE,
  ) + ggrepel::geom_text_repel(
    aes(label = ifelse(after_stat(x) == 2, as.character(after_stat(stratum)), NA)),
    stat = "stratum", size = 3.65, direction = "y", nudge_x = +.35, reverse = FALSE,
  ) +
    #=========================================================
  theme_bw() +
    theme(text = element_text(size = 13, family = 'Arial')) +
    theme(plot.title = element_text(size = 13, family = 'Arial')) +
    ggtitle(text_cap)
  
}