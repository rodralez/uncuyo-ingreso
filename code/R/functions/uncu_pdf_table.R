library(pdftools)
library(readr)

rm(list = ls()) # clean global environment

# FUNCTIONS

# Para separar un pdf en páginas:
# pdftk file.pdf burst output name-%d.pdf

uncu_2_df <- function(text)
{
  write.csv( text, file="./text.txt",
             col.names = FALSE,
             row.names = FALSE )
  data <- readr::read_fwf(
    file = "./text.txt",
    # col_positions = fwf_widths( c(21, 50, 34, 14, 13) ,
    col_positions = fwf_widths( c(21, 50, 36, 22, 15) ,
                                col_names = c("Preinscripcion",
                                              "Escuela",
                                              "Orientacion",
                                              "Promedio",
                                              "Aleatorio") 
                    ),
    col_types = c("i", "c", "c", "d", "i") ,
    skip = 5,
  )
  data <- data %>% filter (!is.na(Preinscripcion))
  
  data <- data %>%
    mutate_at("Promedio", str_replace, "," , ".") %>%
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
  data <- data %>% mutate ( Promedio = as.double(Promedio))
  
  system("rm ./text.txt")
  
  return(data)
}

lae_2_df <- function(text)
{
  write.csv( text, file="./text.txt",
             col.names = FALSE,
             row.names = FALSE )
  data <- readr::read_fwf(
    file = "./text.txt",
    col_positions = fwf_widths( c(28, 20, 8) ,
                                col_names = c("Orden",
                                              "Preinscripcion",
                                              "Promedio") 
    ),
    col_types = c("i", "i", "d") ,
    skip = 3,
  )
  data <- data %>% filter (!is.na(Preinscripcion))
  
  data <- data %>%
    mutate_at("Promedio", str_replace, "," , ".") %>%
    add_column(Escuela = "LAE", .before = "Promedio") %>%
    add_column(Orientacion = "Tecnología Alimentaria", .before = "Promedio") %>%
    select(Escuela, Orientacion, Promedio)
    
  options(digits = 6)
  data <- data %>% mutate ( Promedio = as.double(Promedio))
  
  system("rm ./text.txt")
  
  return(data)
}

# SCRIPT

setwd("/home/rodralez/hostdir/uncu-ingreso/")

# Datos de los colegios UNCU con orientaciones

n_pag <- 18   # Numeros de paginas en el pdf 

df <- vector(mode = "list", length = n_pag)

for (idx in 1:n_pag) 
{
    pdf_file <- sprintf("./pg_00%02d.pdf", idx)
    
    text <- pdf_text(pdf_file)
    
    data <- uncu_2_df(text)
    
    df[[idx]] <- data
}

# Hacer un solo df
for (idx in 1:n_pag) 
{
  if(idx == 1)
  {
    data_uncu <- df[[idx]]
  }  
  else
  {
    data_uncu <-rbind(data_uncu, df[[idx]]) 
  }
}

# Datos del LAE

n_pag <- 2   # Numeros de paginas en el pdf 
df <- vector(mode = "list", length = n_pag)

for (idx in 1:n_pag) 
{
  pdf_file <- sprintf("./lae-%d.pdf", idx)
  
  text <- pdf_text(pdf_file)
  
  data <- lae_2_df(text)
  
  df[[idx]] <- data
}

# Hacer un solo df
for (idx in 1:n_pag) 
{
  if(idx == 1)
  {
    data_lae <- df[[idx]]
  }  
  else
  {
    data_lae <-rbind(data_lae, df[[idx]]) 
  }
}

# Analisis de los datos
library(tidyr)

data_uncu_sm <- data_uncu %>% select(Escuela, Orientacion, Promedio)

data_sm <- rbind(data_uncu_sm, data_lae)
  
# Ingresantes por escuela  

ingresantes <- nrow (data_sm)

# CUC, DAT, LAE, MG, MZ
vacantes_e <- c( 60*3, 120*3, 90, 30+90+30, 120+60+60)

# CVA es Carmen Vera Arena
 
data_e <- data_sm %>% group_by(Escuela) %>% 
                      summarise( Ingresantes = n() ) %>% 
                      add_column(Vacantes = vacantes_e, .after = "Escuela") %>% 
                      mutate(CVA = Vacantes - Ingresantes)

# Ingresantes por orientacion

# CUC, DAT, LAE, MG, MZ
vacantes_o <- c( 60, 60, 60, 
              120, 120, 120, 
              90,
              30, 90, 30,
              60, 120, 60)

data_o <- data_sm %>% group_by(Escuela, Orientacion) %>% 
                      summarise(Ingresantes = n() ) %>% 
                      add_column(Vacantes = vacantes_o, .after = "Orientacion") %>% 
                      mutate(CVA = Vacantes - Ingresantes)

# Total de ingresantes vs alumnos del CVA
data_x <- data_o %>% ungroup() %>% summarise( total_i = sum(Ingresantes) , total_cva = sum(CVA))

# Estadisticas, Mejores promedios por orientacion
data_s <- data_sm %>% group_by(Escuela, Orientacion) %>% 
                      summarise( max = max(Promedio), min = min(Promedio),
                                 mean = mean(Promedio), median = median(Promedio), 
                                 sd = sd(Promedio) )

# ¿A dónde van todos los 10?

data_d <- data_sm %>% group_by(Escuela, Orientacion) %>% 
                      filter (Promedio == 10.000) %>% 
                      summarise( Dieces = n() )  %>% 
                      add_column(Ingresantes = data_o$Ingresantes, .after = "Dieces") %>% 
                      mutate(Porcentaje = Dieces/Ingresantes*100)
# Visual

library(ggplot2)
library(dplyr)
library(hrbrthemes)

## PLOT 1
data_sm  %>% ungroup() %>% # arrange("CUC", "MZ", "MAG", "DAD") %>%
ggplot(.) + 
  geom_jitter(mapping = aes(y = Escuela, #x = Promedio, 
                            x= Promedio, # y = Escuela, 
                            color = Orientacion,
                           ),
             size = 3,
             alpha = 0.75,
             
             ) +
  geom_point(mapping = aes(y = Escuela,
                           x= Promedio), 
                           stat = "summary", 
             fun = "median", colour = "red", size = 4) +
labs(title="Distribución de los promedios", 
     subtitle="Año 2022", 
     y="Escuela", 
     x="Promedios",
     # caption="Midwest Demographics"
     ) + 
  scale_colour_brewer(palette = "Spectral") + # change color palette  
  theme_bw() +
  scale_x_continuous(breaks = round(seq(min(data_uncu$Promedio),
  max(data_uncu$Promedio), by = 0.01), 1) ) 
  # scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

## PLOT 2
data_sm  %>% select(Promedio) %>% #filter(Escuela == "CUC")  %>%
  ggplot(aes(Promedio)) + # , color = Escuela, fill = Escuela
  # geom_histogram(aes(y = after_stat(density), binwidth = 5))
  geom_histogram( aes(x=Promedio),  binwidth = 0.02, 
                  color="white", fill = "#1380A1") 
# +
#   geom_density(alpha = 0.1, fill = "red")  
# geom_bar()

## PLOT 3 ALLUVIAL

library(ggalluvial)
library("viridis")  
library(ggforce)

is_alluvia_form(as.data.frame(data_sm), axes = 1:3, silent = TRUE)

data_alu <- data_sm  %>% ungroup()  %>% 
  mutate(freq = row_number(), .before = "Escuela") %>% 
  group_by(Rangos      = cut(Promedio, breaks = c(9.70, 9.85,  9.90,  9.95,  9.99, 10.00) ) ) %>% 
  group_by(Rango_label = cut(Promedio, breaks = c(9.70, 9.85,  9.90,  9.95,  9.99, 10.00) ) ) %>% 
  mutate(. , Rango_label = fct_recode(Rango_label , "R1" = "(9.99,10]" )) %>%
  mutate(. , Rango_label = fct_recode(Rango_label , "R2" = "(9.95,9.99]" )) %>%
  mutate(. , Rango_label = fct_recode(Rango_label , "R3" = "(9.9,9.95]" )) %>%
  mutate(. , Rango_label = fct_recode(Rango_label , "R4" = "(9.85,9.9]" )) %>%
  mutate(. , Rango_label = fct_recode(Rango_label , "R5" = "(9.7,9.85]" )) %>%
  group_by(Orientacion, Escuela, Promedio)
  # %>%
#   mutate(. , Rango_label = fct_recode(Rango_label , "R5" = "(9.75,9.8]" ))
  # mutate(Rango_label = fct_reorder(Rango_label, Rangos, .fun='length')) %>%
  # arrange( match(Escuela, c("CUC", "MZ", "MAG", "DAD") ) ) 

g <- data_alu  %>%  
  ggplot( aes(
              axis1 = Rango_label, 
              axis2 = Escuela,
              axis3 = Orientacion
              ) ) + # 
  # geom_alluvium( aes(fill = Rangos, color = Rangos ),
                 # width = 1/12 ,
                 # alpha = 0.35 ,
                 # reverse = FALSE,
                 # )  +
  geom_flow(aes(fill = Rangos, color = Rangos ), 
            curve_type = "sigmoid",
            width = 1/12,
            alpha = 0.85,
            reverse = FALSE,) +   
  geom_stratum(width = 1/12 , fill = "white",
               color = "black",
               reverse = FALSE,
               # position = 1,
               )  +
  geom_label(stat = "stratum",
             aes(label = after_stat(stratum)),
             reverse = FALSE,
             size = 2.5,
             color = "black",
             ) +
  scale_x_discrete(limits = c("Rangos", "Escuela", "Orientacion"),
                   expand = c(.05 , .05)) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis( discrete = TRUE, option = "D") + 
  theme_bw() +
  ggtitle("Ingresantes a los colegios de la UNCuyo en 2022", 
          "Distribución de promedios por rangos") 
# + 
#   # annotate("text", x = 0.0, y = 0.0, label = "DRAFT",
#   #           col="red", fontface = "bold", angle=0, alpha = 0.95)
#   annotate("text", x = Inf, y = -Inf, label = "DRAFT by rodralez",
#            hjust=0.6, vjust=-9, col="gray", cex=20,
#            fontface = "bold", angle=45, alpha = 0.5) 
# SAVE
aspect_ratio <- 2
ggsave("./uncu_alu.png", g, height = 8 , width = 7 * aspect_ratio)

