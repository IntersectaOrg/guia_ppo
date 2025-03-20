#------------------------------------------------------------------------------#
# Proyecto:                   Incidencia de homicidios
# Objetivo:                   Figuras de PPO (2010-2021)
#
# Encargadas:                 Fernanda Torres
# Correo:                     ftorres@intersecta.org
# 
# Fecha de creación:          26 de octubre de 2022
# Última actualización:       23 de noviembre de 2022
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# ---- Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# ---- Cargar librerías 
require(pacman)
p_load(
  foreign, readxl, googledrive, googlesheets4, tidyverse, dplyr, lubridate, 
  srvyr, zoo, beepr, gdata, readxl)

# ---- Limpiar espacio de trabajo 
rm(list=ls())

# ---- Desactivar notación científica
options(scipen=999)

# ---- Establecer directorios
paste_fig <- function(x){paste0("04_figuras/ppo/", x)}

# 1. Cargar datos --------------------------------------------------------------

## 1.1. Homicidios INEGI -------------------------------------------------------

# Defunciones por homicidio por año de registro y sexo
# Fuente: https://www.inegi.org.mx/sistemas/olap/consulta/general_ver4/MDXQueryDatos.asp?proy=

df_raw <- read_excel("~/Downloads/INEGI_exporta_23_11_2022_11_30_37.xlsx",
                            skip = 3)

# 2. Procesamiento -------------------------------------------------------------

df_homicidios <- df.raw                           %>%
  # Renombrar columna de año
  rename(Año = ...1)                              %>% 
  # Eliminar rows sin información
  slice(-c(33:36))                                %>% 
  # Eliminar comas
  mutate(across(everything(),~ gsub(",","", .)))  %>% 
  # Convertir a numérico
  mutate_at(c(1:5), as.numeric)
  
## 2.0. Configuración ----------------------------------------------------------

# ---- Tema
tema        <-  theme_linedraw() +
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot", 
    text                  = element_text(family = "Fira Sans", color = "black"),
    plot.title            = element_text(family = "Fira Sans Medium", color = "#4d4c7d", size = 10, margin = margin(10,5,5,5)),
    plot.subtitle         = element_text(family = "Fira Sans", color = "#4d4c7d", size = 10, face = "italic", margin = margin(5, 5, 5, 5)),
    plot.caption          = element_text(family = "Fira Sans", color = "#7473a5", size = 8, face = "italic", hjust = 0),
    panel.grid            = element_line(linetype = 2, color = "#b3c2c9"),
    plot.margin           = margin(0, .5, 0, .5, "cm"),
    legend.position       = "top",
    panel.border          = element_blank(),
    legend.title          = element_text(size = 10, family = "Fira Sans Medium", color = "#4d4c7d"),
    legend.text           = element_text(size = 10, family = "Fira Sans", color = "#4d4c7d"),
    axis.title            = element_text(size = 10, family = "Fira Sans", color = "#7473a5", hjust = .5, margin = margin(1,1,1,1)),
    axis.text.y           = element_text(size = 10, family = "Fira Sans", angle=0,  color = "#7473a5", hjust=1),
    axis.text.x           = element_text(size = 10, family = "Fira Sans", angle=90, color = "#7473a5", hjust=.5, vjust = 0.5),
    strip.text.x          = element_text(size = 10, family = "Fira Sans", face = "bold", color = "#7473a5"),
    strip.text.y          = element_text(size = 10, family = "Fira Sans", face = "bold", color = "#7473a5"), 
    strip.background      = element_rect(fill = "white", color = NA))

# ---- Colores
v_color <- "#e23e57"

# ---- Vectores de texto 
v_empty <- ""
v_caption_h <- "Fuente: Registros administrativos de mortalidad publicados por el INEGI.\nDatos procesados por Intersecta (intersecta.org).\n"

## 4.6. Homicidios (general) ---------------------------------------------------

# ---- Figura 

# Labels
v_title <- "Homicidios"
v_subtitle <- "Por año\n"

# Figura
ggplot(df_homicidios %>% filter(Año %in% c(seq(2010, 2021))),
       aes(x = as.numeric(Año), y = as.numeric(Total)))  +
  geom_line() +
  geom_point() +
  geom_label(aes(label = scales::comma(Total)),
             size = 1.2,
             face = "bold",
             position = position_jitter(width = 0, height = 0.8)) +
  labs(
    title = v_title,
    subtitle = v_subtitle,
    x = "\nAño",
    y = "Número de homicidios cometidos\n",
    caption = v_caption_h
  ) +
  scale_x_continuous(breaks = seq(2010, 2021)) +
  scale_y_continuous(labels=scales::comma
                     # limits = c(0, 37000)
  ) +
  tema

# Guardar figura
ggsave(file = paste_fig("05_homicidios_text.png"), 
       type = "cairo", device = "png", 
       width = 6, height = 4)

## 4.7. Homicidios por sexo ----------------------------------------------------

df_data <- df_homicidios %>% 
  mutate(Año = as.numeric(Año),
         Hombres = as.numeric(Hombre), 
         Mujeres = as.numeric(Mujer),
         `No especificado` = as.numeric(`No especificado`)) %>% 
  select(Año, Hombres, Mujeres, `No especificado`) %>% 
  pivot_longer(cols = c(2:4), names_to = "Sexo", values_to = "Homicidios") %>% 
  filter(Año >= 2010)

# ---- Figura 

# Labels
v_title <- "Homicidios"
v_subtitle <- "Por año y por sexo\n"

# Figura
ggplot(df_data,
       aes(x = Año, y = Homicidios))  +
  geom_line() +
  geom_point() +
  labs(
    title = v_title,
    subtitle = v_subtitle,
    x = "\nAño",
    y = "Número de homicidios cometidos\n",
    caption = v_caption_h
  ) +
  scale_x_continuous(breaks = seq(2010, 2021)) +
  scale_y_continuous(labels=scales::comma,
                     limits = c(0, NA)) +
  tema +
  facet_wrap(~Sexo,
             scales = "free_y")

# Guardar figura
ggsave(file = paste_fig("06_homicidios_sexo2.png"), 
       type = "cairo", device = "png", 
       width = 6, height = 4)

# FIN --------------------------------------------------------------------------
