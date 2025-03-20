#------------------------------------------------------------------------------#
# Proyecto:                   Incidencia de delitos con arma de fuego
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

# ---- Método para el caso de 1 UPM en los estratos
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

# ---- Establecer directorios
paste_fig <- function(x){paste0("04_figuras/ppo/", x)}

# 1. Cargar datos --------------------------------------------------------------

## 1.1. Modulo de victimización 2011 - 2022 ------------------------------------

# Fuente: https://www.inegi.org.mx/programas/envipe/

# 2. Funciones -----------------------------------------------------------------

## 2.1. Delitos (2011-2012) ----------------------------------------------------

codificar_delito1 <- function(var = x){
  v_delito <- c(
    "Robo total de vehículo",
    "Robo de accesorios",
    "Robo en casa habitación",
    "Robo en la calle",
    "Otro tipo de robos",
    "Fraude bancario",
    "Fraude al consumidor",
    "Extorsión",
    "Amenazas verbales",
    "Lesiones",
    "Secuestro",
    "Agresiones sexuales",
    "Violación sexual",
    "Otros"
  )
  
  case_when(
    var == "01" ~ v_delito[1],
    var == "02" ~ v_delito[2],
    var == "03" ~ v_delito[3],
    var == "04" ~ v_delito[4],
    var == "05" ~ v_delito[5],
    var == "06" ~ v_delito[6],
    var == "07" ~ v_delito[7],
    var == "08" ~ v_delito[8],
    var == "09" ~ v_delito[9],
    var == "10" ~ v_delito[10],
    var == "11" ~ v_delito[11],
    var == "12" ~ v_delito[12],
    var == "13" ~ v_delito[13],
    var == "14" ~ v_delito[14]
  )
}

## 2.2. Delitos (2013-2022) ----------------------------------------------------

codificar_delito2 <- function(var = x){
  v_delito <- c(
    "Robo total de vehículo",
    "Robo de accesorios",
    "Vandalismo",
    "Robo en casa habitación",
    "Robo en la calle",
    "Otro tipo de robos",
    "Fraude bancario",
    "Fraude al consumidor",
    "Extorsión",
    "Amenazas verbales",
    "Lesiones",
    "Secuestro",
    "Agresiones sexuales",
    "Violación sexual",
    "Otros"
  )
  
  case_when(
    var == "01" ~ v_delito[1],
    var == "02" ~ v_delito[2],
    var == "03" ~ v_delito[3],
    var == "04" ~ v_delito[4],
    var == "05" ~ v_delito[5],
    var == "06" ~ v_delito[6],
    var == "07" ~ v_delito[7],
    var == "08" ~ v_delito[8],
    var == "09" ~ v_delito[9],
    var == "10" ~ v_delito[10],
    var == "11" ~ v_delito[11],
    var == "12" ~ v_delito[12],
    var == "13" ~ v_delito[13],
    var == "14" ~ v_delito[14],
    var == "15" ~ v_delito[15]
  )
}

## 2.3. Razones de no denuncia -------------------------------------------------

codificar_razon <- function(var = x){
  v_razones <- c(
    "Por miedo al agresor",
    "Por miedo a que lo extorsionaran",
    "Delito de poca importancia",
    "Pérdida de tiempo",
    "Trámites largos y difíciles",
    "Desconfianza en la autoridad",
    "No tenían pruebas",
    "Por actitud hostil de la autoridad",
    "Otros"
  )
  
  case_when(
    var == "01" ~ v_razones[1],
    var == "02" ~ v_razones[2],
    var == "03" ~ v_razones[3],
    var == "04" ~ v_razones[4],
    var == "05" ~ v_razones[5],
    var == "06" ~ v_razones[6],
    var == "07" ~ v_razones[7],
    var == "08" ~ v_razones[8],
    var == "09" ~ v_razones[9],
    var == "99" ~ v_razones[9]
  )
}

# 3. Diseño de encuesta --------------------------------------------------------

## 3.1. Factor de expansión delito ---------------------------------------------

# Cambiar nombres 
v_bases <- c("df_raw_1", "df_raw_2", "df_raw_3", "df_raw_4")

for (i in v_bases) {
  df_temp <- get(i)
  df_name <- df_temp %>% 
    rename(
      UPM_DIS = UPM,
      EST_DIS = EST
    )
  assign(as.character(i), df_name)
}

# Aplicar el diseño de encuesta 
v_bases <-  c("df_raw_1", "df_raw_2", "df_raw_3", "df_raw_4", "df_raw_5", "df_raw_6",
              "df_raw_7", "df_raw_8", "df_raw_9", "df_raw_10", "df_raw_11", "df_raw_12")

for(i in 1:length(v_bases)){
  df_temp <- get(v_bases[i])
  df_encuesta <- df_temp                  %>% 
    mutate(FAC_DEL = as.numeric(FAC_DEL)) %>% 
    as_survey_design(
      ids = UPM_DIS, strata = EST_DIS, weights = FAC_DEL
    )
  assign(paste0("df_encuesta_", i), df_encuesta)
}

# 4. Figuras -------------------------------------------------------------------

## 4.0. Configuración ----------------------------------------------------------

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
v_caption <- "Fuente: Encuesta Nacional de Victimización y Percepción sobre Seguridad Pública (ENVIPE).\nDatos procesados por Intersecta (intersecta.org).\n"

## 4.1. Delitos reportados -----------------------------------------------------

# ---- Obtener proporciones para cada año 

for (i in 1:12) {
  name    <-  paste0("df_encuesta_", i)
  df_temp <- get(name) 
  
  df_data <- df_temp                   %>% 
    group_by(BPCOD)                    %>% 
    srvyr::summarise(
      total = survey_total(vartype = c("cv"))
    )
  assign(paste0("df_data_delito_", i), df_data)
}

## 4.2. Delitos cometidos con arma de fuego (BP1_15) ---------------------------

# Vector de años
v_años <- c(seq(2010, 2021))

# Vector de bases de datos
v_df <- c("df_encuesta_1", "df_encuesta_2", "df_encuesta_3", "df_encuesta_4", "df_encuesta_5", 
          "df_encuesta_6", "df_encuesta_7", "df_encuesta_8", "df_encuesta_9", "df_encuesta_10",
          "df_encuesta_11", "df_encuesta_12")

# ---- Datos para la figura 
df_data_figura <- df_encuesta_1  %>% 
  group_by(BP1_16_1)             %>% 
  summarise(
    total = survey_total(),
    year  = 2010
  )                           

for (i in 2:length(v_años)) {
  df_data_loop <- get(v_df[i])
  
  df_data_figura_loop <- df_data_loop  %>% 
    group_by(BP1_16_1)                 %>% 
    summarise(
      total = survey_total(),
      year  = v_años[i]
    ) 
  
  df_data_figura <- df_data_figura %>%  full_join(df_data_figura_loop)
  
}

# ---- Figura 

# Labels
v_title <- "Incidencia de delitos cometidos con arma de fuego"
v_subtitle <- "Por año\n"

# Figura
ggplot(df_data_figura  %>% filter(BP1_16_1 == 1),
       aes(x = year, y = total))  +
  geom_line(color = v_color) +
  geom_point(color = v_color) +
  # geom_label(aes(label = scales::comma(total)),
  #            size = 1.2,
  #            position = position_jitter(width = 0, height = 0.8)) +
  labs(
    title = v_title,
    subtitle = v_subtitle,
    x = "\nAño",
    y = "Número de delitos cometidos\n",
    caption = v_caption
  ) +
  scale_x_continuous(breaks = seq(2010, 2021)) +
  scale_y_continuous(labels=scales::comma
                     #limits = c(0, 6300000)
  ) +
  tema

# Guardar figura
ggsave(file = paste_fig("01_arma_fuego_text.png"), 
       type = "cairo", device = "png", 
       width = 6, height = 4)

## 4.2b. Delitos cometidos con arma de fuego (BP1_15) --------------------------

# Vector de años
v_años <- c(seq(2010, 2021))

# Vector de bases de datos
v_df <- c("df_encuesta_1", "df_encuesta_2", "df_encuesta_3", "df_encuesta_4", "df_encuesta_5", 
          "df_encuesta_6", "df_encuesta_7", "df_encuesta_8", "df_encuesta_9", "df_encuesta_10",
          "df_encuesta_11", "df_encuesta_12")

# ---- Datos para la figura 
df_data_figura_2011 <- df_encuesta_1         %>% 
  mutate(delito = codificar_delito1(BPCOD))  %>% 
  group_by(delito, BP1_16_1)                 %>% 
  summarise(
    total = survey_total(),
    year  = 2010
  )                           

df_data_figura_2012 <- df_encuesta_2         %>% 
  mutate(delito = codificar_delito1(BPCOD))  %>% 
  group_by(delito, BP1_16_1)                 %>% 
  summarise(
    total = survey_total(),
    year  = 2011
  )  

df_data_figura <- df_data_figura_2011 %>%  full_join(df_data_figura_2012)

for (i in 3:length(v_años)) {
  df_data_loop <- get(v_df[i])
  
  df_data_figura_loop <- df_data_loop          %>% 
    mutate(delito = codificar_delito2(BPCOD))  %>% 
    group_by(delito, BP1_16_1)                 %>% 
    summarise(
      total = survey_total(),
      year  = v_años[i]
    ) 
  
  df_data_figura <- df_data_figura %>%  full_join(df_data_figura_loop)
  
}

# ---- Figura 

# Labels
v_title <- "Incidencia de delitos cometidos con arma de fuego"
v_subtitle <- "Por delito y por año\n"

# Figura
ggplot(df_data_figura  %>% filter(BP1_16_1 == 1,
                                  !delito %in% c("Fraude al consumidor",
                                                 "Fraude bancario")),
       aes(x = year, y = total))  +
  geom_line(color = v_color) +
  geom_point(color = v_color) +
  # geom_label(aes(label = scales::comma(total)),
  #            size = 1.2,
  #            position = position_jitter(width = 0, height = 0.8)) +
  labs(
    title = v_title,
    subtitle = v_subtitle,
    x = "\nAño",
    y = "Número de delitos cometidos\n",
    caption = v_caption
  ) +
  scale_x_continuous(breaks = seq(2010, 2021)) +
  scale_y_continuous(labels=scales::comma
                     #limits = c(0, 6300000)
  ) +
  facet_wrap(~delito,
             scales = "free_y") +
  tema +
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    plot.title            = element_text(size = 13),
    plot.subtitle         = element_text(size = 13)
  )

# Guardar figura
ggsave(file = paste_fig("01_arma_fuego_delito_text.png"), 
       type = "cairo", device = "png", 
       width = 10, height = 6)

## 4.3. Incidencia de secuestros  ----------------------------------------------

# Vector de años
v_años <- c(seq(2010, 2021))

# Vector de bases de datos
v_df <- c("df_data_delito_1", "df_data_delito_2", "df_data_delito_3", "df_data_delito_4", "df_data_delito_5", 
          "df_data_delito_6", "df_data_delito_7", "df_data_delito_8", "df_data_delito_9", "df_data_delito_10",
          "df_data_delito_11", "df_data_delito_12")

# Vector con código correspondiente a secuestro
v_delito <- c("11", "11", "12", "12", "12", "12", "12", "12", "12", "12", "12", "12")

# ---- Datos
df_data_figura <- df_data_delito_1 %>% 
  filter(BPCOD == "11")            %>% 
  mutate(year = 2010)

for (i in 2:length(v_años)) {
  df_loop <- get(v_df[i])
  
  df_figura_loop <- df_loop  %>% 
    filter(BPCOD == v_delito[i])    %>% 
    mutate(year = v_años[i])
  
  df_data_figura <- df_data_figura %>% full_join(df_figura_loop)
}

# ---- Figura 

# Labels
v_title <- "Incidencia de secuestros"
v_subtitle <- "Por año\n"

# Figura
ggplot(df_data_figura,
       aes(x = year, y = total))  +
  geom_line() +
  geom_point() +
  geom_label(aes(label = scales::comma(total)),
             size = 1.2,
             position = position_jitter(width = 0, height = 0.8)) +
  labs(
    title = v_title,
    subtitle = v_subtitle,
    x = "\nAño",
    y = "Número de delitos cometidos\n",
    caption = v_caption
  ) +
  scale_x_continuous(breaks = seq(2010, 2021)) +
  scale_y_continuous(labels=scales::comma
                     # limits = c(0, 120000)
  ) +
  tema

# Guardar figura
ggsave(file = paste_fig("02_secuestros_text.png"), 
       type = "cairo", device = "png", 
       width = 6, height = 4)

## 4.4. Razones por delito(2022) -----------------------------------------------
# Cruce de las razones por las que las víctimas de secuestro, violación y robo a casa habitación
# por las que no denuncian


df_data <- df_encuesta_12                     %>% 
  mutate(denuncia = case_when(
    ((BP1_20 == 1) | (BP1_21 == 1)) ~ "Sí",
    TRUE ~ "No"
  ))                                          %>% 
  filter(denuncia == "No")                    %>%
  mutate(delito = codificar_delito2(BPCOD),
         razon  = codificar_razon(BP1_23))    %>%
  group_by(delito, razon)                     %>% 
  summarise(
    porcentaje = survey_prop()
  )                                           %>% 
  filter(delito %in% c("Violación sexual", 
                       "Secuestro para exigir dinero",
                       "Robo en casa habitación"))

ggplot(df_data, 
       aes(x = porcentaje, y = reorder(razon, porcentaje), group = delito)) +
  geom_col(fill = v_color)  +
  geom_label(aes(label = paste0(round(porcentaje*100, digits = 1), "%")),
             size = 2) +
  tema +
  facet_wrap(~delito,
             labeller = label_wrap_gen(20)) +
  labs(
    title = "¿Por qué las personas no denunciaron?",
    subtitle = "Por delito\n",
    x = v_empty,
    y = v_empty,
    caption = v_caption
  ) +
  scale_x_continuous(labels=scales::percent_format(),
                     limits = c(-.05, .6)) +
  scale_y_discrete(labels = scales::wrap_format(25))  +
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8)
  )

# Guardar plot
ggsave(file = paste_fig("07_razones_denuncia_delito.png"), 
       type = "cairo", device = "png", 
       width = 8, height = 6)

# FIN --------------------------------------------------------------------------

