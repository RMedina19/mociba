#------------------------------------------------------------------------------#
# Proyecto:                   ENPOL 2016
# Objetivo:                   Características generales de la población
#
# Encargadas:                 Fernanda Torres (CIDE)
# Correo:                     ftorres@intersecta.org
# Fecha de creación:          09 de mayo de 2022
# Última actualización:       09 de mayo de 2022
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# Silenciar warnings 
options(warn=-1)

# Cargar librerías 
require(pacman)
p_load(foreign, srvyr, tidyverse, dplyr, lubridate, scales)

# Limpiar espacio de trabajo 
rm(list=ls())

# Funciones para llamar archivos de las carpetas del repositorio
paste_code  <- function(x){paste0("01_códigos/"      , x)}
paste_inp   <- function(x){paste0("02_datos_crudos/" , x)}
paste_data  <- function(x){paste0("03_datos_limpios/", x)}
paste_figs  <- function(x){paste0("04_figuras/"      , x)}

# 1. Cargar datos --------------------------------------------------------------

# MOCIBA 2021
df_raw21 <- read.dbf(paste_inp("mod_2021_ciberacoso.dbf"))

v_caption <- "Fuente: Elaboración propia con datos del Módulo sobre Ciberacoso (MOCIBA) 2021 del INEGI."

# 2. Preprocesamiento datos ----------------------------------------------------

# Codificar tipos de agresiones 
df_mociba21 <- df_raw21 %>% 
  janitor::clean_names() %>% 
  # Cambiar tipo de dato (primero a caracter porque son factores)
  mutate_all(~as.numeric(as.character(.))) %>% 
  # Convertir en variables binarias 
  mutate_at(
    .vars = c("p4_01", "p4_02", "p4_03", "p4_04", "p4_05", "p4_06", "p4_07", 
              "p4_08", "p4_09", "p4_10", "p4_11", "p4_12", "p4_13"), 
    ~if_else(.==1, 1, 0)) %>% 
  # Estimar número de agresiones y si las personsa sufrieron ciberacoso 
  mutate(
    num_agresiones = p4_01 + p4_02 + p4_03 + p4_04 + p4_05 + p4_06 + p4_07 + 
                     p4_08 + p4_09 + p4_10 + p4_11 + p4_12 + p4_13, 
    ciberacoso = if_else(num_agresiones == 0, 0, 1)
  ) %>% 
  # Codificar valores 
  mutate(
    ent = case_when(
     ent == 1	~ "Aguascalientes",
     ent == 2	~ "Baja California",
     ent == 3	~ "Baja California Sur",
     ent == 4	~ "Campeche",
     ent == 5	~ "Coahuila",
     ent == 6	~ "Colima",
     ent == 7	~ "Chiapas",
     ent == 8	~ "Chihuahua",
     ent == 9	~ "Ciudad de México",
     ent == 10	~ "Durango",
     ent == 11	~ "Guanajuato",
     ent == 12	~ "Guerrero",
     ent == 13	~ "Hidalgo",
     ent == 14	~ "Jalisco",
     ent == 15	~ "Estado de México",
     ent == 16	~ "Michoacán",
     ent == 17	~ "Morelos",
     ent == 18	~ "Nayarit",
     ent == 19	~ "Nuevo León",
     ent == 20	~ "Oaxaca",
     ent == 21	~ "Puebla",
     ent == 22	~ "Querétaro",
     ent == 23	~ "Quintana Roo",
     ent == 24	~ "San Luis Potosí",
     ent == 25	~ "Sinaloa",
     ent == 26	~ "Sonora",
     ent == 27	~ "Tabasco",
     ent == 28	~ "Tamaulipas",
     ent == 29	~ "Tlaxcala",
     ent == 30	~ "Veracruz",
     ent == 31	~ "Yucatán",
     ent == 32	~ "Zacatecas")) 

table(df_mociba21$num_agresiones)
table(df_mociba21$ciberacoso)



# 4. Estimaciones --------------------------------------------------------------

# 4.1. Personas que vivieron ciberacoso ----------------------------------------

df_data <- df_mociba21            %>% 
  group_by(ciberacoso)            %>% 
  # mutate(total = ciberacoso * factor) %>% 
  summarise(total = sum(factor))  %>% 
  ungroup()                       %>% 
  mutate(prop = total/sum(total), 
         sexo = 0) %>% 
  bind_rows(df_mociba21            %>% 
               group_by(sexo, ciberacoso)      %>% 
               # mutate(total = ciberacoso * factor) %>% 
               summarise(total = sum(factor))  %>% 
               group_by(sexo)                  %>% 
               mutate(prop = total/sum(total))) %>% 
  mutate(
    sexo = case_when(
      sexo == 0 ~ "Población general", 
      sexo == 1 ~ "Hombres", 
      sexo == 2 ~ "Mujeres"
    )
  )

ggplot(df_data %>% 
         filter(ciberacoso == 1) %>% 
         filter(sexo != "Población general"), 
       aes(x = sexo, y = prop, fill = sexo)) +
  # Geoms
  geom_col() +
  geom_hline(data = df_data %>% 
               filter(ciberacoso == 1) %>% 
               filter(sexo == "Población general"), 
             aes(yintercept = prop), linetype = "dashed") +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)), vjust = -1) +
  # Etiquetas 
  labs( 
    title = "Población mayor de 12 años que sufrió ciberacoso en México\n", 
    subtitle = "Por sexo de la persona agredida", 
    x = "\nSexo", 
    y = "Porcentaje\n", 
    fill = "", 
    shape = "Sexo a nivel nacional", 
    caption = v_caption, 
  ) +
  # Escalas 
  scale_y_continuous(label = scales::percent_format(), limits = c(0, 0.25)) +
  # Tema
  theme_bw() +
  theme(legend.position = "none")


ggsave(file = paste_figs("01_nacional_sexo.png"))

# 4.2. Personas que vivieron ciberacoso ----------------------------------------

# Personas que vivieron ciberacoso por sexo y por entidad 
df_data <- df_mociba21            %>% 
  group_by(ent, ciberacoso) %>% 
  # mutate(total = ciberacoso * factor) %>% 
  summarise(total = sum(factor))  %>% 
  group_by(ent)             %>% 
  mutate(prop = total/sum(total), 
         sexo = 0) %>% 
  bind_rows(df_mociba21            %>% 
              group_by(ent, sexo, ciberacoso)      %>% 
              # mutate(total = ciberacoso * factor) %>% 
              summarise(total = sum(factor))  %>% 
              group_by(ent, sexo)                  %>% 
              mutate(prop = total/sum(total))) %>% 
  mutate(
    sexo = case_when(
      sexo == 0 ~ "Población general", 
      sexo == 1 ~ "Hombres", 
      sexo == 2 ~ "Mujeres"
    )
  )


ggplot(df_data %>% 
         filter(ciberacoso == 1) %>% 
         filter(sexo != "Población general"), 
       aes(x = sexo, y = prop, fill = sexo)) +
  facet_wrap(~ent, ncol = 8, labeller = label_wrap_gen(width = 15)) +
  # Geoms
  geom_col() +
  geom_hline(data = df_data %>% 
               filter(ciberacoso == 1) %>% 
               filter(sexo == "Población general"), 
             aes(yintercept = prop), linetype = "dashed") +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)), vjust = -0.8, size = 2) +
  # Etiquetas 
  labs( 
    title = "Población mayor de 12 años que sufrió ciberacoso\n", 
    subtitle = "Por sexo de la persona agredida y entidad federativa", 
    x = "\nSexo", 
    y = "Porcentaje\n", 
    fill = "", 
    caption = v_caption, 
  ) +
  # Escalas
  scale_y_continuous(limits = c(0, 0.35), labels = scales::percent_format()) +
  # Tema
  theme_bw() +
  theme(legend.position = "none")


ggsave(file = paste_figs("02_entidades_sexo.png"), width = 8, height = 6)


# 4.3. Tipo de agresión --------------------------------------------------------

df_data <- df_mociba21 %>% 
  filter(ciberacoso == 1) %>% 
  select(ent, factor, starts_with("p4_")) %>% 
  pivot_longer(
    cols = starts_with("p4_"), 
    names_to = "tipo", 
    values_to = "respuesta") %>% 
  group_by(tipo, respuesta) %>% 
  summarise(total = sum(factor)) %>% 
  group_by(tipo) %>% 
  mutate(prop = total/sum(total)) %>% 
  mutate(sexo = 0) %>% 
  bind_rows(
    df_mociba21 %>% 
      filter(ciberacoso == 1) %>% 
      select(sexo, ent, factor, starts_with("p4_")) %>% 
      pivot_longer(
        cols = starts_with("p4_"), 
        names_to = "tipo", 
        values_to = "respuesta") %>% 
      group_by(tipo, respuesta, sexo) %>% 
      summarise(total = sum(factor)) %>% 
      group_by(sexo, tipo) %>% 
      mutate(prop = total/sum(total)) 
  ) %>% 
  mutate(
    sexo = case_when(
      sexo == 0 ~ "Población general", 
      sexo == 1 ~ "Hombres", 
      sexo == 2 ~ "Mujeres"
    ), 
    tipo = case_when(
      tipo == "p4_01" ~ "Le mandó mensajes ofensivos o burlas",
      tipo == "p4_02" ~ "Le hizo llamadas ofensivas o burlas",
      tipo == "p4_03" ~ "Criticó de apariencia o clase social",
      tipo == "p4_04" ~ "Se hizo pasar por usted para enviar información falsa o agredir",
      tipo == "p4_05" ~ "Lo(a) contactó con nombres falsos para molestarle",
      tipo == "p4_06" ~ "Vigiló sus sitios o cuentas de internet",
      tipo == "p4_07" ~ "Lo(a) provocó en línea para que reaccionara de forma negativa",
      tipo == "p4_08" ~ "Le hizo insinuaciones o propuestas de tipo sexual que le molestaron",
      tipo == "p4_09" ~ "Le envió fotos o videos de contenido sexual que le molestaron",
      tipo == "p4_10" ~ "Compartió imágenes de contenido íntimo sexual de usted sin su consentimiento",
      tipo == "p4_11" ~ "Publicó información personal (no sexual) de usted para dañarle",
      tipo == "p4_12" ~ "Amenazón con publicar información a cambio de que usted hiciera algo",
      tipo == "p4_13" ~ "Otra situación",
    )
  )

df_sexo <- df_data %>% filter(respuesta == 1, sexo != "Población general")

ggplot(df_sexo, 
       aes(x = prop, y = reorder(tipo, prop)), fill = sexo) +
  # facet_wrap(~sexo) +
  # Geoms
  geom_col(aes(fill = sexo), position = "dodge") +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)), 
            position = position_dodge(0.9), vjust = if_else(df_sexo$sexo == "Hombres", 0.9, -0.5),
            hjust = -0.1,
            color = "black", size = 3.5) +
  geom_point(
    data = df_data %>% filter(respuesta == 1, sexo == "Población general"),
             aes(shape = sexo)
) +
  # Etiquetas 
  labs( 
    title = "Tipos de ciberacoso más frecuentes", 
    subtitle = "Por género de la persona agredida", 
    x = "Porcentaje del total de personas que reportaron ciberacoso", 
    y = "Alguien:", 
    shape = "", 
    fill = "Sexo",
    caption = v_caption, 
    ) +
  # Escalas 
  scale_y_discrete(label = scales::wrap_format(25)) +
  scale_x_continuous(limits = c(0, 0.45), labels = scales::percent_format()) +
  # Tema 
  theme_bw() +
  theme(legend.position = "top")

ggsave(file = paste_figs("03_nacional_sexo_tipo.png"), width = 6, height = 8)


# 4.2. Personas que vivieron ciberacoso ----------------------------------------

df_ags <- df_mociba21 %>% 
  filter(ciberacoso == 1, ent == "Aguascalientes") %>% 
  select(ent, factor, starts_with("p4_")) %>% 
  pivot_longer(
    cols = starts_with("p4_"), 
    names_to = "tipo", 
    values_to = "respuesta") %>% 
  group_by(tipo, respuesta) %>% 
  summarise(total = sum(factor)) %>% 
  group_by(tipo) %>% 
  mutate(prop = total/sum(total)) %>% 
  mutate(sexo = 0) %>% 
  bind_rows(
    df_mociba21 %>% 
      filter(ciberacoso == 1, ent == "Aguascalientes") %>% 
      select(sexo, ent, factor, starts_with("p4_")) %>% 
      pivot_longer(
        cols = starts_with("p4_"), 
        names_to = "tipo", 
        values_to = "respuesta") %>% 
      group_by(tipo, respuesta, sexo) %>% 
      summarise(total = sum(factor)) %>% 
      group_by(sexo, tipo) %>% 
      mutate(prop = total/sum(total)) 
  ) %>% 
  mutate(
    sexo = case_when(
      sexo == 0 ~ "Población general", 
      sexo == 1 ~ "Hombres", 
      sexo == 2 ~ "Mujeres"
    ), 
    tipo = case_when(
      tipo == "p4_01" ~ "Le mandó mensajes ofensivos o burlas",
      tipo == "p4_02" ~ "Le hizo llamadas ofensivas o burlas",
      tipo == "p4_03" ~ "Criticó de apariencia o clase social",
      tipo == "p4_04" ~ "Se hizo pasar por usted para enviar información falsa o agredir",
      tipo == "p4_05" ~ "Lo(a) contactó con nombres falsos para molestarle",
      tipo == "p4_06" ~ "Vigiló sus sitios o cuentas de internet",
      tipo == "p4_07" ~ "Lo(a) provocó en línea para que reaccionara de forma negativa",
      tipo == "p4_08" ~ "Le hizo insinuaciones o propuestas de tipo sexual que le molestaron",
      tipo == "p4_09" ~ "Le envió fotos o videos de contenido sexual que le molestaron",
      tipo == "p4_10" ~ "Compartió imágenes de contenido íntimo sexual de usted sin su consentimiento",
      tipo == "p4_11" ~ "Publicó información personal (no sexual) de usted para dañarle",
      tipo == "p4_12" ~ "Amenazón con publicar información a cambio de que usted hiciera algo",
      tipo == "p4_13" ~ "Otra situación",
    )
  )

df_sexo_ags <- df_ags %>% filter(respuesta == 1, sexo != "Población general")

ggplot(df_sexo_ags, 
       aes(x = prop, y = reorder(tipo, prop)), fill = sexo) +
  # facet_wrap(~sexo) +
  # Geoms
  geom_col(aes(fill = sexo), position = "dodge") +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)), 
            position = position_dodge(0.9),
            vjust = if_else(df_sexo_ags$sexo == "Hombres", 0.9, -0.5),
            hjust = -0.1,
            color = "black", size = 3.5) +
  geom_point(
    data = df_sexo %>% filter(respuesta == 1),
    aes(shape = sexo), position =  position_dodge(0.9),
    vjust = if_else(df_sexo_ags$sexo == "Hombres", 0.9, -0.5), hjust = -0.1,
    
  ) +
  # Etiquetas 
  labs( 
    title = "Tipos de ciberacoso más frecuentes en Aguascalientes", 
    subtitle = "Por género de la persona agredida", 
    x = "Porcentaje del total de personas que reportaron ciberacoso", 
    y = "Alguien:", 
    fill = "Aguascalientes", 
    shape = "Nacional", 
    caption = v_caption, 
  ) +
  # Escalas 
  scale_y_discrete(label = scales::wrap_format(25)) +
  scale_x_continuous(limits = c(0, 0.45), labels = scales::percent_format()) +
  # Tema 
  theme_bw() +
  theme(legend.position = "top")


ggsave(file = paste_figs("04_aguascalientes_sexo_tipo.png"), width = 7, height = 8)


## ¿Qué acciones ha tomado en consecuencia? (P12_)
