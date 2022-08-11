#------------------------------------------------------------------------------#
# Proyecto:                   MÓDULO DE CIBERACOSO (MOCIBA)
# Objetivo:                   Patrones de violencia en 2021
#
# Encargadas:                 Regina Isabel Medina Rosales
# Correo:                     remedina@intersecta.org
# Fecha de creación:          03 de agosto de 2022
# Última actualización:       11 de agosto de 2022
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# Silenciar warnings 
options(warn=-1)

# Cargar librerías 
# remotes::install_github("IndrajeetPatil/kittyR")
require(pacman)
p_load(foreign, srvyr, tidyverse, dplyr, lubridate, scales, kittyR)


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

v_caption <- "Fuente: Módulo sobre Ciberacoso (MOCIBA) 2021 del INEGI. Datos procesados por INTERSECTA (intersecta.org).\n"

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


# 4. Figuras -------------------------------------------------------------------

## 4.0. Tema -------------------------------------------------------------------

# ---- Tema 
tema        <-  theme_linedraw() +
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    text                  = element_text(family = "Roboto Slab", color = "black"),
    plot.title            = element_text(family = "Roboto Slab", color = "black",   size = 16,  face  = "bold",  margin = margin(10,5,5,5)),
    plot.subtitle         = element_text(family = "Roboto Slab", color = "black",   size = 14,  margin = margin(5, 5, 5, 5)),
    plot.caption          = element_text(family = "Fira Sans",   color = "#92A39D", size = 8,  hjust = 0),
    panel.grid            = element_line(linetype = 2),
    plot.margin           = margin(0, 2, 0, 1.5, "cm"),
    legend.position       = "top",
    panel.border          = element_blank(),
    legend.title          = element_text(size = 11, family = "Fira Sans", face   = "bold"),
    legend.text           = element_text(size = 11, family = "Fira Sans"),
    axis.title            = element_text(size = 11, family = "Fira Sans", hjust = .5, margin = margin(1,1,1,1)),
    axis.text.y           = element_text(size = 11, family = "Fira Sans", angle=0,  hjust=1),
    axis.text.x           = element_text(size = 11, family = "Fira Sans", angle=90, hjust=1, vjust = 0.5),
    strip.text.x          = element_text(size = 11, family = "Fira Sans", face = "bold", color = "black"),
    strip.text.y          = element_text(size = 11, family = "Fira Sans", face = "bold", color = "black"),
    strip.background      = element_rect(fill = "white", color = NA))


# ---- Colores 
c2 <- c("#E09F3E", "#9E2A2B")


## 4.1. Personas que vivieron ciberacoso ---------------------------------------

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
  # geom_hline(data = df_data %>% 
  #              filter(ciberacoso == 1) %>% 
  #              filter(sexo == "Población general"), 
  #            aes(yintercept = prop), linetype = "dashed") +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)), 
            family = "Fira Sans", 
            vjust = -1) +
  # Etiquetas 
  labs( 
    title = "Población mayor de 12 años que sufrió\nciberacoso en México", 
    subtitle = "Por sexo de la persona agredida\n", 
    x = "\nSexo", 
    y = "Porcentaje\n", 
    fill = "", 
    shape = "Sexo a nivel nacional", 
    caption = v_caption, 
  ) +
  # Escalas 
  scale_y_continuous(label = scales::percent_format(), limits = c(0, 0.27)) +
  scale_fill_manual(values = c2) +
  # Tema
  tema +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  theme(legend.position = "none")

ggsave(file = paste_figs("01_nacional_sexo.png"), 
       device = "png", type = "cairo", 
       width = 6, height = 6)


## 4.2. Sexo y entidad ---------------------------------------------------------

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
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)), 
            family = "Fira Sans", 
            vjust = -0.8, size = 2) +
  # Etiquetas 
  labs( 
    title = "Población mayor de 12 años que sufrió ciberacoso en México", 
    subtitle = "Por sexo de la persona agredida y entidad federativa\n", 
    x = "\n", 
    y = "Porcentaje\n", 
    fill = "", 
    caption = v_caption, 
  ) +
  # Escalas
  scale_y_continuous(limits = c(0, 0.35), labels = scales::percent_format()) +
  scale_fill_manual(values = c2) +
  # Tema
  tema +
  theme(
    axis.text.x = element_blank(), 
    strip.text.x = element_text(size = 8))

ggsave(file = paste_figs("02_entidades_sexo.png"), 
       device = "png", type = "cairo", 
       width = 9, height = 6)


## 4.3. Sexo y edad ------------------------------------------------------------

df_data <- df_mociba21 %>% 
  # filter(ciberacoso == 1) %>% 
  filter(edad < 98) %>% 
  mutate(
    grupo_edad = case_when(
      edad    < 18    ~ "Entre 12 y 18", 
      edad %in% 18:29 ~ "Entre 18 y 29", 
      edad %in% 30:39 ~ "Entre 30 y 39", 
      edad %in% 40:49 ~ "Entre 40 y 49", 
      edad %in% 50:59 ~ "Entre 50 y 59", 
      edad   >= 60    ~ "60 o más")) %>% 
  group_by(ciberacoso, sexo, grupo_edad) %>% 
  summarise(total = sum(factor)) %>% 
  group_by(grupo_edad, sexo) %>% 
  mutate(prop = total/sum(total)) %>% 
  filter(ciberacoso == 1) %>% 
  mutate(
    sexo = case_when(
      sexo == 0 ~ "Población general", 
      sexo == 1 ~ "Hombres", 
      sexo == 2 ~ "Mujeres"), 
    grupo_edad = factor(grupo_edad, levels = c(
      "Entre 12 y 18", "Entre 18 y 29", "Entre 30 y 39", "Entre 40 y 49", 
      "Entre 50 y 59", "60 o más"))
  )


ggplot(df_data, 
  # Coordenadas
        aes(x = prop, y = grupo_edad, fill = sexo)) +
  facet_wrap(~sexo) +
  # Geom
  geom_col() +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)), 
            family = "Fira Sans", 
            hjust = -0.2, size = 3) +
  # Etiquetas 
  labs( 
    title = "Población mayor de 12 años que sufrió ciberacoso en México", 
    subtitle = "Por sexo y edad de la persona agredida\n", 
    y = "Edad (en años)\n", 
    x = "\nPorcentaje\n", 
    fill = "", 
    caption = v_caption, 
  ) +
  # Escalas
  scale_x_continuous(limits = c(0, 0.35), labels = scales::percent_format()) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = c2) +
  tema +
  theme(legend.position = "none")


ggsave(file = paste_figs("03_edad_sexo.png"), 
       device = "png", type = "cairo", 
       width = 9, height = 6)



## 4.4. Tipo de agresión -------------------------------------------------------

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
            position = position_dodge(1), 
            vjust = if_else(df_sexo$sexo == "Hombres", 1.5, -0.2),
            hjust = -0.2,
            family = "Fira Sans", 
            color = "black", size = 3.5) +
#   geom_point(
#     data = df_data %>% filter(respuesta == 1, sexo == "Población general"),
#              aes(shape = sexo)
# ) +
  # Etiquetas 
  labs( 
    title = "Tipos de ciberacoso más frecuentes en México", 
    subtitle = "Por sexo de la persona agredida\n", 
    x = "\nPorcentaje del total de personas que reportaron ciberacoso\n", 
    y = "Alguien:", 
    shape = "", 
    fill = "",
    caption = v_caption, 
    ) +
  # Escalas 
  scale_y_discrete(label = scales::wrap_format(30)) +
  scale_x_continuous(limits = c(0, 0.4), labels = scales::percent_format()) +
  scale_fill_manual(values = c2) +
  # Tema 
  tema +
  theme(axis.title.y = element_text(face = "bold"))

ggsave(file = paste_figs("04_nacional_sexo_tipo.png"), 
       device = "png", type = "cairo", 
       width = 9, height = 10)


## 4.5. Tipo en Aguascalientes -------------------------------------------------

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
            position = position_dodge(1),
            vjust = if_else(df_sexo$sexo == "Hombres", 1.5, -0.2),
            family = "Fira Sans", 
            hjust = -0.2,
            color = "black", size = 3.5) +
  geom_point(
    data = df_sexo %>% filter(respuesta == 1),
    aes(shape = sexo), position =  position_dodge(0.9),
    vjust = if_else(df_sexo_ags$sexo == "Hombres", 0.9, -0.5), hjust = -0.1,
    
  ) +
  # Etiquetas 
  labs( 
    title = "Tipos de ciberacoso más frecuentes en Aguascalientes", 
    subtitle = "Por sexo de la persona agredida\n", 
    x = "\nPorcentaje del total de personas que reportaron ciberacoso\n", 
    y = "Alguien:", 
    fill = "Aguascalientes:", 
    shape = "Nacional:", 
    caption = v_caption, 
  ) +
  # Escalas 
  scale_y_discrete(label = scales::wrap_format(30)) +
  scale_x_continuous(limits = c(0, 0.45), labels = scales::percent_format()) +
  scale_fill_manual(values = c2) +
  # Tema 
  tema +
  theme(axis.title.y = element_text(face = "bold"))


ggsave(file = paste_figs("04_aguascalientes_sexo_tipo.png"), 
       device = "png", type = "cairo", 
       width = 9, height = 10)


## 4.6. Relación personas agresoras --------------------------------------------

# Relación con la persona agresora "P5_"

# 01	Novio(a) / Pareja actual
# 02	Ex novio(a) / ex pareja
# 03	Familiar
# 04	Amigo(a)
# 05	Compañero(a) de clase / trabajo
# 06	Conocido(a) de poco trato
# 07	Conocido(a) solo de vista
# 08	Desconocido(a)
# 09	Otro (ESPECIFIQUE) 
# 99	No sabe / no responde


df_data <- df_mociba21                              %>% 
  filter(ciberacoso == 1)                           %>% 
  select(sexo, ent, factor, starts_with("p5_"))     %>% 
  pivot_longer(
      cols = starts_with("p5_"), 
      names_to = "code", 
      values_to = "agresor")                        %>%
  drop_na(agresor) %>% 
  # filter(agresor != 99)                             %>%
  mutate(
    sexo = case_when(
      sexo == 1 ~ "Hombres", 
      sexo == 2 ~ "Mujeres"
    ), 
    agresor = case_when(
      agresor == 1   ~ "Novio(a) / Pareja actual",
      agresor == 2   ~ "Ex novio(a) / ex pareja",
      agresor == 3   ~ "Familiar",
      agresor == 4   ~ "Amigo(a)",
      agresor == 5   ~ "Compañero(a) de clase / trabajo",
      agresor == 6   ~ "Conocido(a) de poco trato",
      agresor == 7   ~ "Conocido(a) solo de vista",
      agresor == 8   ~ "Desconocido(a)",
      agresor == 9   ~ "Otro (ESPECIFIQUE) ",
      agresor == 99  ~ "No sabe / no responde",
    )
  ) %>% 
  group_by(sexo, agresor)                         %>% 
  summarise(total = sum(factor))                  %>% 
  group_by(sexo)                                  %>% 
  mutate(prop = total/sum(total))     

ggplot(df_data, 
  # Coordenadas
            aes(x = prop, y = reorder(agresor, prop), fill = sexo)) +
  facet_wrap(~sexo) +
  # Geoms
  geom_col() +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)), 
            family = "Fira Sans", 
            hjust = if_else(df_data$agresor != "Desconocido(a)", -0.25, 1), size = 3) +
  # Etiquetas 
  labs( 
    title = "Relación con la persona agresora en\ncasos de ciberacoso en México", 
    subtitle = "Por sexo de la persona agredida y relación\ncon la persona agresora \n", 
    x = "\nPorcentaje del total de personas que reportaron ciberacoso\n", 
    y = "", 
    fill = "",
    caption = v_caption, 
  ) +
  # Escalas 
  scale_y_discrete(label = scales::wrap_format(20)) +
  scale_x_continuous(limits = c(0, 0.65), labels = scales::percent_format()) +
  scale_fill_manual(values = c2) +
  # Tema 
  tema +
  theme(legend.position = "none")

ggsave(file = paste_figs("05_agresor_relacion_sexo.png"), 
       device = "png", type = "cairo", 
       width = 6, height = 6)


## 4.7. Sexo personas agresoras ------------------------------------------------

# Sexo de personas agresoras inicia con "P6_"


df_data <- df_mociba21                              %>% 
  filter(ciberacoso == 1)                           %>% 
  select(sexo, ent, factor, starts_with("p6_"))     %>% 
  pivot_longer(
    cols = starts_with("p6_"), 
    names_to = "code", 
    values_to = "agresor")                          %>%
  drop_na(agresor) %>% 
  # filter(agresor != 99)                             %>%
  mutate(
    sexo = case_when(
      sexo == 1 ~ "Hombres", 
      sexo == 2 ~ "Mujeres"
    ), 
    agresor = case_when(
      agresor == 1   ~ "Agresor(es) hombre(s)",
      agresor == 2   ~ "Agresora(as) mujer(es)",
      agresor == 3   ~ "Agresores hombre y mujer",
      agresor == 9   ~ "No sabe",
    )
  ) %>% 
  group_by(sexo, agresor)                         %>% 
  summarise(total = sum(factor))                  %>% 
  group_by(sexo)                                  %>% 
  mutate(prop = total/sum(total))  

ggplot(df_data, 
       # Coordenadas
       aes(x = prop, y = reorder(agresor, prop), fill = sexo)) +
  facet_wrap(~sexo) +
  # Geoms
  geom_col() +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)), 
            family = "Fira Sans", 
            hjust = if_else(df_data$agresor != "Agresor(es) hombre(s)", -0.25, 1), size = 3) +
  # Etiquetas 
  labs( 
    title = "Sexo de la persona agresora en\ncasos de ciberacoso en México", 
    subtitle = "Por sexo de la persona agredida\n", 
    x = "\nPorcentaje del total de personas que reportaron ciberacoso\n", 
    y = "", 
    fill = "",
    caption = v_caption, 
  ) +
  # Escalas 
  scale_y_discrete(label = scales::wrap_format(15)) +
  scale_x_continuous(limits = c(0, 0.65), labels = scales::percent_format()) +
  scale_fill_manual(values = c2) +
  # Tema 
  tema +
  theme(legend.position = "none")

ggsave(file = paste_figs("06_agresores_sexo.png"), 
       device = "png", type = "cairo", 
       width = 6, height = 5)


## 4.8. En casos de contenido íntimo -------------------------------------------


meowR(sound = 3)

# FIN. -------------------------------------------------------------------------


