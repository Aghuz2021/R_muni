library(readxl)
library(purrr)
library(tidyverse)
library(tidytext)
library(tm)

# explicito el directorio donde estan los archivos diarios del scrapping
# Con el fin de explorar la información combino todos los archivos en df.properati
# no estoy filtrando las publicaciones repetidas

setwd("G:/Mi unidad/Capas/SEDETyM - DPE/vivienda/SIVyS")
properati.lista <- list.files(pattern='*.csv', recursive = TRUE)
properati.lista <- lapply(properati.lista, read_csv)
df.properati<- properati.lista %>% map(~ mutate(.x, across(everything(), ~as.character(.x))))
df.properati <- bind_rows(df.properati, .id = "id") 

# con el fin de explotar lo más posible el campo de descripción veo cuales son las
# palabras mas comunes.


# primero creo un diccionario con los conectores del español y otras palabras que
# no sean descriptivas


custom_stop_words <- bind_rows(stop_words,
                               tibble(word = tm::stopwords("spanish"),
                                          lexicon = "custom"))

# Con el diccionario armado creo un df con las palabras más comunes del campo
# descripcion, le quito las palabras contenidas en el diccionario
# cuento el número de ocurrencias de cada palabra


descripcion <- df.properati %>%
  select(6) %>% 
  unnest_tokens(word, descripcion) %>% 
  anti_join(custom_stop_words) %>% 
  count(word, sort = TRUE)
view(descripcion)

# Empiezo a ordenar la base y a explotar los campos existentes de caracteristicas
# descripcion y otros


df.properati.trabajada <- df.properati %>% mutate(
  anio_construccion = parse_number(anio_construccion),
  area = parse_number(area),
  banios = parse_number(banios),
  moneda = ifelse(grepl("USD", precio, fixed = TRUE), "USD", "PESOS"),
  precio_corregido = parse_number(precio,locale = locale("es", decimal_mark = ",")),
    gas = ifelse(grepl(
      "Gas", caracteristicas_de_la_vivienda, fixed = TRUE
    ), T, F),
    elec = ifelse(
      grepl("Electricidad", caracteristicas_de_la_vivienda, fixed = TRUE),
      T,
      F
    ),
    agua = ifelse(
      grepl("Agua", caracteristicas_de_la_vivienda, fixed = TRUE),
      T,
      F
    ),
    internet = ifelse(
      grepl("Internet", caracteristicas_de_la_vivienda, fixed = TRUE),
      T,
      F
    ),
    parrilla = ifelse(
      grepl("Parrilla", caracteristicas_de_la_vivienda, fixed = TRUE) |
        grepl(
          "parrilla",
          descripcion,
          fixed = FALSE,
          ignore.case = TRUE
        ),
      T,
      F
    ),
    balcon = ifelse(
      grepl("Balcón", caracteristicas_de_la_vivienda, fixed = TRUE) |
        grepl(
          "balcón",
          descripcion,
          fixed = FALSE,
          ignore.case = TRUE
        ) |
        grepl(
          "balcon",
          descripcion,
          fixed = FALSE,
          ignore.case = TRUE
        ),
      T,
      F
    ),
    terraza = ifelse(
      grepl("Terraza", caracteristicas_de_la_vivienda, fixed = TRUE) |
        grepl("Azotea", caracteristicas_de_la_vivienda, fixed = TRUE) |
        grepl(
          "terraza",
          descripcion,
          fixed = FALSE,
          ignore.case = TRUE
        ),
      T,
      F
    ),
    patio = ifelse(
      grepl("Patio", caracteristicas_de_la_vivienda, fixed = TRUE) |
        grepl(
          "patio",
          descripcion,
          fixed = FALSE,
          ignore.case = TRUE
        ),
      T,
      F
    ),
    jardin = ifelse(
      grepl("Jardín", caracteristicas_de_la_vivienda, fixed = TRUE) |
        grepl(
          "jardín",
          descripcion,
          fixed = FALSE,
          ignore.case = TRUE
        ),
      T,
      F
    ),
    lavadero = ifelse(grepl("lavadero", descripcion, fixed = TRUE), T, F),
    garage = ifelse(
      grepl("Garage", caracteristicas_de_la_vivienda, fixed = TRUE) |
        grepl(
          "cochera",
          descripcion,
          fixed = FALSE,
          ignore.case = TRUE
        ) |
        grepl(
          "cocheras",
          descripcion,
          fixed = FALSE,
          ignore.case = TRUE
        ),
      T,
      F
    ),
    pileta = ifelse(
      grepl("Piscina", caracteristicas_de_la_vivienda, fixed = TRUE),
      T,
      F
    ),
    gimnasio = ifelse(
      grepl("Gimnasio", caracteristicas_de_la_vivienda, fixed = TRUE),
      T,
      F
    ),
    calefaccion = ifelse(
      grepl("Calefacción", caracteristicas_de_la_vivienda, fixed = TRUE) |
        grepl(
          "calef",
          descripcion,
          fixed = FALSE,
          ignore.case = TRUE
        ),
      T,
      F
    ),
    encargado = ifelse(
      grepl("Conserje", caracteristicas_de_la_vivienda, fixed = TRUE),
      T,
      F
    ),
    seguridad = ifelse(
      grepl("Seguridad", caracteristicas_de_la_vivienda, fixed = TRUE),
      T,
      F
    ),
    comercial = ifelse(
      grepl(
        "comercial",
        descripcion,
        fixed = FALSE,
        ignore.case = TRUE
      ),
      T,
      F
    )
    
  )





