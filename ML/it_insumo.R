library(readxl)
IT_nacion<- read_excel("C:/Users/fsavi/Desktop/it/IT nacion-provincia.xlsx", 
                                  sheet = "NACION")%>% 
  mutate(PARTIDA = as.character(PARTIDA))
View(IT_nacion)


frequency_df <- as.data.frame(table(IT_nacion$CAA)) %>% 
  filter(Freq > 1)


a <- IT_nacion %>% 
  group_by(CAA) %>%
  filter(n()>1)

it_nacion <- IT_nacion[!duplicated(IT_nacion$CAA), ]


parcelario <- sf::st_read("C:/Users/fsavi/Desktop/it/117_parcela/110101.shp")
  
union_nacion <- left_join(it_nacion, parcelario, by = c("CAA" = "CCA"), keep = T)

write_xlsx(union_nacion, "it_nacion.xlsx")






IT_provincia<- read_excel("C:/Users/fsavi/Desktop/it/IT nacion-provincia.xlsx", 
                       sheet = "PROVINCIA")%>% 
  mutate(PARTIDA = as.character(PARTIDA))
View(IT_provincia)


frequency_df <- as.data.frame(table(IT_provincia$CAA)) %>% 
  filter(Freq > 1)


a <- IT_provincia %>% 
  group_by(CAA) %>%
  filter(n()>1)

it_provincia <- IT_provincia[!duplicated(IT_provincia$CCA), ]


parcelario <- sf::st_read("C:/Users/fsavi/Desktop/it/117_parcela/110101.shp")

union_provincia <- left_join(it_provincia, parcelario, by = c("CCA" = "CCA"), keep = T)

write_xlsx(union_provincia, "it_provincia.xlsx")
