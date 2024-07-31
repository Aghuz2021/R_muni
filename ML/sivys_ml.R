rm(list=ls())

library(sf)
library(httr)
library(ows4R)
library(svDialogs)
library(tidyverse)
library(sp)
library(nngeo)
library(lwgeom)
library(writexl)
library(utilidades3F)


options(scipen = 100)
wd <- dirname(rstudioapi::getSourceEditorContext()$path)

setwd(wd)

# archivo <- read_csv("insumo/11-12-23.ml.csv")
anio <- dlg_input(message="Ingresar año")$res
mes <- dlg_input(message="Ingresar mes")$res
file_nom <- paste0("insumo/",anio,"_",mes,".xlsx")

archivo <- readxl::read_xlsx(file_nom)

archivo <- archivo %>% mutate(
  LONGITUDE= str_replace_all(LONGITUDE, ",", "."),
  LATITUDE= str_replace_all(LATITUDE, ",", ".")
)


archivo <-filter(archivo, !is.na(LATITUDE)|LATITUDE=="")
archivo <- st_as_sf(archivo, coords = c("LONGITUDE", "LATITUDE"),
              crs = 4326)

archivo <- archivo %>%  mutate(pesosm2tot = round(.$ITE_SITE_CURRENT_PRICE/ .$STotalM2,2),
                               dolaresm2tot = round(.$ITE_BASE_CURRENT_PRICE/ .$STotalM2, 2), .after= ITE_BASE_CURRENT_PRICE)



localidades <- obtener_capa("localidades")

archivo_limp <- st_filter(archivo, localidades, .predicate = st_within)

paste(nrow(archivo)-nrow(archivo_limp),
      "casos han sido limpiados por no estar contenidos en los poligonos de localidades."," Se eliminó el",
      round((nrow(archivo)-nrow(archivo_limp))/nrow(archivo)*100,2),"porcien de los casos")


n_limp <- nrow(archivo)-nrow(archivo_limp)

archivo_limp <- normalizar_localidades(archivo_limp)


archivo_limp <- archivo_limp %>% mutate(ITE_BASE_CURRENT_PRICE =  as.numeric(ITE_BASE_CURRENT_PRICE),
                                        ITE_SITE_CURRENT_PRICE = as.numeric(ITE_SITE_CURRENT_PRICE)) %>%
  mutate(TC= ITE_SITE_CURRENT_PRICE/ITE_BASE_CURRENT_PRICE, .after= ITE_BASE_CURRENT_PRICE)


# funcion para verificar todos digitos iguales
verificar_digitos_iguales <- function(digitos) {
  digitos_str <- as.character(abs(digitos))  # Convert number to string
  digitos <- strsplit(digitos_str, "")[[1]]  # Split number into individual digits
  iguales <- length(unique(digitos)) == 1  # Check if all digits are the same
  return(iguales)
}


# filtro digitos iguales en dolares.
archivo_limp$digitos_iguales <- sapply(archivo_limp$ITE_BASE_CURRENT_PRICE, verificar_digitos_iguales)

archivo_limp <- filter(archivo_limp,archivo_limp$digitos_iguales== FALSE)




# filtro digitos iguales en pesos

archivo_limp$digitos_iguales_pesos <- sapply(archivo_limp$ITE_SITE_CURRENT_PRICE, verificar_digitos_iguales)

archivo_limp <- filter(archivo_limp,archivo_limp$digitos_iguales_pesos== FALSE)

paste(nrow(archivo)-(nrow(archivo_limp)+n_limp),
      "casos han sido limpiados por tener un precio con todos los fígitos iguales (ej: 11111111)."," Se eliminó el",
      round((nrow(archivo)-(nrow(archivo_limp)+n_limp))/(nrow(archivo)+n_limp)*100,2),"porcien de los casos")


n_limp <- nrow(archivo)-(nrow(archivo_limp))
# filtro publicaciones menores a 25 metros cuadrados totales

archivo_limp <- filter(archivo_limp, archivo_limp$STotalM2 >=25)

paste(nrow(archivo)-(nrow(archivo_limp)+n_limp),
      "casos han sido limpiados por tener metros cuadrados totales menores a 25"," Se eliminó el",
      round((nrow(archivo)-(nrow(archivo_limp)+n_limp))/(nrow(archivo)+n_limp)*100,2),"porcien de los casos")



n_limp <- nrow(archivo)-(nrow(archivo_limp))

paste(n_limp,
      "casos han sido limpiados por todos los motivos anteriores"," Se eliminó el",
      round(n_limp/(nrow(archivo))*100,2),"porcien de los casos")




archivo_limp_alq <- filter(archivo_limp, archivo_limp$OPERACION=="Alquiler")

archivo_limp_vent <- archivo_limp %>% filter(OPERACION=="Venta")

boxplot(archivo_limp_alq$ITE_BASE_CURRENT_PRICE)
boxplot(archivo_limp_vent$ITE_BASE_CURRENT_PRICE)

iqr <- IQR(archivo_limp_alq$ITE_BASE_CURRENT_PRICE, na.rm = T)


out <- boxplot.stats(archivo_limp_vent$dolaresm2tot)$out

out_ind <- which(archivo_limp_vent$dolaresm2tot %in% c(out))

outliers_vent <- archivo_limp_vent[out_ind, ]




archivo_vent2 <- archivo_vent %>% mutate(ITE_BASE_CURRENT_PRICE= ifelse(DigitsEqual,NA, ITE_BASE_CURRENT_PRICE)) %>%
  filter(ITE_BASE_CURRENT_PRICE<2000000)


hist(archivo_vent2$ITE_BASE_CURRENT_PRICE,
     xlab = "hwy",
     main = "Histogram of hwy",
     breaks = sqrt(nrow(archivo_vent2)))



parcelario <-  utilidades3F::obtener_capa("Parcelario")




par <- utilidades3F::obtener_capa("Parcelario")

archivo_geo <-filter(archivo, !is.na(LATITUDE)|LATITUDE=="")
s <- st_as_sf(archivo_geo, coords = c("LONGITUDE", "LATITUDE"),
                        crs = 4326)
s <- st_transform(s,4326)


centroides <- st_centroid(parcelario)
st_write(centroides, "centroides.geojson",delete_dsn = T )
centroides <- st_read("centroides.geojson")


prueba <- normalizar_localidades(s)


joined_sf <- prueba %>%
  cbind(
    centroides[st_nearest_feature(prueba, centroides),])

joined_sf<- joined_sf %>% select(1:15, 18, 56:57)

dist <- joined_sf %>%
  mutate( dist = st_distance(geometry, geometry.1, by_element = T)) %>%
  mutate(dist= as.character(dist))

dist <- dist %>% mutate(dist=str_replace_all(dist, ",", "."))
dist <- dist %>% mutate(dist= as.numeric(dist))

dist2 <-dist %>%
  mutate( CCA = if_else(.$dist>50,NA,CCA),
          geometry.1 = if_else(.$dist>50,NA,geometry.1),
          dist = if_else(.$dist>50,NA,dist))

st_write(dist2,"ml12.geojson")
write_xlsx(dist2,"2023_12_ml_limp.xlsx")
write_xlsx()

