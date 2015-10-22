
#Cargar paquetes

library(raster)
library(rgdal)
library(shapefiles)
library(pgirmess)
library(maptools)

#Ubicación de la carpeta de trabajo

setwd(".")

#Referecia corta, que sirva de indicador en los archivos generados

ref <- "satander_15_nov_2015"

#Lectura de csv y shapeFile

baseConCoordenadas   <- read.csv("cordenadas_santander.csv")

shapeColombia        <- readShapeSpatial("MUNICIPIOS IGAC/Col_mun_igac_2011_84.shp") # Los archivos para colombia pueden descargarse de ftp://ftp.ciat.cgiar.org/DAPA/projects/BIGDATA_AEPS/GIS_FILES/Divisi%C3%B3n%20p%C3%B3litica%20Colombia_IGAC/Municipios/

#Filtrar variables de interés (Solo si es necesario reducir las columnas de la matriz original, sino se puede saltar)

baseConCoordenadas   <- baseConCoordenadas[c("ID_CULTIVO","USUARIO","PRODUCTOR","LAT_LOTE","LONG_LOTE","DEPARTAMENTO", "CIUDAD","FECHA_SIEMBRA", "FECHA_COSECHA")]

#Variables de Departamento, Municipio, Longitud y Latitud

lonLanomCor <- c("LONG_LOTE","LAT_LOTE")   
dptMunDb    <- c("NOM_DEPART","NOM_MUNICI")
dptMunShap  <- c("DEP_IGAC","MUN_IGAC")


#extraccion de los valores de coordenadas

projection(shapeColombia)<-"+proj=utm +ellps=WGS84 +datum=WGS84 +units=m"

LugarCoordenada <- extract(shapeColombia,baseConCoordenadas[lonLanomCor])[dptMunDb]

names(LugarCoordenada) <- dptMunShap

unionCordUbicac <- cbind(baseConCoordenadas,LugarCoordenada)

#CONVERTIR EN CARACTER Y QUITAR ACENTO

nam <- c("USUARIO","PRODUCTOR","DEPARTAMENTO","CIUDAD","DEP_IGAC","MUN_IGAC")

for(n in nam[2:length(nam)]){
    unionCordUbicac[,n] <- as.character(unionCordUbicac[,n])
    unionCordUbicac[,n] <- iconv(unionCordUbicac[,n], to="ASCII//TRANSLIT")
    
}

#COLOCAR ESPACIOS PERDIDOS DEL RASTER COMO VALORES PERIDOS EN TEXTO

unionCordUbicac[,dptMunShap[1]][is.na(unionCordUbicac[,dptMunShap[1]] )] <- "Perdido"

unionCordUbicac[,dptMunShap[2]][is.na(unionCordUbicac[,dptMunShap[2]])]  <- "Perdido"

#EVALUAR LAS COINCIDENCIAS MUNICIPIO/DEPARTANEBTO

unionCordUbicac$CoincidenciaTotal <- !( is.na(unionCordUbicac$DEP_IGAC) | unionCordUbicac$DEP_IGAC != unionCordUbicac$DEPARTAMENTO | unionCordUbicac$MUN_IGAC != unionCordUbicac$CIUDAD) 


cordConProblemas <- unionCordUbicac[unionCordUbicac$CoincidenciaTotal==FALSE,]

#COORDENADAS CON INCONVENIENTES

coordinates(cordConProblemas) <- c("LONG_LOTE","LAT_LOTE")
proj4string(cordConProblemas) <- CRS("+proj=longlat +datum=WGS84")

writeOGR(cordConProblemas,paste0("cord_incosists_",ref,".kml"), layer="", driver="KML",overwrite_layer=T) 


cordSinProblemas <- unionCordUbicac[unionCordUbicac$CoincidenciaTotal==TRUE,]

#COORDENADAS CON INCONVENIENTES

coordinates(cordSinProblemas) <- c("LONG_LOTE","LAT_LOTE")
proj4string(cordSinProblemas) <- CRS("+proj=longlat +datum=WGS84")

writeOGR(cordSinProblemas, paste0("cord_consists_",ref,".kml"), layer="", driver="KML") 

write.csv(unionCordUbicac,paste0("cord_revision_",ref,".csv"),row.names=F)

