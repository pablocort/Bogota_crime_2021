### Pablo Cortés 



# Limpiar la consola, el entorno y fijar directorio de trabajo
cat('\f')
rm(list=ls())
options('scipen'=100, 'digits'=4) # Forzar a R a no usar e+
setwd("~/uniandes/big data y machine learning/final_project")
getwd()

#### 0.2. Instalar las librerias que vamos a usar en la clase de hoy
paquetes = c("tidyverse",'sf','ggmap','ggsn','ggnewscale','ggplot2',
             'RColorBrewer','viridis','scales', 'grid', 'raster',
             'png', 'ggspatial', 'openxlsx', 'gganimate', 'vtable',
             'cowplot','scales', 'sysfonts','showtext','geojsonio',
             'patchwork')
for ( paquete in paquetes){
  if (length(grep(paquete,installed.packages()[,1])) == 0 ){ install.packages(paquete) ; print(paste0('La libreria ', '"', paquete ,'"', ' ha sido instalada.'))}
  else { print(paste0('La libreria ', '"', paquete ,'"', ' ya estÃ¡ instalada.'))}
  rm(paquete)
}

#### Llamar las librerias
sapply(paquetes,require,character.only=T) 
rm(paquetes)


#install.packages("devtools") 
devtools::install_github("BlakeRMills/MetBrewer", force = TRUE) 



### cargar upz de bogotá
upz <- st_read('data/proyecto_educacion/upz', layer = 'upz') %>%
  dplyr::select(NOMBRE, geometry) 

## cambiar nombre de variable en base de upz_geo para igualar identificadores
colnames(upz)[1] = 'UPZ'


### Cargar la base de colegios para tomar CRS
setwd("~/Base de datos/Georeferencia")
colegios = st_read('bta/11_BOGOTA/URBANO', 
                   layer = "MGN_URB_TOPONIMIA") 

colegios <- subset(colegios,
                   CSIMBOL == '020903')

### Pasar coordenadar de upz a coordenadas DANE

upz$geometry[[1]]
jardines_bta$geometry[[7]]
crs(upz)
crs(colegios)
upz <- st_transform(x = upz, crs = crs(colegios))
crs(upz)
rm(colegios)


### Cargar base de crímenes

### descarga de todos los archivos como una lista y pegarlos verticalmente
setwd("~/uniandes/big data y machine learning/final_project")

filenames <- list.files('data/proyecto_educacion/crimen_bta/2020/',
                        pattern="Siedco_Datos_Detallados", 
                        full.names=TRUE)

crimen <- lapply(filenames, read.csv) %>%
  bind_rows()

### Arreglos inciales

# Nombres de variables
colnames(crimen)[1] = 'Fecha'
colnames(crimen)[2] = 'year'
colnames(crimen)[3] = 'numero_mes'
colnames(crimen)[4] = 'mes'
st(crimen)


# Valores de strings
crimen$Rango.del.Dia <- plyr::revalue(crimen$Rango.del.Dia,
                                      c(`MAÃ'ANA` = 'morning'))

crimen$Nombre.Dia <- plyr::revalue(crimen$Nombre.Dia,
                                   c(`MIÃ???RCOLES`='miercoles'))

crimen$Nombre.Dia <- plyr::revalue(crimen$Nombre.Dia,
                                   c('SÃ\u0081BADO'='sabado'))

### Manipulación de base obtener conteos

crimenes_total <- crimen %>%
  group_by(Delito, Sexo, UPZ, Numero.Hechos) %>%
  dplyr::count(Delito) %>% 
  mutate(total_crimen = Numero.Hechos*n) %>%
  dplyr::select(-c(n, Numero.Hechos)) 

#%>% ViewPipeSteps::print_pipe_steps() -> result


### Igualar nombres de las upz en las bases
crimenes_total$UPZ <- plyr::revalue(crimenes_total$UPZ, 
                                    c('MINUTO DE DIOS'='EL MINUTO DE DIOS'))
crimenes_total$UPZ <- plyr::revalue(crimenes_total$UPZ, 
                                    c('PARQUE SIMON BOLIVAR - CAN'='PARQUE SIMON BOLIVAR-CAN'))
crimenes_total$UPZ <- plyr::revalue(crimenes_total$UPZ, 
                                    c('PARQUE SALITRE'='PARQUE EL SALITRE'))
crimenes_total$Delito <- plyr::revalue(crimenes_total$Delito, 
                                       c('VIOLENCIA INTRAFAMILIAR'='VIOLENCIA \nINTRAFAMILIAR'))
crimenes_total$Delito <- plyr::revalue(crimenes_total$Delito, 
                                       c('LESIONES PERSONALES'='LESIONES \nPERSONALES'))


### crímenes de interés
  crimenes_total <- crimenes_total[(
    crimenes_total$Delito == 'HOMICIDIOS' |
      crimenes_total$Delito == 'LESIONES \nPERSONALES' |
      crimenes_total$Delito == 'HURTO A PERSONAS' |
      crimenes_total$Delito == 'DELITOS SEXUALES' |
      crimenes_total$Delito == 'HURTO DE BICICLETAS' |
      crimenes_total$Delito == 'HURTO DE CELULARES' |
      crimenes_total$Delito == 'HURTO A RESIDENCIAS' |
      crimenes_total$Delito == 'HURTO AUTOMOTORES' |
      crimenes_total$Delito == 'HURTO DE CELULARES (CASOS)' |
      crimenes_total$Delito == 'HURTO A COMERCIO' |
      crimenes_total$Delito == 'HURTO MOTOCICLETAS' |
      crimenes_total$Delito == 'VIOLENCIA \nINTRAFAMILIAR' |
      crimenes_total$Delito == 'AMENAZAS'|
      crimenes_total$Delito == 'EXTORSION' ),]
  
  

crimenes_total <- crimenes_total %>%
  dplyr::group_by(Delito, Sexo, UPZ) %>%
  summarise(n_crimen = sum(total_crimen))


crimenes_1 <- reshape2::dcast(crimenes_total, 
                     UPZ ~ Delito, 
                     fun.aggregate = sum,
                     value.var = "n_crimen") 

cols <- c(1:15)            
for ( col in cols){
  colnames(crimenes_1)[col] <- gsub(" ", "_", colnames(crimenes_1[col]))
}     


crimenes_ <- crimenes_1 %>%
  replace(is.na(.), 0) %>%
  mutate(Hurtos = HURTO_A_COMERCIO + HURTO_A_PERSONAS +
           HURTO_A_RESIDENCIAS + HURTO_AUTOMOTORES +
           HURTO_DE_BICICLETAS + HURTO_DE_CELULARES +
           `HURTO_DE_CELULARES_(CASOS)` + HURTO_MOTOCICLETAS,
         'Amenazas o extorsiones' = AMENAZAS + EXTORSION) %>%
  dplyr::select(UPZ,DELITOS_SEXUALES, HOMICIDIOS, 'LESIONES_\nPERSONALES',
                'VIOLENCIA_\nINTRAFAMILIAR', Hurtos, 
                'Amenazas o extorsiones')

### Transformamos variables para obtener el porcentaje de crímenes por upz

  crimenes_total_upz <- crimenes_ %>% 
    summarise(porc_delitos_sex = sum(DELITOS_SEXUALES),
              porc_homicidios = sum(HOMICIDIOS),
              porc_lesiones =sum(`LESIONES_
PERSONALES`),
              porc_intrafamiliar = sum(`VIOLENCIA_
INTRAFAMILIAR`), porc_amenazas = sum(`Amenazas o extorsiones`),
              porc_hurtos = sum(Hurtos))
            
### Consolidar porcentajes
crimenes_$porc_delitos_sex <- 
  (crimenes_$DELITOS_SEXUALES / crimenes_total_upz$porc_delitos_sex)*100
crimenes_$porc_homicidios <- 
  (crimenes_$HOMICIDIOS / crimenes_total_upz$porc_homicidios)*100
crimenes_$porc_lesiones <- (crimenes_$`LESIONES_
PERSONALES` /crimenes_total_upz$porc_lesiones)*100
crimenes_$porc_intrafamiliar <- 
  (crimenes_$`VIOLENCIA_
INTRAFAMILIAR` / crimenes_total_upz$porc_intrafamiliar)*100
crimenes_$porc_amenazas <- 
  (crimenes_$`Amenazas o extorsiones` / crimenes_total_upz$porc_amenazas)*100
crimenes_$porc_hurtos <- 
  (crimenes_$Hurtos / crimenes_total_upz$porc_hurtos)*100

### base final para mapas
crimenes_ <- left_join(upz, crimenes_, by = 'UPZ')



#### MAPA

# Aes
showtext_auto()
font_add_google("Fira Sans")
font_add_google("Oswald")

font_add_google("Oswald", "oswald")
font_add_google("Roboto Condensed", "roboto condensed")
font_add_google("Share Tech Mono", "techmono")

font1 <- "Fira Sans"
font2 <- "Oswald"


### Gráficas de crímenes con paleta de colores (individual)

## 1 Violencia intrafamiliar
violencia_intraf <- ggplot() + 
  geom_sf(data = crimenes_, aes(fill= porc_intrafamiliar, 
                              geometry=geometry), color="gray60") +
  coord_sf() +
  scale_fill_gradientn(colors = MetBrewer::met.brewer("Hiroshige")[5:10],
                       labels=comma,
                       breaks = breaks_extended(n=7),
                       guide = guide_colorbar(ticks.linewidth = 4)) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        plot.margin = margin(2.5, 0.5, 1.25, 0.5, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.text = element_text(size=10, family = font2,
                                   face="bold", color="#7F2704"))
violencia_intraf

## 2 delitos sexuales
delitos_sex <- ggplot() + 
  geom_sf(data=crimenes_, aes(fill=porc_delitos_sex, 
                              geometry=geometry), color="gray60") +
  coord_sf() +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "RdPu")[3:9],
                       labels=comma,
                     #  breaks = breaks_extended(n=8),
                       guide = guide_colorbar(ticks.linewidth = 4)) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        plot.margin = margin(2.5, 0.5, 1.25, 0.5, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.text = element_text(size=10, family = font2,
                                   face="bold", color="#7F2704"))
delitos_sex

## 3 Hurtos
  hurtos <- ggplot() + 
  geom_sf(data=crimenes_, aes(fill= porc_hurtos, 
                              geometry=geometry), color="gray60") +
  coord_sf() +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Oranges")[3:9],
                       labels=comma,
                       breaks = breaks_extended(n=5),
                       guide = guide_colorbar(ticks.linewidth = 4)) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        plot.margin = margin(2.5, 0.5, 1.25, 0.5, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.text = element_text(size=10, family = font2,
                                   face="bold", color="#7F2704"))

  hurtos
  
## 4 Homicidios
  homicidios <- ggplot() + 
    geom_sf(data=crimenes_, aes(fill= porc_homicidios, 
                                geometry=geometry), color="gray40") +
    coord_sf() +
    scale_fill_gradientn(colors = rev(MetBrewer::met.brewer("Troy"))[4:8],
                         labels=comma,
                         #breaks = breaks_extended(n=10),
                         guide = guide_colorbar(ticks.linewidth = 4)) +
    theme_void() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_blank(),
          plot.margin = margin(2.5, 0.5, 1.25, 0.5, "cm"),
          legend.key.width = unit(1, "cm"),
          legend.key.height = unit(0.3, "cm"),
          legend.text = element_text(size=10, family = font2,  face="bold", color="#7F2704"))
  
  homicidios 

## 5 LESIONES \nPERSONALES
  lesiones_personales <- ggplot() + 
    geom_sf(data=crimenes_, 
            aes(fill= porc_lesiones, 
                geometry=geometry), 
            color="gray60") +
    coord_sf() +
    scale_fill_gradientn(colors = MetBrewer::met.brewer("Cassatt2")[6:10],
                         labels=comma,
                         breaks = breaks_extended(n=5),
                         guide = guide_colorbar(ticks.linewidth = 4)) +
    theme_void() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_blank(),
          plot.margin = margin(2.5, 0.5, 1.25, 0.5, "cm"),
          legend.key.width = unit(1, "cm"),
          legend.key.height = unit(0.3, "cm"),
          legend.text = element_text(size=10,family = font2, face="bold", color="#7F2704"))
  lesiones_personales

## 6 Amenazas o extorsiones
  amenazas <- ggplot() +
    geom_sf(data = crimenes_,
            aes(fill = porc_amenazas,
                geometry = geometry),
            color = 'gray50',
            size=0.02) +
    coord_sf() +
    scale_fill_gradientn(colors = rev(MetBrewer::met.brewer("VanGogh1",
                                                        type="continuous"))[3:7],
                         labels = comma,
                         # breaks = breaks_extended(n = 7),
                         guide = guide_colorbar(ticks.linewidth = 4)) +
    theme_void() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_blank(),
          plot.margin = margin(2.5, 0.5, 1.25, 0.5, "cm"),
          legend.key.width = unit(1, "cm"),
          legend.key.height = unit(0.3, "cm"),
          legend.text = element_text(size=10,family = font2, 
                                     face="bold", color="#7F2704"))
  amenazas
  
  
### Consolidado total de mapas
  ##
  gg_inset_map2 <- ggdraw()+
    
    draw_text(text="Datos disponibles en: https://scj.gov.co/es/oficina-oaiee/estadisticas-mapas | GitHub: pablocort | Twitter: @PabloCort", 
              x=0.5, y=0.02, size=9, color="Black", fontface="bold", family=font1) +
    #
    draw_text(text="Criminalidad en Bogotá - Colombia (2021)", 
              x=0.03, y=0.5, size=25, color="#3f000a", 
              fontface="bold", family=font1, srt= 90) +
    #
    draw_plot(delitos_sex,
              x = 0.15, y = -0.02,width = 0.67, height = 0.67) +
    draw_plot(violencia_intraf, 
              x = 0.4, y = -0.02,width = 0.67, height = 0.67) +
    draw_plot(hurtos, 
              x = -0.10, y = -0.02,width = 0.67, height = 0.67) +
    draw_plot(homicidios, 
              x = 0.15, y = 0.47,width = 0.67, height = 0.67) +
    draw_plot(lesiones_personales, 
              x = 0.4, y = 0.47,width = 0.67, height = 0.67) +
    draw_plot(amenazas, 
              x = -0.10, y = 0.47,width = 0.67, height = 0.67) +
    #
    draw_text(text="Amenazas y \nextorsiones", x=0.21, y=0.96, size=12, color="#08306B", 
              fontface="bold", family=font1) +
    draw_text(text="Homicidios", x=0.47, y=0.95, size=12, color="#67000D", fontface="bold", 
              family=font1) +
    draw_text(text="Lesiones \npersonales", x=0.71, y=0.96, size=12, color="#00441B", fontface="bold",
              family=font1) +
    draw_text(text="Hurtos", x=0.23, y=0.46, size=12, color="#7F2704", 
              fontface="bold", family=font1) +
    draw_text(text="Delitos \nsexuales", x=0.47, y=0.47, size=13, color="#AE017E", fontface="bold",
              family=font1)+
    draw_text(text="Violencia \nIntrafamiliar", x=0.71, y=0.47, size=13, color="#2171B5", fontface="bold",
              family=font1) +
    #
    draw_text(text="% del total de homicidios cometidos", x=0.49, y=0.6, 
              size=10, color="#67000D", fontface="bold", family=font2) +
    draw_text(text="% del total de lesiones personales", x=0.74, y=0.60, 
              size=10, color="#00441B", fontface="bold", family=font2) +
    draw_text(text="% del total de amenazas y extorsiones", x=0.24, y=0.60, 
              size=10, color="#434475", fontface="bold", family=font2) +
    draw_text(text="% del total de delitos sexuales", x=0.49, y=0.11, 
              size=10, color="#7A0177", fontface="bold", family=font2) +
    draw_text(text="% del total de hurtos", x=0.24, y=0.11, 
              size=10, color="#A63603", fontface="bold", family=font2) +
    draw_text(text="% del total de delitos de violencia intrafamiliar", x=0.74, y=0.11, 
              size=10, color="#1e466e", fontface="bold", family=font2) +
    draw_text(text="Las cifras de delitoss de \nviolencia intrafamiliar \npresentan alto subreporte", 
              x=0.65, y=0.38, size=10, color="#1e466e", lineheight=0.8, family=font2) + 
    draw_text(text="Las cifras de delitos \nsexuales presentan \nalto subreporte", 
              x=0.415, y=0.38, size=10, color="#7A0177", lineheight=0.8, family=font2) + 
    draw_text(text="Se incluyen hurtos de \ncelulares, automotores, bicicletas\nmotocicletas, de comercio, \nde residencias y a personas \n(no especificado)", 
              x=0.14, y=0.38, size=10, color="#A63603", lineheight=0.8, family=font2) +
    draw_line(x=c(-40, 0.22), y=c(1.1, 1.1), color="#3f000a", size=3.5) +
    draw_line(x=c(0.78, 1.2), y=c(5, 1.1), color="#3f000a", size=3.5) +
    #
    draw_text(text = "n = 11.598", x=0.155, y=0.75, 
              size=11, color="#434475", fontface="bold", family=font2) +
    draw_text(text = "n = 1.029", x=0.405, y=0.75, 
              size=11, color="#67000D", fontface="bold", family=font2) +
    draw_text(text = "n = 19.781", x=0.655, y=0.75, 
              size=11, color="#00441B", fontface="bold", family=font2) +
    draw_text(text = "n = 227.308", x=0.155, y=0.25, 
              size=11, color="#A63603", fontface="bold", family=font2) +
    draw_text(text = "n = 4.934", x=0.405, y=0.25, 
              size=11, color="#7A0177", fontface="bold", family=font2) +
    draw_text(text = "n = 29.496", x=0.655, y=0.25, 
              size=11, color="#1e466e", fontface="bold", family=font2)
    

  gg_inset_map2
  
  

  
  
