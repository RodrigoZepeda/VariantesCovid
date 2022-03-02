#DATOS DE https://www.epicov.org/epi3/frontend#62aa9e
#VARIANT SURVEILLANCE
rm(list = ls())

library(covidmx)
library(tidyverse)
library(lubridate)
library(ggstream)
library(MetBrewer)
library(latexpdf)
library(cowplot)


#Lectura de la base
#------------------------------------------------
fname                <- list.files(pattern = "variant_surveillance_tsv*", full.names = T)
tsv_name             <- untar(fname, list = TRUE)  
tsv_name             <- tsv_name[which(str_detect(tsv_name,".tsv"))]
untar(fname, as.character(tsv_name))
variant_surveillance <- read_delim(tsv_name, delim = "\t", escape_double = FALSE, trim_ws = TRUE)

#Filtro para México
#------------------------------------------------
mx_surveillance <- variant_surveillance %>%
  filter(str_detect(Location,"Mexico")) %>%
  filter(`Is complete?`) %>%
  filter(!str_detect(Location,"New Mexico")) %>%
  filter(!is.na(Variant)) %>%
  mutate(`Collection date` = ymd(`Collection date`)) %>%
  filter(!is.na(`Collection date`)) %>%
  mutate(Semana = epiweek(`Collection date`)) %>%
  mutate(Año = year(`Collection date`)) %>%
  filter(Año <= year(today()))

#Función para procesamiento
plot_state <- function(mx_surveillance, plot_name, title_name, subtitle_name = ""){

  vcount <- mx_surveillance %>%
    mutate(Variant = if_else(str_detect(`Pango lineage`,"BA.2") & str_detect(Variant, "Omicron"), "Omicron BA.2", Variant)) %>%
    group_by(Variant, Semana, Año) %>%
    tally() %>%
    ungroup() %>%
    mutate(Variant = word(Variant, 1,2, sep = " ")) %>%
    mutate(fecha_proxy = ymd(paste0(Año,"/01/03")) + weeks(Semana)) %>%
    filter(fecha_proxy > ymd("2021-03-20"))
  
  vprop <- vcount %>%
    group_by(Semana, Año, fecha_proxy) %>%
    summarise(Total = sum(n), .groups = "keep") 
  
  vcount <- vcount %>% 
    left_join(vprop, by = c("fecha_proxy","Semana","Año")) %>%
    mutate(Prop = n/Total) %>%
    filter(fecha_proxy > ymd("2021-03-20") & year(fecha_proxy) <= year(today()))
  
  Sys.setlocale(locale="es_ES.UTF-8")
  variantplot <- ggplot(vcount) +
    geom_stream(aes(x = fecha_proxy, y = n, fill = Variant), type = "proportional", alpha = 1) +
    theme_minimal() +
    labs(
      x = "",
      y = "Proporción",
      title = paste0("Variantes del SARS-CoV-2 por semana epidemiológica en ", title_name, " [GISAID EpiFlu™ Database]"),
      subtitle = subtitle_name
    ) +
    scale_x_date(date_labels = "%B %y", date_breaks = "3 months",
                 date_minor_breaks = "1 month") +
    scale_fill_manual("Variante", values = met.brewer("Hiroshige", 12, "continuous"))
  ggsave(plot_name, variantplot, width = 10, height = 4, dpi = 750)
  
  return(variantplot)
}

#------------------------------------------------------------------------
#                                GRAFICAS 
#------------------------------------------------------------------------

#NACIONAL
#------------------------------------------------------------------------
plot_state(mx_surveillance, "images/Variantes_Nacional.png", "México")

#CIUDAD DE MÉXICO 
#------------------------------------------------------------------------
mx_surveillance %>%
  filter(str_detect(Location,"Mexico City|CDMX|CMX|Distrito Federal|Ciudad de Mexico|Mexico city")) %>%
  plot_state("images/Variantes_CDMX.png", "CDMX")

#NORTE
#------------------------------------------------------------------------
norte <- mx_surveillance %>%
  filter(str_detect(Location,"Baja California|Baja California Sur|Chihuahua|Coahuila|Sinaloa|Sonora|Durango|Nuevo Leon|Nuevo León|Tamaulipas|Mexicali|Ensenada|Tijuana|Hermosillo|Monterrey|Guasave|Ahome|Los Mochis|Saltillo|Torreon|Ciudad Juarez|")) %>%
  plot_state("images/Variantes_NORTE.png", "Región Norte", "BC, BCS, CHIH, COAH, SIN, SON, DGO, NL, TAM")

#CENTRO
#------------------------------------------------------------------------
centro <- mx_surveillance %>%
  filter(str_detect(Location,"Aguascalientes|Guanajuato|Querétaro|Queretaro|Zacatecas|San Luis Potosi|San Luis Potosí|Mexico City|CDMX|CMX|Distrito Federal|Ciudad de Mexico|Mexico city|Estado de México|Edomex|EDOMEX|Estado de mexico|Estado de Mexico|State of Mexico|Mexico / Mexico|State of mexico|Estado de méxico|Morelos")) %>%
  plot_state("images/Variantes_CENTRO.png", "Región Centro", "AGS, GRO, QRO, ZAC, SLP, CDMX, EDOMEX, MOR")

#SUR
#------------------------------------------------------------------------
sur <- mx_surveillance %>%
  filter(str_detect(Location,"Chiapas|Guerrero|Guerero|Oaxaca|Mérida|Campeche|Quintana Roo|Tabasco|Yucatán|Yucatan|Cancun")) %>%
  plot_state("images/Variantes_SUR.png", "Región Sur", "CHIS, GUE, OAX, QROO, CAM, TAB, YUC")

#ORTIENTE + OESTE
#------------------------------------------------------------------------
oriente_oeste <- mx_surveillance %>%
  filter(str_detect(Location,"Colima|Jalisco|Michoacan|Michoacán|Nayarit|Hidalgo|Puebla|Tlaxcala|Veracruz")) %>%
  plot_state("images/Variantes_Oriente_y_Oeste.png", "Regiones Oriente + Oeste", "COL, JAL, MICH, NAY, HGO, PUE, TLAX, VER")

sqplot <- plot_grid(
          norte  + ggtitle("NORTE") + theme(legend.position = "none"), 
          centro + ggtitle("CENTRO") + theme(legend.position = "none"), 
          sur    + ggtitle("SUR") + theme(legend.position = "none"), 
          oriente_oeste + ggtitle("ORIENTE Y OESTE") + theme(legend.position = "none"))

# extract the legend from one of the plots
legend <- get_legend(
  norte +  
    guides(fill = guide_legend(nrow = 2)) + 
    theme(legend.position = "bottom")
)

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(sqplot, legend, ncol = 1, rel_heights = c(1, 0.1))
ggsave("images/Regiones_variantes.png", width = 10, height = 8, dpi = 750)


#GRÁFICA DE BARRAS
if (!require(covidmx)){
  devtools::install_github("RodrigoZepeda/covidmx")
  library(covidmx)
}

if (require(covidmx)){
  covid        <- descarga_datos_abiertos()
  ambulatorios <- covid %>% casos(group_by_entidad = F,
                                  tipo_caso    = "Sospechosos y Confirmados COVID")
  
  ambulatorios <- ambulatorios %>%
    filter(FECHA_SINTOMAS <= max(ambulatorios$FECHA_SINTOMAS) - 7)
  
  ambulatorios <- ambulatorios %>%
    mutate(SEMANA = epiweek(FECHA_SINTOMAS)) %>%
    mutate(AÑO = year(FECHA_SINTOMAS)) %>%
    group_by(SEMANA, AÑO) %>%
    summarise(casos = sum(Casos_SOSPECHOSOS_Y_CONFIRMADOS_COVID)) %>%
    ungroup()
  
  ambulatorios <- ambulatorios %>%
    mutate(fecha_proxy = ymd(paste0(AÑO,"/01/03")) + weeks(SEMANA)) 
  
  vcount <- mx_surveillance %>%
    mutate(Variant = if_else(str_detect(`Pango lineage`,"BA.2") & str_detect(Variant, "Omicron"), "Omicron BA.2", Variant)) %>%
    group_by(Variant, Semana, Año) %>%
    tally() %>%
    ungroup() %>%
    mutate(Variant = word(Variant, 1,2, sep = " ")) %>%
    mutate(fecha_proxy = ymd(paste0(Año,"/01/03")) + weeks(Semana)) %>%
    filter(fecha_proxy > ymd("2021-03-20"))
  
  vprop <- vcount %>%
    group_by(Semana, Año, fecha_proxy) %>%
    summarise(Total = sum(n), .groups = "keep") 
  
  vcount <- vcount %>% 
    left_join(vprop, by = c("fecha_proxy","Semana","Año")) %>%
    mutate(Prop = n/Total) %>%
    filter(fecha_proxy > ymd("2021-03-20") & year(fecha_proxy) <= year(today()))
  
  vcount <- vcount %>%
    left_join(ambulatorios, 
              by = c("fecha_proxy", "Año" = "AÑO", "Semana" = "SEMANA"))
  
  vcount <- vcount %>%
    mutate(Casos_variante = casos*Prop)
  
  ggplot(vcount) +
    geom_col(aes(x = fecha_proxy, y = Casos_variante, fill = Variant)) +
    theme_classic() +
    labs(
      y = "Casos de enfermedad respiratoria",
      x = "Fecha",
      title = "Casos de enfermedad respiratoria en México (2021-2022)",
      subtitle = "Datos Abiertos COVID-19"
    ) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_date(date_labels = "%B-%y") +
    scale_fill_manual("Variant", values = met.brewer("Hiroshige", 12, "continuous")) 
  ggsave("images/barplot.png", width = 8, height = 4, dpi = 750)
} else {
  warning("No realizamos la gráfica de barras pues no cuentas con covidmx. ¡Descárgalo!")
}

#Delete downloaded file
file.remove(fname)

