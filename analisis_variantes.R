#DATOS DE https://www.epicov.org/epi3/frontend#62aa9e
#VARIANT SURVEILLANCE
#https://github.com/guillermodeandajauregui/covid19mx-sql/blob/main/data_loader.R
rm(list = ls())

library(covidmx)
library(tidyverse)
library(lubridate)
library(ggstream)
library(MetBrewer)
library(latexpdf)
library(cowplot)
library(DBI)
library(odbc)
library(RMariaDB)
library(glue)

flag <- FALSE

#Lectura de la base
#------------------------------------------------
fname                <- list.files(pattern = "*.tar", full.names = T)
tsv_name             <- untar(fname, list = TRUE)
tsv_name             <- tsv_name[which(str_detect(tsv_name,".tsv"))]
untar(fname, as.character(tsv_name))

#Creamos el MARIADB
#------------------------------------------------
con    <- dbConnect(RMariaDB::MariaDB(),
                    user     = Sys.getenv("MariaDB_user"),
                    password = Sys.getenv("MariaDB_password"),
                    dbname = "COVID")

header <- read_delim(tsv_name, delim = "\t", n_max = 100, escape_double = FALSE,
                     trim_ws = TRUE)

dbSendStatement(conn = con, statement = "SET sql_mode = 'NO_ENGINE_SUBSTITUTION,NO_AUTO_CREATE_USER';")
dbWriteTable(conn = con, name = "variant_surveillance", value = header,
                    overwrite = T)
dbSendStatement(conn = con, statement = "DELETE FROM variant_surveillance;")

for (colname in c("Clade","`Pango lineage`","`AA Substitutions`","Location","Type","`Accession ID`","Variant","Host")){
  longtext <- glue("ALTER TABLE variant_surveillance MODIFY {colname} TEXT;")
  dbSendStatement(conn = con, statement = longtext)
}

logicols <- sapply(header, typeof)
for (colname in names(logicols[which(logicols == "logical")])){
  longtext <- glue("ALTER TABLE variant_surveillance MODIFY \`{colname}\` TEXT;")
  dbSendStatement(conn = con, statement = longtext)
}

mi_query <- glue("LOAD DATA LOCAL INFILE \'{tsv_name}\' ",
                 "REPLACE INTO TABLE variant_surveillance ",
                 "CHARACTER SET UTF8 ",
                 "COLUMNS TERMINATED BY '\\t' ",
                 "LINES TERMINATED BY '\\n' ",
                 "IGNORE 1 LINES;")
dbSendStatement(conn = con, statement = mi_query)

variant_surveillance <- tbl(con, "variant_surveillance")

#Tets if works
#res <- dbSendQuery(conn = con, "SELECT * FROM variant_surveillance LIMIT 10")
#dbFetch(res)
#dbClearResult(res)

#Filtro para México
#------------------------------------------------
mx_surveillance <- variant_surveillance %>%
  filter(str_detect(Location,"Mexico")) %>%
  filter(!str_detect(Location,"New Mexico")) %>%
  filter(`Is complete?` == "True") %>%
  filter(Variant != "") %>%
  collapse() %>%
  as.data.frame %>%
  mutate(`Collection date` = ymd(`Collection date`)) %>%
  filter(!is.na(`Collection date`)) %>%
  filter(`Collection date` <= today()) %>%
  mutate(Semana = epiweek(`Collection date`)) %>%
  mutate(Año = year(`Collection date`)) %>%
  mutate(Variant = if_else(str_detect(`Pango lineage`,"BA.2") & str_detect(Variant, "Omicron"), "Omicron BA.2", Variant)) %>%
  mutate(Variant = word(Variant, 1,2, sep = " "))

mx_surveillance %>% write_excel_csv("variantes_mx.csv")

variantes <- unique(mx_surveillance$Variant)
fechas    <- unique(mx_surveillance$`Collection date`)

#Función para procesamiento
plot_state <- function(mx_surveillance, plot_name, title_name, subtitle_name = "", variantes = variantes, fechas = fechas){

  vcount <- mx_surveillance %>%
    group_by(Variant, Semana, Año) %>%
    tally() %>%
    ungroup() %>%
    mutate(fecha_proxy = ymd(paste0(Año,"/01/03")) + weeks(Semana)) %>%
    filter(fecha_proxy > ymd("2021-03-20")) %>%
    mutate(fecha_proxy = if_else(year(fecha_proxy) > Año, fecha_proxy - years(1),
                                 fecha_proxy))

  vprop <- vcount %>%
    group_by(Semana, Año, fecha_proxy) %>%
    summarise(Total = sum(n), .groups = "keep")

  maxcount        <- -Inf
  semana_reciente <- today() + 1
  while (maxcount < 20){
    message(paste0("Fecha: ", semana_reciente))
    vcount <- vcount %>%
      filter(fecha_proxy < !!semana_reciente)

    semana_reciente <- max(vcount$fecha_proxy)
    maxcount        <- vcount %>%
      filter(fecha_proxy == !!semana_reciente) %>%
      summarise(n = sum(n)) %>% unlist() %>% as.numeric()
  }

  vcount <- vcount %>%
    left_join(vprop, by = c("fecha_proxy","Semana","Año")) %>%
    mutate(Prop = n/Total) %>%
    filter(fecha_proxy > ymd("2021-03-20") & year(fecha_proxy) <= year(today()))

  Sys.setlocale(locale="es_ES.UTF-8")
  colores        <- met.brewer("Hiroshige", length(variantes), "continuous")
  names(colores) <- sort(variantes)
  variantplot <- ggplot(vcount) +
    geom_stream(aes(x = fecha_proxy, y = n, fill = Variant), type = "proportional", alpha = 1) +
    theme_minimal() +
    labs(
      x = "",
      y = "Proporción",
      title = paste0("Variantes del SARS-CoV-2 por semana epidemiológica en ", title_name, " [GISAID EpiFlu™ Database]"),
      subtitle = subtitle_name,
      caption  = paste0("Última recolección de datos con n ≥ 20: ", max(vcount$fecha_proxy),"\nGráfica elaborada el ", today())
    ) +
    scale_x_date(date_labels = "%B %y", date_breaks = "3 months",
                 date_minor_breaks = "1 month") +
    scale_fill_manual("Variante", values = colores) +
    theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white", color = "white"),
          axis.text.x = element_text(angle = 45, size = 10, hjust = 1))
  ggsave(plot_name, variantplot, width = 10, height = 4, dpi = 750, bg = "white")

  return(variantplot)
}

#------------------------------------------------------------------------
#                                GRAFICAS
#------------------------------------------------------------------------

#NACIONAL
#------------------------------------------------------------------------
plot_state(mx_surveillance, "images/Variantes_Nacional.png", "México", variantes =  variantes, fechas)

#CIUDAD DE MÉXICO
#------------------------------------------------------------------------
mx_surveillance %>%
  filter(str_detect(Location,
                    paste0("Mexico City|CDMX|CMX|Distrito Federal",
                           "|Ciudad de Mexico|Mexico city"))) %>%
  plot_state("images/Variantes_CDMX.png", "CDMX", variantes = variantes)

#NORTE
#------------------------------------------------------------------------
norte <- mx_surveillance %>%
  filter(str_detect(Location,
                    paste0("Baja California|Baja California Sur|Chihuahua|",
                           "Coahuila|Sinaloa|Sonora|Durango|Nuevo Leon|",
                           "Nuevo León|Tamaulipas|Mexicali|Ensenada|Tijuana|",
                           "Hermosillo|Monterrey|Guasave|Ahome|Los Mochis|",
                           "Saltillo|Torreon|Ciudad Juarez|"))) %>%
  plot_state("images/Variantes_NORTE.png", "Región Norte",
             "BC, BCS, CHIH, COAH, SIN, SON, DGO, NL, TAM", variantes)

#CENTRO
#------------------------------------------------------------------------
centro <- mx_surveillance %>%
  filter(str_detect(Location,
                    paste0("Aguascalientes|Guanajuato|Querétaro|Queretaro|",
                           "Zacatecas|San Luis Potosi|San Luis Potosí|",
                           "Mexico City|CDMX|CMX|Distrito Federal|",
                           "Ciudad de Mexico|Mexico city|Estado de México|",
                           "Edomex|EDOMEX|Estado de mexico|Estado de Mexico|",
                           "State of Mexico|Mexico / Mexico|State of mexico|",
                           "Estado de méxico|Morelos"))) %>%
  plot_state("images/Variantes_CENTRO.png", "Región Centro",
             "AGS, GRO, QRO, ZAC, SLP, CDMX, EDOMEX, MOR", variantes)

#SUR
#------------------------------------------------------------------------
sur <- mx_surveillance %>%
  filter(str_detect(Location,
                    paste0("Chiapas|Guerrero|Guerero|Oaxaca|Mérida|Campeche|",
                           "Quintana Roo|Tabasco|Yucatán|Yucatan|Cancun"))) %>%
  plot_state("images/Variantes_SUR.png", "Región Sur",
             "CHIS, GUE, OAX, QROO, CAM, TAB, YUC", variantes)

#ORTIENTE + OESTE
#------------------------------------------------------------------------
oriente_oeste <- mx_surveillance %>%
  filter(str_detect(Location,
                    paste0("Colima|Jalisco|Michoacan|Michoacán|Nayarit|",
                           "Hidalgo|Puebla|Tlaxcala|Veracruz"))) %>%
  plot_state("images/Variantes_Oriente_y_Oeste.png", "Regiones Oriente + Oeste",
             "COL, JAL, MICH, NAY, HGO, PUE, TLAX, VER", variantes)

sqplot <- plot_grid(
          norte  + ggtitle("NORTE")  + theme(legend.position = "none"),
          centro + ggtitle("CENTRO") + theme(legend.position = "none"),
          sur    + ggtitle("SUR")    + theme(legend.position = "none"),
          oriente_oeste + ggtitle("ORIENTE Y OESTE") +
            theme(legend.position = "none"))

# extract the legend from one of the plots
legend <- get_legend(
  norte +
    guides(fill = guide_legend(nrow = 2)) +
    theme(legend.position = "bottom")
)

# add the legend
plot_grid(sqplot, legend, ncol = 1, rel_heights = c(1, 0.1))
ggsave("images/Regiones_variantes.png", width = 10, height = 8, dpi = 750,
       bg = "white")


#GRÁFICA DE BARRAS
if (!require(covidmx)){
  devtools::install_github("RodrigoZepeda/covidmx")
  library(covidmx)
}

if (require(covidmx) & flag){
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
file.remove(tsv_name)
