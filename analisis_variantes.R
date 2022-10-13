#DATOS DE https://www.epicov.org/epi3/frontend#62aa9e
#VARIANT SURVEILLANCE
#https://github.com/guillermodeandajauregui/covid19mx-sql/blob/main/data_loader.R
rm(list = ls())

#remotes::install_github("covidmx")
pacman::p_load(tidyverse, lubridate, ggstream, MetBrewer, ggtext, latexpdf, cowplot, 
               DBI, duckdb, glue, reticulate, covidmx, cli)

flag     <- FALSE
attempts <- 10 #Intentos de descarga

#Environment de conda
#Install pangolin in a different conda env as the other GISAID stuff is incompatible
if (Sys.info()["user"] == "rod"){
  conda_path <- "/usr/local/Caskroom/miniconda/base/envs/GISAID/bin/python3"
  conda_path_pango <- "/usr/local/Caskroom/miniconda/base/envs/pangolin/bin/python3"
} else if (Sys.info()["user"] == "rodrigo") {
  conda_path       <- "/home/rodrigo/miniconda3/envs/GISAID/bin/python3"
  conda_path_pango <- "/home/rodrigo/miniconda3/envs/pangolin/bin/python3"
} else {
  conda_path <- conda_list(conda = "auto")[1,2]
  conda_path_pango <- conda_path
  cli::cli_alert_danger("Usando el conda default de {conda_path}")
}

#Lectura de la base ------------------------------------------------

#Get latest tar file
tarfiles <- list.files(pattern = "variant_surveillance_tsv.*.tar", full.names = T)
df       <- file.info(tarfiles)
fname    <- rownames(df)[which.max(df$mtime)]
cli::cli_alert_info("Usando el archivo {fname}")

tsv_name <- untar(fname, list = TRUE)
tsv_name <- tsv_name[which(str_detect(tsv_name,".tsv"))]
untar(fname, as.character(tsv_name))

# Get file connection
cli::cli_alert_info("Creando conexión a duckdb")
con <- duckdb::dbConnect(
  drv   = duckdb::duckdb(),
  dbdir = "/media/rodrigo/covid/datos_variantes.duckdb",
)

DBI::dbExecute(con, "PRAGMA memory_limit = '1GB'")
duckdb_read_csv(conn = con, name = "variant_surveillance", 
                files = "variant_surveillance.tsv", sep="\t")

cli::cli_alert_info("Leyendo tabla de duckdb")
variant_surveillance <- tbl(con, "variant_surveillance")

#Tests if works
#res <- dbSendQuery(conn = con, "SELECT * FROM variant_surveillance LIMIT 10")
#dbFetch(res)
#dbClearResult(res)

#Filtro para Mexico-----------------------------------------------
mx_surveillance <- variant_surveillance %>%
  filter(str_detect(Location,"Mexico")) %>%
  filter(!str_detect(Location,"New Mexico")) %>%
  filter(`Is.complete.` == "True") %>%
  mutate(across(everything(), ~ str_remove_all(. , "\\0"))) %>%
  collect() %>%
  mutate(Collection.date = ymd(Collection.date)) %>%
  filter(Collection.date <= today()) %>%
  mutate(Semana = epiweek(Collection.date)) %>%
  mutate(Año = epiyear(Collection.date)) %>%
  filter(!is.na(Collection.date)) 

mx_surveillance %>%
  write_excel_csv("variantes_mx.csv")

dbDisconnect(con)

#Agregamos los que no tienen PANGO pero ya calculamos
recovered_pango <- read_csv("Pango_recovered.csv", show_col_types = FALSE) 

binded_pango <- recovered_pango %>%
  rename(`Accession.ID` = index) %>%
  mutate(`Accession.ID` = str_remove_all(`Accession.ID`,"fasta_processed//|.csv")) %>%
  rename(Variant = scorpio_call) %>%
  rename(`Pango.lineage` = lineage) %>%
  filter(!is.na(Variant)) %>%
  select(`Accession.ID`, `Pango.lineage`)
  
mx_surveillance <- read_csv("variantes_mx.csv", show_col_types = F) %>%
  filter(Variant != "") %>%
  mutate(`Pango.lineage` = if_else(`Pango.lineage` == "Unassigned", NA_character_, `Pango.lineage`)) %>%
  left_join(binded_pango, by = "Accession.ID") %>%
  mutate(`Pango.lineage` = if_else(!is.na(`Pango.lineage.x`), `Pango.lineage.x`, `Pango.lineage.y`)) %>%
  select(-`Pango.lineage.x`, -`Pango.lineage.y`) %>%
  mutate(Collection.date <= today()) 

#Creamos la base de datos de los id para descargar y buscarles su linaje
unassigned <- mx_surveillance %>%
  filter(is.na(`Pango.lineage`)) %>%
  select(`Accession.ID`) %>%
  write_excel_csv("Unassigned.csv")

while (attempts > 0 & (nrow(unassigned) > 0 | length(list.files("fasta")) > 0)){
  
  if (nrow(unassigned) > 0){
    #Call python to look for them in GISAID
    system2(conda_path, "download_fasta.py")
  }
  
  #Process the downloaded FASTA files
  fastas <- list.files("fasta", pattern = "EPI.*fasta")
  if (length(fastas) > 0){
    system2("bash", "get_pangolin.sh")
  }
  
  #Now read the processed fasta files and delete from 'fasta' and move to `Pango_recovered.csv`
  fasta_files <- read_csv(list.files("fasta_processed/", "*.csv", full.names = T), id="index")

  #Add to list of recovered
  recovered_pango <- recovered_pango %>%
    full_join(fasta_files) %>%
    distinct()
  
  #Save
  recovered_pango %>% write_excel_csv("Pango_recovered.csv")
  
  for (id in recovered_pango$index){
    if (file.exists(id)){
      file.remove(id)
    }
    id <- str_remove_all(id,"fasta_processed//|.csv")
    if (file.exists(glue("fasta/{id}.fasta"))){
      file.remove(glue("fasta/{id}.fasta"))
    }
  }
  
  
  binded_pango <- recovered_pango %>%
    rename(`Accession.ID` = index) %>%
    mutate(`Accession.ID` = str_remove_all(`Accession.ID`,"fasta_processed//|.csv")) %>%
    rename(Variant = scorpio_call) %>%
    rename(`Pango.lineage` = lineage) %>%
    filter(!is.na(Variant)) %>%
    select(`Accession.ID`, `Pango.lineage`)
  
  mx_surveillance <- read_csv("variantes_mx.csv") %>%
    filter(Variant != "") %>%
    mutate(`Pango.lineage` = if_else(`Pango.lineage` == "Unassigned", NA_character_, `Pango.lineage`)) %>%
    left_join(binded_pango, by = "Accession ID") %>%
    mutate(`Pango.lineage` = if_else(!is.na(`Pango.lineage.x`), `Pango.lineage.x`, `Pango.lineage.y`)) %>%
    select(-`Pango.lineage.x`, -`Pango.lineage.y`)
 
  unassigned <- mx_surveillance %>%
    filter(is.na(`Pango.lineage`)) %>%
    select(`Accession.ID`) %>%
    write_excel_csv("Unassigned.csv")
 
  attempts <- attempts - 1 
}

#Re-fit
mx_surveillance <- mx_surveillance %>%
  mutate(`Pango.lineage` = if_else(is.na(`Pango.lineage`), "Unassigned", `Pango.lineage`)) %>%
  mutate(Variant = if_else(str_detect(Variant, "Omicron"),
                           paste0("Omicron ", str_sub(`Pango.lineage`,1,5)), Variant)) %>%
  mutate(Variant = if_else(str_detect(Variant, "Omicron") &
                             str_detect(`Pango.lineage`,"Unassigned"),"Omicron (sin_asignar)",
                           Variant)) %>%
  mutate(Variant = word(Variant, 1,2, sep = " ")) %>%
  mutate(Variant = case_when(
    str_detect(Pango.lineage, "BA.5.2.1.7|BF.7") ~ "Omicron BF.7",
    str_detect(Pango.lineage, "XBB")     ~ "Omicron XBB",
    str_detect(Pango.lineage, "BA.5")    ~ "Omicron BA.5",
    str_detect(Pango.lineage, "BA.2.75") ~ "Omicron BA.2.75",
    str_detect(Pango.lineage, "BA.2")    ~ "Omicron BA.2",
    str_detect(Pango.lineage, "BA.1")    ~ "Omicron BA.1",
    str_detect(Pango.lineage, "BA.4")    ~ "Omicron BA.4",
    str_detect(Pango.lineage, "BN.1")    ~ "Omicron BN.1",
    str_detect(Variant, "Omicron BG|Omicron X|Omicron AY|Omicron B.1.1|Omicron BE|Omicron|Omicron BF|sin_asignar") ~ "Omicron (otros)",
    TRUE ~ Variant
  )) %>%
  filter(!is.na(Variant))

#Remove from fasta and fasta processed if now they have a match

#Datos para publicar
mx_surveillance %>%
  group_by(Variant, Semana, Año) %>%
  summarise(n = n(), .groups = "keep") %>%
  group_by(Semana, Año) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup() %>%
  arrange(Año, Semana, freq) %>%
  janitor::clean_names() %>%
  mutate(`Actualizacion` = Sys.time())  %>%
  mutate(`Fuente` = "GISAID: https://www.gisaid.org/") %>%
  write_excel_csv("tablas/Proporcion_variantes_nacional.csv")

mx_surveillance %>%
  filter(str_detect(Location,paste0("Mexico City|CDMX|CMX|Distrito Federal",
                                    "|Ciudad de Mexico|Mexico city"))) %>%
  group_by(Variant, Semana, Año) %>%
  summarise(n = n(), .groups = "keep") %>%
  group_by(Semana, Año) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup() %>%
  arrange(Año, Semana, freq) %>%
  janitor::clean_names() %>%
  mutate(`Actualizacion` = Sys.time())  %>%
  mutate(`Fuente` = "GISAID: https://www.gisaid.org/") %>%
  write_excel_csv("tablas/Proporcion_variantes_cdmx.csv")

mx_surveillance <- mx_surveillance %>%
  mutate(fecha_proxy = ymd(paste0(Año,"/01/03")) + weeks(Semana)) %>%
  filter(fecha_proxy > today() - years(1))

variantes <- unique(mx_surveillance$Variant)
fechas    <- unique(mx_surveillance$Collection.date)

#Función para procesamiento
plot_state <- function(mx_surveillance, plot_name, title_name, subtitle_name = "",
                       variantes = variantes, fechas = fechas){

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

  #Distribución actual
  dactual            <- vcount %>%
    filter(fecha_proxy == max(fecha_proxy)) %>%
    arrange(desc(Prop))
  
  variantes_actuales <- ""
  for (i in 1:nrow(dactual)){
    variantes_actuales <- paste0(variantes_actuales,
                                 glue("{dactual[i,'Variant']}: {scales::percent(dactual[i,'Prop'][[1]])} (n = {dactual[i,'n'][[1]]})"),
                                 "\n")
  }

  #Sys.setlocale(locale="es_ES.UTF-8")
  colores        <- met.brewer("Hiroshige", length(variantes), "continuous")
  names(colores) <- sort(variantes)
  Sys.setlocale("LC_ALL",'es_MX.UTF-8')
  
  vcount2 <- vcount %>% 
    filter(fecha_proxy == max(fecha_proxy)) %>%
    mutate(fecha_proxy = fecha_proxy + weeks(1)) %>%
    bind_rows(vcount)
  
  variantplot <- ggplot(vcount2) +
    geom_stream(aes(x = fecha_proxy, y = n, fill = Variant), type = "proportional", alpha = 1,
                bw = 0.75) +
    labs(
      x = "",
      y = "Porcentaje de casos registrados",
      title = title_name,
      subtitle = subtitle_name,
      caption  = glue("**Fuente:** GISAID EpiCoV Database. ",
                      "| **Github**: RodrigoZepeda/VariantesCovid | ",
                        "Gráfica elaborada el {today()} usando datos hasta el {max(vcount$fecha_proxy)}.")
    ) +
    annotate("label", x = today() - years(1) + days(14), y = 0.95, hjust = 0, vjust = 1, alpha = 0.75,
                  fill = "white", size = 2.75,
             label = glue("Distribución actual:\n",
                          "--------------------\n",
                          variantes_actuales)) +
    scale_x_date(date_labels = "%B %y", date_breaks = "1 month", expand = c(0,0)) +
    scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
    scale_fill_manual("Variante/Subvariante", values = colores) +
    theme(panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white", color = "white"),
          axis.text.x = element_text(angle = 45, size = 10, hjust = 1),
          legend.position = "bottom",
          panel.border = element_blank(),
          plot.title = element_markdown(size = 20, hjust = 0.5),
          plot.subtitle = element_markdown(size = 12, hjust = 0.5),
          plot.caption = element_markdown(),
          axis.line = element_blank(),
          axis.line.y.left = element_line()) 
  ggsave(plot_name, variantplot, width = 10, height = 6, dpi = 750, bg = "white")
  ggsave(str_replace_all(plot_name,".png",".pdf"), variantplot, width = 10, height = 6, dpi = 750, bg = "white")

  return(variantplot)
}

#------------------------------------------------------------------------
#                                GRAFICAS
#------------------------------------------------------------------------

#NACIONAL
#------------------------------------------------------------------------
nacional <- plot_state(mx_surveillance, "images/Variantes_Nacional.png",
           "Variantes de <span style='color:#006400'>SARS-CoV-2</span> en México",
           "_Proporción de variantes a nivel nacional_", variantes =  variantes, fechas)

#CIUDAD DE MÉXICO
#------------------------------------------------------------------------
cdmx <- mx_surveillance %>%
  filter(str_detect(Location,
                    paste0("Mexico City|CDMX|CMX|Distrito Federal",
                           "|Ciudad de Mexico|Mexico city"))) %>%
  plot_state("images/Variantes_CDMX.png",
             "Variantes de <span style='color:#006400'>SARS-CoV-2</span> en Región CDMX",
             "_Proporción de variantes en Ciudad de México_", variantes = variantes)

#NORTE
#------------------------------------------------------------------------
norte <- mx_surveillance %>%
  filter(str_detect(Location,
                    paste0("Baja California|Baja California Sur|Chihuahua|",
                           "Coahuila|Sinaloa|Sonora|Durango|Nuevo Leon|",
                           "Nuevo León|Tamaulipas|Mexicali|Ensenada|Tijuana|",
                           "Hermosillo|Monterrey|Guasave|Ahome|Los Mochis|",
                           "Saltillo|Torreon|Ciudad Juarez|"))) %>%
  plot_state("images/Variantes_NORTE.png",
             "Variantes de <span style='color:#006400'>SARS-CoV-2</span> en Región Norte",
             "_Proporción de variantes en BC, BCS, CHIH, COAH, SIN, SON, DGO, NL, TAM_", variantes)

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
  plot_state("images/Variantes_CENTRO.png",
             "Variantes de <span style='color:#006400'>SARS-CoV-2</span> en Región Centro",
             "_Proporción de variantes en AGS, GRO, QRO, ZAC, SLP, CDMX, EDOMEX, MOR_", variantes)

#SUR
#------------------------------------------------------------------------
sur <- mx_surveillance %>%
  filter(str_detect(Location,
                    paste0("Chiapas|Guerrero|Guerero|Oaxaca|Mérida|Campeche|",
                           "Quintana Roo|Tabasco|Yucatán|Yucatan|Cancun"))) %>%
  plot_state("images/Variantes_SUR.png",
             "Variantes de <span style='color:#006400'>SARS-CoV-2</span> en Región Sur",
             "_Proporción de variantes en  CHIS, GUE, OAX, QROO, CAM, TAB, YUC_", variantes)

#ORTIENTE + OESTE
#------------------------------------------------------------------------
oriente_oeste <- mx_surveillance %>%
  filter(str_detect(Location,
                    paste0("Colima|Jalisco|Michoacan|Michoacán|Nayarit|",
                           "Hidalgo|Puebla|Tlaxcala|Veracruz"))) %>%
  plot_state("images/Variantes_Oriente_y_Oeste.png",
             "Variantes de <span style='color:#006400'>SARS-CoV-2</span> en Regiones Oriente + Oeste",
             "_Proporción de variantes en COL, JAL, MICH, NAY, HGO, PUE, TLAX, VER_", variantes)

mx_surveillance <- mx_surveillance %>%
  mutate(fecha_proxy = ymd(paste0(Año,"/01/03")) + weeks(Semana)) %>%
  filter(fecha_proxy > ymd("2021-03-20")) %>%
  filter(fecha_proxy <= today())

sqplot <- plot_grid(
          norte  + ggtitle("NORTE")  + theme(legend.position = "none") +
            labs(caption = "", y = "",
                 subtitle = "_BC, BCS, CHIH, COAH, SIN, SON, DGO, NL, TAM_") +
            theme(axis.text.x = element_text(color = "white", angle = 0),
                  axis.ticks.x  = element_line(color = "white")) +
            coord_cartesian(xlim = c(min(mx_surveillance$fecha_proxy), max(mx_surveillance$fecha_proxy))),
          centro + ggtitle("CENTRO") + theme(legend.position = "none") +
            labs(caption = "", y = "",
                 subtitle = "_AGS, GRO, QRO, ZAC, SLP, CDMX, EDOMEX, MOR_") +
            theme(axis.text.x = element_text(color = "white", angle = 0),
                  axis.ticks.x  =element_line(color = "white")) +
            coord_cartesian(xlim = c(min(mx_surveillance$fecha_proxy), max(mx_surveillance$fecha_proxy))),
          sur    + ggtitle("SUR")    + theme(legend.position = "none") +
            labs(caption = "", y = "",
                 subtitle = "_CHIS, GUE, OAX, QROO, CAM, TAB, YUC_") +
            coord_cartesian(xlim = c(min(mx_surveillance$fecha_proxy), max(mx_surveillance$fecha_proxy))),
          oriente_oeste + ggtitle("ORIENTE Y OESTE") +
            theme(legend.position = "none") +
            labs(caption = "", y = "",
                 subtitle = "_COL, JAL, MICH, NAY, HGO, PUE, TLAX, VER_") +
            coord_cartesian(xlim = c(min(mx_surveillance$fecha_proxy), max(mx_surveillance$fecha_proxy))),
          rel_heights = c(0.85, 1))

# extract the legend from one of the plots
legend <- get_legend(
  norte +
    guides(fill = guide_legend(nrow = 2, title.position = "top")) +
    theme(legend.position = "bottom")
)

# add the legend
plot_grid(nacional + theme(legend.position = "none",
                           plot.subtitle   = element_markdown(size = 10, hjust = 0.5, 
                                                              margin = margin(0,0,0,0)),
                           plot.title = element_markdown(size = 30)) + ylab("") +
            labs(
              title = "Variantes de <span style='color:#006400'>SARS-CoV-2</span> en México",
              caption = "",
              subtitle = glue("**Fuente:** GISAID EpiCoV Database |",
                              " **Github:** RodrigoZepeda/VariantesCovid | ",
                   "Elaborada el {today()}.")
            )
            , sqplot, legend, ncol = 1, rel_heights = c(1, 1, 0.1))
ggsave("images/Regiones_variantes.pdf", width = 12, height = 14, dpi = 750,
       bg = "white")

if (require(ghostpdf)){
  #There was an issue with ggsave to png this solves it
  ghostpdf::pdf_to_image("images/Regiones_variantes.pdf", output_file = "images/Regiones_variantes.png")
}


#Delete downloaded file
file.remove(tarfiles)
file.remove(tsv_name)

cli::cli_alert_success("Procesamiento terminado")
