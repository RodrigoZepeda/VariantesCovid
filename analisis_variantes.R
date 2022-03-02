#DATOS DE https://www.epicov.org/epi3/frontend#62aa9e
#VARIANT SURVEILLANCE

rm(list = ls())
library(tidyverse)
library(lubridate)
library(ggstream)
library(MetBrewer)
library(latexpdf)
library(covidmx)

setwd("~/Dropbox/GISAID VARIANTES")
variant_surveillance <- read_delim("variant_surveillance.tsv",
                                   delim = "\t", escape_double = FALSE,
                                   trim_ws = TRUE)

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

vcount <- mx_surveillance %>%
  group_by(Variant, Semana, Año) %>%
  tally() %>%
  ungroup() %>%
  mutate(Variant = word(Variant, 1,2, sep = " ")) %>%
  mutate(fecha_proxy = ymd(paste0(Año,"/01/03")) + weeks(Semana)) %>%
  filter(fecha_proxy > ymd("2021-03-20"))

vprop <- vcount %>%
  group_by(Semana, Año, fecha_proxy) %>%
  summarise(Total = sum(n)) 

vcount <- vcount %>% 
  left_join(vprop, by = c("fecha_proxy","Semana","Año")) %>%
  mutate(Prop = n/Total) %>%
  filter(fecha_proxy > ymd("2021-03-20") & year(fecha_proxy) <= year(today()))

Sys.setlocale(locale="es_ES.UTF-8")
ggplot(vcount) +
  geom_stream(aes(x = fecha_proxy, y = n, fill = Variant), type = "proportional", alpha = 1) +
  theme_minimal() +
  labs(
    x = "",
    y = "Proporción",
    title = "Variantes del SARS-CoV-2 por semana epidemiológica en México",
    caption = "Fuente: GISAID EpiFlu™ Database en https://www.gisaid.org/"
  ) +
  scale_x_date(date_labels = "%B %y", date_breaks = "3 months",
               date_minor_breaks = "1 month") +
  scale_fill_manual("Variante", values = met.brewer("Hiroshige", 11, "continuous"))
ggsave("Variantes_MX.png", width = 8, height = 4, dpi = 750)


vcount <- mx_surveillance %>%
  filter(str_detect(Location,"Mexico City|CDMX|CMX|Distrito Federal|Ciudad de Mexico")) %>%
  group_by(Variant, Semana, Año) %>%
  tally() %>%
  mutate(Variant = word(Variant, 1,2, sep = " "))

Sys.setlocale(locale="es_ES.UTF-8")
ggplot(vcount) +
  geom_stream(aes(x = ymd(paste0(Año,"/01/03")) + weeks(Semana),
                  y = n, fill = Variant),
              type = "proportional", alpha = 1) +
  theme_minimal() +
  labs(
    x = "",
    y = "Proporción",
    title = "Variantes del SARS-CoV-2 por semana epidemiológica en Ciudad de México",
    caption = "Fuente: GISAID EpiFlu™ Database en https://www.gisaid.org/"
  ) +
  scale_x_date(date_labels = "%B %y", date_breaks = "3 months",
               date_minor_breaks = "1 month") +
  scale_fill_manual("Variante", values = met.brewer("Hiroshige", 11, "continuous"))
ggsave("Variantes_CDMX.png", width = 8, height = 4, dpi = 750)

covid        <- descarga_datos_abiertos()
ambulatorios <- covid %>% casos(group_by_entidad = F,
                                tipo_caso    = "Sospechosos y Confirmados COVID")

ambulatorios <- ambulatorios %>%
  filter(FECHA_SINTOMAS <= max(ambulatorios$FECHA_SINTOMAS) - 7)

ambulatorios <- ambulatorios %>%
  mutate(SEMANA = epiweek(FECHA_SINTOMAS)) %>%
  mutate(AÑO = year(FECHA_SINTOMAS)) %>%
  group_by(SEMANA, AÑO) %>%
  summarise(casos = sum(Casos_TODOS)) %>%
  ungroup()

ambulatorios <- ambulatorios %>%
  mutate(fecha_proxy = ymd(paste0(AÑO,"/01/03")) + weeks(SEMANA)) 

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
  scale_fill_manual("Variant", values = met.brewer("Hiroshige", 11, "continuous")) 


