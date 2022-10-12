# VariantesCOVID

## Imágenes

![Variantes por región en México dividido en Norte, Sur, Centro y la combinación de Oriente + Oeste](images/Regiones_variantes.png)

### Nacional

![Variantes a nivel nacional](images/Variantes_Nacional.png)

### CDMX

![Variantes en Ciudad de México](images/Variantes_CDMX.png)

### Norte

![Variantes en región norte](images/Variantes_NORTE.png)

### Oriente y Oeste

![Variantes en regiones oriente y oeste](images/Variantes_Oriente_y_Oeste.png)

### Sur 

![Variantes a en región del sur](images/Variantes_SUR.png)

### Sur 

![Variantes a en región del centro](images/Variantes_SUR.png)

## Descripción 
Publicación diaria de las variantes en México usando datos de [GISAID](https://www.gisaid.org/).
+ Tablas en la carpeta `tablas`
+ Gráficas en la carpeta `images`

> **Nota** Si usas los datos debes citar a GISAID (ver referencias)

## Paquete covidmx en rstats

Para analizar en `R` los datos más actuales de esta publicación instala el paquete [`covidmx`](https://github.com/RodrigoZepeda/covidmx):

```{r}
#remotes::install_github("RodrigoZepeda/covidmx")
variantes <- covidmx::descarga_datos_variantes_GISAID()
```

> **Nota** Si usas los datos del paquete debes citar a GISAID (ver referencias) así como el paquete [`covidmx`](https://github.com/RodrigoZepeda/covidmx). 
 

## Datos

Los datos necesitas obtenerlos de [GISAID](https://www.gisaid.org/) yendo a `Downloads > Variant Surveillance`. 

Alternativamente puedes usar el scrapper `download_gisaid.py` con tu password y tu usuario. No es un producto oficial y úsalo bajo tu riesgo pues desconozco si va en contra de los términos y condiciones de GISAID.

## Automatización

Para Linux puedes usar `crontab` para automatizar la descarga de la base de datos. Por ahora necesitas `mariadb` para que `R` los transforme. Sugerencia: 

1. Crea tu crontab parecido a este:

```{bash} 
57 14 * * * export DISPLAY=:0 && /bin/sh /home/rodrigo/VariantesCovid/orchestrate.sh > /dev/null 2>&1
```

donde tu número de display se obtiene haciendo

```{bash}
env | grep 'DISPLAY'
```

2. Guarda tus credenciales en `gisaid_user_password.txt` donde el primer renglón es tu usuario y el segundo tu password:

```{bash}
usuario
password
```

3. Crea tu base en `mariadb` que se llame `COVID` y dale el acceso a tu usuario. Para poner tu usuario exporta las variables en tu `.bash_profile` 

```{bash}
export MariaDB_user="usuario"
export MariaDB_password="password"
```

4. Cambia los paths en el `orchestrate.sh` y vulélvelo ejecutable con `chmod +x orchestrate.sh`.

## Funcionamiento

Algunas de las variantes que no fueron asignadas a un linaje uso `pangolin` para asignarlas. Para ello el ciclo se vuelve más complejo pues hay que descargar los `FASTA` de GISAID. El diagrama es así 

```mermaid
flowchart TD;
    a[<code>download_gisaid.py</code><br>Descarga los datos<br>de GISAID]-->b[<code>analisis_variantes.R</code><br>Identifica los datos<br>de variantes faltantes];
    b-->c[<code>download_fasta.py</code><br>Descarga los FASTA que<br> no han sido asignados<br> en GISAID];
    c-->d[<code>get_pangolin.sh</code><br>Corre <code>pangolin</code> para <br> clasificar el FASTA];
    d-->b;
    b-->e[<code>analisis_variantes.R</code><br>Genera el reporte<br>diario de variantes];
```

## Referencias de GISAID

Khare, S., et al (2021) _GISAID’s Role in Pandemic Response._ China CDC Weekly, 3(49): 1049-1051. [doi:10.46234/ccdcw2021.255](doi:10.46234/ccdcw2021.255)  PMCID: 8668406

Elbe, S. and Buckland-Merrett, G. (2017) _Data, disease and diplomacy: GISAID’s innovative contribution to global health._ Global Challenges, 1:33-46. [doi:10.1002/gch2.1018](doi:10.1002/gch2.1018)  PMCID: 31565258

Shu, Y. and McCauley, J. (2017)  _GISAID: from vision to reality._ EuroSurveillance, 22(13) [doi:10.2807/1560-7917.ES.2017.22.13.30494](doi:10.2807/1560-7917.ES.2017.22.13.30494)  PMCID: PMC5388101
