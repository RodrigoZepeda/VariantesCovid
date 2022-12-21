#ORCHESTRATE
#-----------------------------
#File for downloading info and creating the plots
#Author: Rodrigo Zepeda
#Contact: rzepeda17[at]gmail.com
#----------------------------------------
cd /home/rod/VariantesCovid
date=$(date '+%Y-%m-%d')
/home/rod/miniconda3/envs/GISAID/bin/python3 /home/rod/VariantesCovid/download_gisaid.py
/usr/bin/R < /home/rod/VariantesCovid/analisis_variantes.R --no-save
/usr/bin/git -C /home/rod/VariantesCovid add .
/usr/bin/git -C /home/rod/VariantesCovid commit -m "Actualización ${date}"
/usr/bin/git -C /home/rod/VariantesCovid push origin main