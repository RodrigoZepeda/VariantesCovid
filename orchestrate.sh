#ORCHESTRATE
#-----------------------------
#File for downloading info and creating the plots
#Author: Rodrigo Zepeda
#Contact: rzepeda17[at]gmail.com
#----------------------------------------
. ~/.keychain/`/bin/hostname`-sh
cd /home/rodrigo/VariantesCovid
date=$(date '+%Y-%m-%d')
/home/rodrigo/miniconda3/envs/GISAID/bin/python3 /home/rodrigo/VariantesCovid/download_gisaid.py
/usr/bin/R < /home/rodrigo/VariantesCovid/analisis_variantes.R --no-save
/usr/bin/git -C /home/rodrigo/VariantesCovid add .
/usr/bin/git -C /home/rodrigo/VariantesCovid commit -m "Actualización ${date}"
/usr/bin/git -C /home/rodrigo/VariantesCovid push origin main