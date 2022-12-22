#ORCHESTRATE
#-----------------------------
#File for downloading info and creating the plots
#Author: Rodrigo Zepeda
#Contact: rzepeda17[at]gmail.com
#----------------------------------------
#FROM https://stackoverflow.com/questions/55966634/unable-to-run-git-commands-with-crontab
eval `ssh-agent -s` && ssh-add ~/.ssh/github && ssh-add -l
cd /home/rod/VariantesCovid
date=$(date '+%Y-%m-%d')
/home/rod/miniconda3/envs/GISAID/bin/python3 /home/rod/VariantesCovid/download_gisaid.py
/usr/bin/R < /home/rod/VariantesCovid/analisis_variantes.R --no-save
/usr/bin/git -C /home/rod/VariantesCovid add .
/usr/bin/git -C /home/rod/VariantesCovid commit -m "Actualización ${date}"
/usr/bin/git -C /home/rod/VariantesCovid push origin main