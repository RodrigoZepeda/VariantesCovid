#ORCHESTRATE
#-----------------------------
#File for downloading info and creating the plots
#Author: Rodrigo Zepeda
#Contact: rzepeda17[at]gmail.com
#----------------------------------------
#FROM https://stackoverflow.com/questions/55966634/unable-to-run-git-commands-with-crontab
eval `ssh-agent -s` && ssh-add ~/.ssh/github && ssh-add -l
cd /Users/rodrigozepedatello/Documents/VariantesCovid
date=$(date '+%Y-%m-%d')
/usr/local/Caskroom/miniconda/base/envs/GISAID/bin/python3 /Users/rodrigozepedatello/Documents/VariantesCovid/download_gisaid.py
/usr/local/bin/R < /Users/rodrigozepedatello/Documents/VariantesCovid/analisis_variantes.R --no-save
/usr/local/bin/git -C /Users/rodrigozepedatello/Documents/VariantesCovid add .
/usr/local/bin/git -C /Users/rodrigozepedatello/Documents/VariantesCovid commit -m "Actualización ${date}"
/usr/local/bin/git -C /Users/rodrigozepedatello/Documents/VariantesCovid push origin main