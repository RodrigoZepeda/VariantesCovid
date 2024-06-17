#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
from selenium import webdriver
import time
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from sys import platform

print("Running python")

#https://stackoverflow.com/questions/34338897/python-selenium-find-out-when-a-download-has-completed
def latest_download_file():
    files = sorted(os.listdir(os.getcwd()), key=os.path.getmtime)
    newest = files[-1]

    return newest

#Folder de descarga y chromedriver segun linux u osx
folder_of_download     = os.getcwd()
if platform == "linux" or platform == "linux2":
    direccion_chromedriver = '/usr/bin/chromedriver'
else:
    direccion_chromedriver = '/usr/local/bin/chromedriver'

#Tiempo de espera
sleep_time = 10

#Usuario y password en txt
a_file = open("gisaid_user_password.txt")
usuario, password = a_file.readlines()
usuario = str.rsplit(usuario)[0]


print("Opening GISAID")


option = webdriver.ChromeOptions()
option.add_argument('--disable-gpu')
option.add_argument("--disable-notifications")
#option.add_argument("-incognito") #There is a bug in 2024 that shows a bubble here
#option.add_argument("disable-features=DownloadBubble,DownloadBubbleV2")

option.add_experimental_option("prefs", {
    "download.default_directory": folder_of_download,
    "savefile.default_directory": folder_of_download,
    "download.prompt_for_download": False,
    "download.directory_upgrade": True,
    "safebrowsing_for_trusted_sources_enabled": False,
    "safebrowsing.enabled": False,
    "profile.default_content_settings.popups": 0,
})

#Click on login
print("Opening chrome")
service = Service(executable_path=direccion_chromedriver)
browser = webdriver.Chrome(service=service, options=option)
browser.set_window_size(2000,1000)
browser.get("https://www.epicov.org/epi3/start")
time.sleep(sleep_time)

#Login with password
browser.find_element(By.ID, "elogin").send_keys(usuario)
browser.find_element(By.ID, "epassword").send_keys(password + Keys.RETURN)
time.sleep(sleep_time)

#Click download
#browser.back() #update to remove publicity
#time.sleep(sleep_time)

browser.find_element(By.XPATH, "/html/body/form/div[5]/div/div[2]/div/div[1]/div/div/div[5]").click()
time.sleep(sleep_time)

#Open new frame
frame = browser.find_element(By.TAG_NAME, 'iframe')
browser.switch_to.frame(frame)
time.sleep(sleep_time)

#Go to variant surveillance
text_variant = browser.find_element(By.XPATH, "//*[contains(text(), 'Variant surveillance')]").click()
time.sleep(sleep_time)

#Go to the new frame
confirmation_frame = browser.find_element(By.TAG_NAME, 'iframe')
browser.switch_to.frame(confirmation_frame)
time.sleep(sleep_time)

#Click on terms and conditions
browser.find_element(By.XPATH, '/html/body/form/div[5]/div/div[2]/div[1]/div/table/tbody/tr/td[2]/table/tbody/tr/td/div/div[1]/div/input').click()
time.sleep(sleep_time)

#Click to download
browser.find_element(By.XPATH, '/html/body/form/div[5]/div/div[2]/div[2]/div/div[2]/div/button').click()

fileends = "crdownload"

#Wait till download
while "crdownload" == fileends:
    time.sleep(sleep_time)
    newest_file = latest_download_file()
    if "crdownload" in newest_file:
        fileends = "crdownload"
    else:
        fileends = "none"

#browser.close()
