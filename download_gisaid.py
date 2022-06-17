#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import pandas as pd
import os
from selenium import webdriver
from selenium.webdriver.support.select import Select
import time
from selenium.webdriver.common.keys import Keys
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
sleep_time = 15

#Usuario y password en txt
a_file = open("gisaid_user_password.txt")
usuario, password = a_file.readlines()
usuario = str.rsplit(usuario)[0]


print("Opening GISAID")

# Set your password once doing and using your USERNAME and PASSWORD for GISAID
#keyring.set_password("GISAID_Download", MAGIC_USERNAME_KEY, "USERNAME")
#keyring.set_password("GISAID_Download", "USERNAME", "PASSWORD")
# and then DELETE the section


option = webdriver.ChromeOptions()
option.add_argument('--disable-gpu')
option.add_argument("-incognito")

option.add_experimental_option("prefs", {
    "download.default_directory": folder_of_download,
    "download.prompt_for_download": False,
    "download.directory_upgrade": True,
    "safebrowsing_for_trusted_sources_enabled": False,
    "safebrowsing.enabled": False
})

#Click on login
print("Opening chrome")
browser = webdriver.Chrome(executable_path=direccion_chromedriver, options=option)
browser.set_window_size(1000,1000)
browser.get("https://www.gisaid.org/")
browser.find_element_by_class_name("Login").click()
time.sleep(sleep_time)

#Login with password
browser.find_element_by_id("elogin").send_keys(usuario)
browser.find_element_by_id("epassword").send_keys(password + Keys.RETURN)
time.sleep(sleep_time)

#Click download
#browser.back() #update to remove publicity
#time.sleep(sleep_time)

browser.find_element_by_xpath("/html/body/form/div[5]/div/div[2]/div/div[1]/div/div/div[5]").click()
time.sleep(sleep_time)

#Open new frame
frame = browser.find_element_by_tag_name('iframe')
browser.switch_to.frame(frame)
time.sleep(sleep_time)

text_variant = browser.find_element_by_xpath("//*[contains(text(), 'Variant surveillance')]").click()
time.sleep(sleep_time)

confirmation_frame = browser.find_element_by_tag_name('iframe')
browser.switch_to.frame(confirmation_frame)
time.sleep(sleep_time)


browser.find_element_by_xpath('/html/body/form/div[5]/div/div[2]/div[1]/div/table/tbody/tr/td[2]/table/tbody/tr/td/div/div[1]/div/input').click()
time.sleep(sleep_time)
browser.find_element_by_xpath('/html/body/form/div[5]/div/div[2]/div[2]/div/div[2]/div/button').click()

fileends = "crdownload"

#Wait till download
while "crdownload" == fileends:
    time.sleep(sleep_time)
    newest_file = latest_download_file()
    if "crdownload" in newest_file:
        fileends = "crdownload"
    else:
        fileends = "none"

browser.close()
