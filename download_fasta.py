#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import pandas as pd
import os
from numpy.random import uniform
from selenium import webdriver
from selenium.webdriver.support.select import Select
import time
import pandas as pd
from selenium.webdriver.common.keys import Keys
from sys import platform

print("Running python")

#https://stackoverflow.com/questions/34338897/python-selenium-find-out-when-a-download-has-completed
def latest_download_file():
    files = sorted(os.listdir(os.getcwd()), key=os.path.getmtime)
    newest = files[-1]

    return newest


#Folder de descarga y chromedriver segun linux u osx
folder_of_download     = os.getcwd() + "/fasta"
if platform == "linux" or platform == "linux2":
    direccion_chromedriver = '/usr/bin/chromedriver'
else:
    direccion_chromedriver = '/usr/local/bin/chromedriver'

#Tiempo de espera
sleep_time = uniform(10, 20)

#Usuario y password en txt
a_file = open("gisaid_user_password.txt")
usuario, password = a_file.readlines()
usuario = str.rsplit(usuario)[0]


print("Opening GISAID")

# Set your password once doing and using your USERNAME and PASSWORD for GISAID
#keyring.set_password("GISAID_Download", MAGIC_USERNAME_KEY, "USERNAME")
#keyring.set_password("GISAID_Download", "USERNAME", "PASSWORD")
# and then DELETE the section

#Chequeo de que haya variantes por asignar
variantes = pd.read_csv("Unassigned.csv")

if variantes.shape[0] > 0:

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
    time.sleep(uniform(10, 20))

    #Login with password
    browser.find_element_by_id("elogin").send_keys(usuario)
    browser.find_element_by_id("epassword").send_keys(password + Keys.RETURN)
    time.sleep(uniform(10, 20))

    #Click download
    #browser.back() #update to remove publicity
    #time.sleep(sleep_time)

    browser.find_element_by_xpath("//*[contains(text(), 'Search')]").click()
    time.sleep(uniform(10, 20))

    input_search = browser.find_element_by_xpath("/html/body/form/div[5]/div/div[2]/div/div[2]/div[1]/div/table/tbody/tr[2]/td[2]/div[1]/div/div[1]/input")

    for variante in variantes["Accession ID"]:
        input_search.send_keys(variante)
        time.sleep(uniform(10, 20))

        #Click on first variant with id
        lineas = browser.find_elements_by_class_name("yui-dt-data")
        lineas[0].click()
        time.sleep(uniform(10, 20))

        frame = browser.find_element_by_tag_name('iframe')
        browser.switch_to.frame(frame)
        browser.find_element_by_xpath("//button[contains(text(), 'FASTA')]").click()
        time.sleep(uniform(10, 20))
        browser.find_element_by_xpath("//button[contains(text(), 'Back')]").click()

        browser.switch_to.parent_frame()
        time.sleep(uniform(10, 20))
        input_search.clear()
        time.sleep(uniform(10, 20))

