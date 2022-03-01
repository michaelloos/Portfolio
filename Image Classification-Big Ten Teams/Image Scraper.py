# -*- coding: utf-8 -*-
"""
Created on Fri Dec 10 11:02:20 2021

@author: loosm
"""

import selenium
from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.chrome.service import Service
import hashlib
from PIL import Image
import io, os
import requests
import time


def fetch_image_urls(query:str, max_links_to_fetch:int, wd:webdriver, sleep_between_interactions:int=1):
    def scroll_to_end(wd):
        wd.execute_script("window.scrollTo(0, document.body.scrollHeight);")
        time.sleep(1) 
        #wd.execute_script("window.scrollTo(0, 0);")
        time.sleep(sleep_between_interactions) 
        wd.execute_script("window.scrollTo(0, document.body.scrollHeight);")
        time.sleep(1)
        load_more_button = wd.find_element(By.CSS_SELECTOR, ".mye4qd")
        if load_more_button:
            wd.execute_script("document.querySelector('.mye4qd').click();")
            print('Execute')
        wd.execute_script("window.scrollTo(0, 0);")
    
    # build the google query
    search_url = "https://www.google.com/search?safe=off&site=&tbm=isch&source=hp&q={q}&oq={q}&gs_l=img"

    # load the page
    wd.get(search_url.format(q=query))

    image_urls = set()
    image_count = 0
    results_start = 0
    while image_count < max_links_to_fetch:
        scroll_to_end(wd)
        scroll_to_end(wd)
        scroll_to_end(wd)
        scroll_to_end(wd)
        scroll_to_end(wd)
        exceptions = 0
        sources = 0

        # get all image thumbnail results
        thumbnail_results = wd.find_elements(By.CSS_SELECTOR, "img.Q4LuWd")
        number_results = len(thumbnail_results)
        
        print(f"Found: {number_results} search results. Extracting links from {results_start}:{number_results}")
        
        for img in thumbnail_results[results_start:number_results]:
            # try to click every thumbnail such that we can get the real image behind it
            try:
                img.click()
                time.sleep(sleep_between_interactions)
            except Exception:
                exceptions +=1
                continue

            # extract image urls    
            actual_images = wd.find_elements(By.CSS_SELECTOR, 'img.n3VNCb')
            for actual_image in actual_images:
                if actual_image.get_attribute('src') and 'http' in actual_image.get_attribute('src'):
                    image_urls.add(actual_image.get_attribute('src'))
                else:
                    sources+=1

            image_count = len(image_urls)

            if len(image_urls) >= max_links_to_fetch:
                print(f"Found: {len(image_urls)} image links, done!")
                break
        else:
            print("Found:", len(image_urls), "image links, looking for more ...")
            time.sleep(3)
            #return
            print('before load')
            load_more_button = wd.find_element(By.CSS_SELECTOR, ".mye4qd")
            print('after load')
            if load_more_button:
                wd.execute_script("document.querySelector('.mye4qd').click();")
            else:
                break

        # move the result startpoint further down
        results_start = len(thumbnail_results)
        print(exceptions)
        print(sources)

    return image_urls

def persist_image(folder_path:str,url:str, counter):
    print(counter, 'persisting:', url)
    try:
        image_content = requests.get(url).content

    except Exception as e:
        print(f"ERROR - Could not download {url} - {e}")

    try:
        image_file = io.BytesIO(image_content)
        image = Image.open(image_file).convert('RGB')
        #file_path = os.path.join(folder_path,hashlib.sha1(image_content).hexdigest()[:10] + '.jpg')
        file_path = os.path.join(folder_path,str(counter) + '.jpg')
        with open(file_path, 'wb') as f:
            image.save(f, "JPEG", quality=85)
        print(f"SUCCESS - saved {url} - as {file_path}")
    except Exception as e:
        print(f"ERROR - Could not save {url} - {e}")
        

def search_and_download(search_term:str,driver_path:str,target_path='./images',number_images=400):
    counter=108
    target_folder = os.path.join(target_path,'_'.join(search_term.lower().split(' ')))

    if not os.path.exists(target_folder):
        os.makedirs(target_folder)

    with webdriver.Chrome(executable_path=driver_path) as wd:
        res = fetch_image_urls(search_term, number_images, wd=wd, sleep_between_interactions=0.1)
        
    print(len(res))
    for elem in res:
        if 'centredaily' in elem:
            continue
        else:
            persist_image(target_folder,elem, counter)
            counter+=1
        


terms = ['Illinois Fighting Illini Football', 'Indiana Hoosiers Football', 'Iowa Hawkeyes Football', 
         'Maryland Terrapins Football', 'Michigan Wolverines Football', 'Michigan State Spartans Football'
         'Minnesota Gophers Football', 'Nebraska Huskers Football', 'Northwestern Wildcats Football',
         'Ohio State Buckeyes Football', 'Penn State Nittany Lions Football', 'Purdue Boilermakers Football',
         'Rutgers Scarlet Knights Football', 'Wisconsin Badgers Football']  
    



for term in terms:

    search_term= term
    search_and_download(search_term=search_term, 
                        driver_path='C:/Users/loosm/DSC-680 Applied Data Science/Project 1/chromedriver.exe',
                        target_path='C:/Users/loosm/DSC-680 Applied Data Science/Project 1/Team Pics')
