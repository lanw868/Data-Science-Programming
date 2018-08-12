#%%
import pandas as pd
import requests
from bs4 import BeautifulSoup
import re
import numpy as np


#%%
def clean(text):
    text = text.replace('\n', '')
    text = text.replace('[', '')
    text = text.replace(']', '')
    text = re.sub(r'\([^)]*\)', '', text)

    return text.replace(' ', '')


def getPage(url):
    response = requests.get(url)
    content = response.text
    soup = BeautifulSoup(content, 'html5lib')
    return soup


#%%
def getpoem(soup):
    title = soup.find_all('h3')
    for ti in title:
        # f.write(clean(ti.a.text))
        href = ti.find('a')['href']
        poemURL = prefix + href
        sess = requests.Session()
        poemContent = sess.get(poemURL).content
        poemSoup = BeautifulSoup(poemContent, 'html5lib')
        poem = poemSoup.find('div', attrs={'class':'shici-content'})
        # if(len(clean(poem.text)) < 150 ):
        #     f.write(clean(poem.text))
        f.write(clean(poem.text))


#%%
def scrapper(url):
    soup = getPage(url)
    getpoem(soup)



prefix = "http://www.shicimingju.com"

#王安石
url = 'http://www.shicimingju.com/chaxun/zuozhe/26.html' 
nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/26_{}.html'
f = open('./data/王安石.txt', 'w', encoding = 'utf-8-sig')

scrapper(url)
for i in np.arange(2, 47, 1):
    print(i)
    scrapper(nextPage.format(i))
f.close()