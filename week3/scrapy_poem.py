#%%
import pandas as pd
import requests
from bs4 import BeautifulSoup
import re
import numpy as np

#%%
#李白
# url = 'http://www.shicimingju.com/chaxun/zuozhe/1.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/1_{}.html'

#王維
# url = 'http://www.shicimingju.com/chaxun/zuozhe/6.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/6_{}.html'

#白居易
# url = 'http://www.shicimingju.com/chaxun/zuozhe/8.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/8_{}.html'

#杜甫
# url = 'http://www.shicimingju.com/chaxun/zuozhe/10.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/10_{}.html'

#孟浩然
# url = 'http://www.shicimingju.com/chaxun/zuozhe/30.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/30_{}.html'

prefix = "http://www.shicimingju.com"


#%%
def clean(text):
    text = text.replace('\n', '')
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
        if(len(clean(poem.text)) == 20+4 or len(clean(poem.text)) == 28+4 
            or len(clean(poem.text)) == 40+8 or len(clean(poem.text)) == 56+8 ):
            f.write(clean(poem.text))


#%%
def scrapper(url):
    soup = getPage(url)
    getpoem(soup)



#%%
#李白
# url = 'http://www.shicimingju.com/chaxun/zuozhe/1.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/1_{}.html'
# f = open('./data/libai3.txt', 'w', encoding = 'utf-8-sig')

#王維
# url = 'http://www.shicimingju.com/chaxun/zuozhe/6.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/6_{}.html'
# f = open('./data/wangwei.txt', 'w', encoding = 'utf-8-sig')

#白居易
url = 'http://www.shicimingju.com/chaxun/zuozhe/8.html' 
nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/8_{}.html'
f = open('./data/baijuyi3.txt', 'w', encoding = 'utf-8-sig')

#杜甫
# url = 'http://www.shicimingju.com/chaxun/zuozhe/10.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/10_{}.html'
# f = open('./data/dufu.txt', 'w', encoding = 'utf-8-sig')

#孟浩然
# url = 'http://www.shicimingju.com/chaxun/zuozhe/30.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/30_{}.html'
# f = open('./data/menghaoran.txt', 'w', encoding = 'utf-8-sig')


scrapper(url)
for i in np.arange(2, 16, 1):
    scrapper(nextPage.format(i))
f.close()