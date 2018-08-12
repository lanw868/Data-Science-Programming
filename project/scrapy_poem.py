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

#李商隱
# url = 'http://www.shicimingju.com/chaxun/zuozhe/11.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/11_{}.html'
# f = open('./data/李商隱.txt', 'w', encoding = 'utf-8-sig')

#李賀
# url = 'http://www.shicimingju.com/chaxun/zuozhe/15.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/15_{}.html'
# f = open('./data/李賀.txt', 'w', encoding = 'utf-8-sig')

#王勃
# url = 'http://www.shicimingju.com/chaxun/zuozhe/25.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/25_{}.html'
# f = open('./data/王勃.txt', 'w', encoding = 'utf-8-sig')

#韓愈
# url = 'http://www.shicimingju.com/chaxun/zuozhe/28.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/28_{}.html'
# f = open('./data/韓愈.txt', 'w', encoding = 'utf-8-sig')

#孟浩然
# url = 'http://www.shicimingju.com/chaxun/zuozhe/30.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/30_{}.html'
# f = open('./data/menghaoran.txt', 'w', encoding = 'utf-8-sig')

#杜牧
# url = 'http://www.shicimingju.com/chaxun/zuozhe/22.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/22_{}.html'
# f = open('./data/杜牧.txt', 'w', encoding = 'utf-8-sig')

#李世民
# url = 'http://www.shicimingju.com/chaxun/zuozhe/24.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/24_{}.html'
# f = open('./data/李世民.txt', 'w', encoding = 'utf-8-sig')

#溫庭筠
# url = 'http://www.shicimingju.com/chaxun/zuozhe/27.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/27_{}.html'
# f = open('./data/溫庭筠.txt', 'w', encoding = 'utf-8-sig')

#孟郊
# url = 'http://www.shicimingju.com/chaxun/zuozhe/31.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/31_{}.html'
# f = open('./data/孟郊.txt', 'w', encoding = 'utf-8-sig')

#柳宗元
# url = 'http://www.shicimingju.com/chaxun/zuozhe/50.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/50_{}.html'
# f = open('./data/柳宗元.txt', 'w', encoding = 'utf-8-sig')

#王昌齡
# url = 'http://www.shicimingju.com/chaxun/zuozhe/56.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/56_{}.html'
# f = open('./data/王昌齡.txt', 'w', encoding = 'utf-8-sig')

#李煜
# url = 'http://www.shicimingju.com/chaxun/zuozhe/43.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/43_{}.html'
# f = open('./data/李煜.txt', 'w', encoding = 'utf-8-sig')

#張繼
# url = 'http://www.shicimingju.com/chaxun/zuozhe/155.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/155_{}.html'
# f = open('./data/張繼.txt', 'w', encoding = 'utf-8-sig')

#歐陽修
# url = 'http://www.shicimingju.com/chaxun/zuozhe/115.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/115_{}.html'
# f = open('./data/歐陽修.txt', 'w', encoding = 'utf-8-sig')

#蘇洵
# url = 'http://www.shicimingju.com/chaxun/zuozhe/68.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/68_{}.html'
# f = open('./data/蘇洵.txt', 'w', encoding = 'utf-8-sig')

#蘇軾
# url = 'http://www.shicimingju.com/chaxun/zuozhe/9.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/9_{}.html'
# f = open('./data/蘇軾.txt', 'w', encoding = 'utf-8-sig')

#蘇轍
# url = 'http://www.shicimingju.com/chaxun/zuozhe/502.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/502_{}.html'
# f = open('./data/蘇轍.txt', 'w', encoding = 'utf-8-sig')

#曾鞏
# url = 'http://www.shicimingju.com/chaxun/zuozhe/449.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/449_{}.html'
# f = open('./data/曾鞏.txt', 'w', encoding = 'utf-8-sig')

#王安石
# url = 'http://www.shicimingju.com/chaxun/zuozhe/26.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/26_{}.html'
# f = open('./data/王安石.txt', 'w', encoding = 'utf-8-sig')

#文天祥
# url = 'http://www.shicimingju.com/chaxun/zuozhe/69.html' 
# nextPage = 'http://www.shicimingju.com/chaxun/zuozhe/69_{}.html'
# f = open('./data/文天祥.txt', 'w', encoding = 'utf-8-sig')

scrapper(url)
for i in np.arange(2, 16, 1):
    scrapper(nextPage.format(i))
f.close()