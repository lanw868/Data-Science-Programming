library(tidyverse)
library(tm)
library(jiebaR)
library(Matrix)
library(wordcloud)
library(ggbiplot)
library(factoextra)
library(plotly)

# 讀檔
dta <- read.csv(file = "./data/tang300/tang300_utf-8.csv", fileEncoding = "UTF-8",
                stringsAsFactors=F)
#轉成類別型態
dta[,'author'] = as.factor(dta[,'author'])
dta[,'style'] = as.factor(dta[,'style'])

author_list = list(unique(dta$author))[[1]]
tag_list = c('寫景', '思鄉', '抒情', '送別', '友情', '思念', '樂府', '女子', '寫人', '邊塞', '懷古', '生活', '山水',
             '戰爭', '秋天', '哲理', '孤獨', '月亮', '愛情', '懷人', '宮怨', '詠史懷古', '離別', '閨怨', '冬天', '抒懷', '詠物')

###
# EXAMPLE A (唐詩三百首中 各tag詩數量統計) (畫長條圖)
# parameters: None
###

tag_count <- dta$tag %>% strsplit('，') %>% unlist %>% count 
tag_count = tag_count[rev(order(tag_count$freq)),]
#只取有在tag_list中的
tag_count = tag_count[tag_count$x %in%  tag_list, ]

ggplot(tag_count, aes(x = reorder(x,-freq), y = freq)) + 
  geom_bar(stat = "identity", fill='lightblue') + 
  labs(x='tag',title='唐詩三百首數量統計') + 
  theme(panel.background = element_blank(),
        axis.title = element_text(color = '#2d2d2d'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text.x = element_text(color='#2d2d2d',face='bold',size=10),
        plot.title = element_text(hjust=0.5,face='bold',size=15))

###
# EXAMPLE B (唐詩三百首各tag中 各作者詩數量統計) (畫直方圖)
#     parameters: input.tag
###

# 選擇tag類別
input.tag = '女子'
tag_author_count <- dta %>% filter(str_detect(tag, input.tag)) %>% select(author) %>% 
  unlist() %>% count

ggplot(tag_author_count, aes(x = reorder(x,-freq), y = freq)) + 
  geom_bar(stat = "identity", fill='lightblue') + 
  labs(x='作者',title=paste('Tag: ',input.tag)) + 
  theme(panel.background = element_blank(),
        axis.title = element_text(color = '#2d2d2d'),
        strip.text.x = element_text(color='#2d2d2d',face='bold',size=10),
        plot.title = element_text(hjust=0.5,face='bold',size=15))


