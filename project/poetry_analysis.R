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

# LISTS
author_list = list(unique(dta$author))[[1]]
tag_list = c('寫景', '思鄉', '抒情', '送別', '友情', '思念', '樂府', '女子', '寫人', '邊塞', '懷古', '生活', '山水',
             '戰爭', '秋天', '哲理', '孤獨', '月亮', '愛情', '懷人', '宮怨', '詠史懷古', '離別', '閨怨', '冬天', '抒懷', '詠物')

# FUNCTIONS
mixseg = worker()
jieba_tokenizer = function(d)
{
  unlist( segment(d[[1]], mixseg) )
}

count_token = function(d)
{
  as.data.frame(table(d))
}

idfCal <- function(word_doc)
{ 
  log2( n / nnzero(word_doc) ) 
}

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
# EXAMPLE B1 (唐詩三百首各tag中 各作者詩數量統計) (畫直方圖)
# parameters: input.tag
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

###
# EXAMPLE B2 
# parameters: input.tag
###

#1. 建立文本資料結構與基本文字清洗
d.corpus_tag <- Corpus(DirSource("./data/tang300/by_tag", encoding = "UTF-8"))
d.corpus_tag <- tm_map(d.corpus_tag, stripWhitespace) #消除空格
d.corpus_tag <- tm_map(d.corpus_tag, removeNumbers) #移除數字
d.corpus_tag <- tm_map(d.corpus_tag, removePunctuation) #移除標點符號
d.corpus_tag <- tm_map(d.corpus_tag, function(word) { # 移除英數
  gsub("[A-Za-z0-9]", "", word)
})

#2. 進行斷詞，並建立文本矩陣 TermDocumentMatrix
seg_tag = lapply(d.corpus_tag, jieba_tokenizer)
tokens_tag = lapply(seg_tag, count_token)

#TDM
n_tag = length(seg_tag)
TDM_tag = tokens_tag[[1]]
tagNames <- list.files('./data/tang300/by_tag')
tagNames <- gsub(".txt", "", tagNames) #取代
for( id in c(2:n_tag) )
{
  TDM_tag = merge(TDM_tag, tokens_tag[[id]], by="d", all = TRUE)
  names(TDM_tag) = c('d', tagNames[1:id])
}
TDM_tag[is.na(TDM_tag)] <- 0 #將NA填0

#3. 將已建好的 TDM 轉成 TF-IDF
tf_tag <- apply(as.matrix(TDM_tag[,2:(n_tag + 1)]), 2, sum) #直向相加計算總數
idf_tag <- apply(as.matrix(TDM_tag[,2:(n_tag + 1)]), 1, idfCal)
doc.tfidf_tag <- TDM_tag

tempY_tag = matrix(rep(c(as.matrix(tf_tag)), each = length(idf_tag)), 
                   nrow = length(idf_tag))
tempX_tag = matrix(rep(c(as.matrix(idf_tag)), each = length(tf_tag)), 
                   ncol = length(tf_tag), byrow = TRUE)
doc.tfidf_tag[,2:(n+1)] <- (doc.tfidf_tag[,2:(n+1)] / tempY_tag) * tempX_tag

#4. 畫各tag前(十)多用字


words_count_tag = TDM_tag[,c('d', input.tag)]
colnames(words_count_tag) = c('word', 'count')
words_count_tag = words_count_tag[rev(order(words_count_tag$count)),]
rownames(words_count_tag)=NULL


ggplot(words_count_tag[1:20,], aes(x = reorder(word, count), y =count)) + 
  geom_bar(stat = "identity", fill='lightblue') + 
  coord_flip()+
  labs(x='word', y='count', title=paste('Tag: ', input.tag)) +
  theme(panel.background = element_blank(),
        axis.title = element_text(color = '#2d2d2d'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text.x = element_text(color='#2d2d2d',face='bold',size=10),
        plot.title = element_text(hjust=0.5,face='bold',size=15))


##
rownames(doc.tfidf) = doc.tfidf$d
doc.tfidf <- doc.tfidf[,1:n+1]

