
library(tidyverse)

###
# EXAMPLE : 如何操作csv檔
###

# 讀檔
dta <- read.csv(file = "./data/tang300/tang300_utf-8.csv", fileEncoding = "UTF-8", stringsAsFactors=F)

str(dta, vec.len = 1)
summary(dta)

#轉成類別型態
dta[,'author'] = as.factor(dta[,'author'])
dta[,'style'] = as.factor(dta[,'style'])

# 選擇tag類別
a <- dta %>% filter(str_detect(tag, '女子')) %>% select(content) %>% 
  unlist() %>%  toString()

# 選擇作者
b <- dta %>% filter(author == '白居易') %>% select(content) %>% 
  unlist() %>%  toString()
         
# 選擇詩類型
c <- dta %>% filter(style == '五言絕句') %>% select(content) %>% 
  unlist() %>%  toString()


author_list = list(unique(dta$author))[[1]]
style_list = list(unique(dta$style))[[1]]
tag_list = c('唐詩三百首', '寫景', '思鄉', '抒情', '送別', '友情', '思念', '樂府', '女子', '寫人', '邊塞', '懷古', '生活', '山水',
                 '戰爭', '秋天', '哲理', '孤獨', '月亮', '愛情', '懷人', '宮怨', '詠史懷古', '離別', '閨怨', '冬天', '抒懷', '詠物')


###
# EXAMPLE 1 (author)
###

library(tm)

#1. 建立文本資料結構與基本文字清洗
d.corpus <- Corpus(DirSource("./data/tang300/by_author", encoding = "UTF-8"))
d.corpus <- tm_map(d.corpus, stripWhitespace) #消除空格
d.corpus <- tm_map(d.corpus, removeNumbers) #移除數字
d.corpus <- tm_map(d.corpus, removePunctuation) #移除標點符號

#2. 進行斷詞，並建立文本矩陣 TermDocumentMatrix

library(jiebaR)

#斷詞
mixseg = worker()
jieba_tokenizer = function(d)
{
  unlist( segment(d[[1]], mixseg) )
}
seg = lapply(d.corpus, jieba_tokenizer)

#計數
count_token = function(d)
{
  as.data.frame(table(d))
}
tokens = lapply(seg, count_token)

#TDM
n = length(seg)
TDM = tokens[[1]]
colNames <- list.files('./data/tang300/by_author')
colNames <- gsub(".txt", "", colNames) #取代
for( id in c(2:n) )
{
  TDM = merge(TDM, tokens[[id]], by="d", all = TRUE)
  names(TDM) = c('d', colNames[1:id])
}

TDM[is.na(TDM)] <- 0 #將NA填0
View(TDM)

#3. 將已建好的 TDM 轉成 TF-IDF
tf <- apply(as.matrix(TDM[,2:(n+1)]), 2, sum) #直向相加計算總數

library(Matrix)
idfCal <- function(word_doc)
{ 
  log2( n / nnzero(word_doc) ) 
}
idf <- apply(as.matrix(TDM[,2:(n+1)]), 1, idfCal)

doc.tfidf <- TDM

tempY = matrix(rep(c(as.matrix(tf)), each = length(idf)), nrow = length(idf))
tempX = matrix(rep(c(as.matrix(idf)), each = length(tf)), ncol = length(tf), byrow = TRUE)
doc.tfidf[,2:(n+1)] <- (doc.tfidf[,2:(n+1)] / tempY) * tempX


#4. TF-IDF 文章取得的重要關鍵字
TopWords = data.frame()
for( id in c(1:n) )
{
  max = order(doc.tfidf[,id+1], decreasing = TRUE)
  showResult = t(as.data.frame(doc.tfidf[max[1:10],1]))
  TopWords = rbind(TopWords, showResult)
}
rownames(TopWords) = colnames(doc.tfidf)[2:(n+1)]
TopWords = droplevels(TopWords)

##
rownames(doc.tfidf) = doc.tfidf$d
doc.tfidf <- doc.tfidf[,1:n+1]

#5. 找前10相似

#Cosine Similiarity
cos <- function(x, y){
  return (x %*% y / sqrt(x %*% x * y %*% y))[1, 1]
}
#compare with k th doc
k = 2
docs.cos.sim <- apply(doc.tfidf, 2, cos, y = doc.tfidf[, k])
sort(docs.cos.sim, decreasing = TRUE)[1:10]

#6. 文字雲

library(wordcloud)
#BY TDM
t <- data.frame(d=TDM$d, count=rowSums(TDM[,-1]))
t <- subset(t, nchar(as.character(t$d))>1)  #不取單一個字的
wordcloud(t$d, t$count, scale=c(4,0.1),max.words=300,
          random.order=FALSE, random.color=TRUE, 
          rot.per=.1, colors=brewer.pal(12,"Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

#BY tf-idf
f <- sort(rowSums(doc.tfidf), decreasing = T)
docs.df <- data.frame(
  word = names(f),
  freq = f
)
row.names(docs.df) = NULL
wordcloud(docs.df$word, docs.df$freq, scale=c(3.5,0.1),max.words=300,
          random.order=FALSE, random.color=TRUE, 
          rot.per=.1, colors=brewer.pal(12,"Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

#7. PCA
library(ggbiplot)

tra = t(doc.tfidf)
pcat = prcomp(tra)
g <- ggbiplot(pcat, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE)
# g

library(factoextra)
# 
# fviz_eig(pcat)
# fviz_pca_ind(pcat, geom= c("point","text","arrow"), col.ind = "cos2")
# fviz_pca_var(pcat, col.var = "contrib")

#8. Kmeans
kmeansData = pcat$x[,1:2]

cl <- kmeans(kmeansData, 3)
plot(kmeansData, col = cl$cluster)
text(kmeansData, labels = rownames(kmeansData), col = cl$cluster)
points(cl$centers, col = 1:3, pch = 8, cex = 2)
# pch 點樣式, cex 點大小


###
# EXAMPLE 2 (tag)
###

#1. 建立文本資料結構與基本文字清洗
d.corpus <- Corpus(DirSource("./data/tang300/by_tag", encoding = "UTF-8"))
d.corpus <- tm_map(d.corpus, stripWhitespace) #消除空格
d.corpus <- tm_map(d.corpus, removeNumbers) #移除數字
d.corpus <- tm_map(d.corpus, removePunctuation) #移除標點符號

#2. 進行斷詞，並建立文本矩陣 TermDocumentMatrix

library(jiebaR)

#斷詞
mixseg = worker()
jieba_tokenizer = function(d)
{
  unlist( segment(d[[1]], mixseg) )
}
seg = lapply(d.corpus, jieba_tokenizer)

#計數
count_token = function(d)
{
  as.data.frame(table(d))
}
tokens = lapply(seg, count_token)

#TDM
n = length(seg)
TDM = tokens[[1]]
colNames <- list.files('./data/tang300/by_tag')
colNames <- gsub(".txt", "", colNames) #取代
for( id in c(2:n) )
{
  TDM = merge(TDM, tokens[[id]], by="d", all = TRUE)
  names(TDM) = c('d', colNames[1:id])
}

TDM[is.na(TDM)] <- 0 #將NA填0
View(TDM)

#3. 將已建好的 TDM 轉成 TF-IDF
tf <- apply(as.matrix(TDM[,2:(n+1)]), 2, sum) #直向相加計算總數

library(Matrix)
idfCal <- function(word_doc)
{ 
  log2( n / nnzero(word_doc) ) 
}
idf <- apply(as.matrix(TDM[,2:(n+1)]), 1, idfCal)

doc.tfidf <- TDM

tempY = matrix(rep(c(as.matrix(tf)), each = length(idf)), nrow = length(idf))
tempX = matrix(rep(c(as.matrix(idf)), each = length(tf)), ncol = length(tf), byrow = TRUE)
doc.tfidf[,2:(n+1)] <- (doc.tfidf[,2:(n+1)] / tempY) * tempX


#4. TF-IDF 文章取得的重要關鍵字
TopWords = data.frame()
for( id in c(1:n) )
{
  max = order(doc.tfidf[,id+1], decreasing = TRUE)
  showResult = t(as.data.frame(doc.tfidf[max[1:10],1]))
  TopWords = rbind(TopWords, showResult)
}
rownames(TopWords) = colnames(doc.tfidf)[2:(n+1)]
TopWords = droplevels(TopWords)


rownames(doc.tfidf) = doc.tfidf$d
doc.tfidf <- doc.tfidf[,1:n+1]

#5. 找前10相似

#Cosine Similiarity
cos <- function(x, y){
  return (x %*% y / sqrt(x %*% x * y %*% y))[1, 1]
}
#compare with k th doc
k = 1
docs.cos.sim <- apply(doc.tfidf, 2, cos, y = doc.tfidf[,k])
sort(docs.cos.sim, decreasing = TRUE)[1:10]

#6. 文字雲

library(wordcloud)
#BY TDM
t <- data.frame(d=TDM$d, count=rowSums(TDM[,-1]))
t <- subset(t, nchar(as.character(t$d))>1)  #不取單一個字的
wordcloud(t$d, t$count, scale=c(4,0.1),max.words=300,
          random.order=FALSE, random.color=TRUE, 
          rot.per=.1, colors=brewer.pal(12,"Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

#BY tf-idf
f <- sort(rowSums(doc.tfidf), decreasing = T)
docs.df <- data.frame(
  word = names(f),
  freq = f
)
row.names(docs.df)=NULL
wordcloud(docs.df$word, docs.df$freq, scale=c(3.5,0.1),max.words=300,
          random.order=FALSE, random.color=TRUE, 
          rot.per=.1, colors=brewer.pal(12,"Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

#7. PCA
library(ggbiplot)

tra = t(doc.tfidf)
pcat = prcomp(tra)
g <- ggbiplot(pcat, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE)
# g

library(factoextra)

fviz_eig(pcat)
fviz_pca_ind(pcat, geom= c("point","text","arrow"), col.ind = "cos2")
# fviz_pca_var(pcat, col.var = "contrib")

#8. Kmeans
library(plotly)

kmeansData = pcat$x[,1:2]
# kmeansData = kmeansData[kmeansData[,1] > -0.05, ]

cl <- kmeans(kmeansData, 5)
kmeansData <- as.data.frame(kmeansData) 
kmeansData$cl <- as.factor(cl$cluster)

# plot_ly(kmeansData, x= ~PC1, y=~PC2, type='scatter',
#          mode='markers', color = ~cl)
plot_ly(kmeansData, x= ~PC1, y=~PC2, type='scatter',
        mode='text', text=paste0("<b>",rownames(kmeansData),"</b>"), 
        color = ~cl, colors="Set1", textfont = list(size = 14) )




