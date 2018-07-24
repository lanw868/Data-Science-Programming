# 載入所需的套件包
library(tm)
library(tmcn)
library(Matrix)
library(wordcloud)
library(bitops)
library(httr)
library(RCurl)
library(XML)
library(NLP)
library(jiebaRD)
library(jiebaR)
library(devtools)
install_github("ggbiplot", "vqv")
library(scales)
library(grid)
library(ggbiplot)
library(factoextra)

# 建立文本資料結構與基本文字清洗
d.corpus <- Corpus( DirSource("./data/poem2", encoding = "UTF-8"))
d.corpus <- tm_map(d.corpus,stripWhitespace) #消除空格
d.corpus <- tm_map(d.corpus, removePunctuation) #移除標點符號
d.corpus <- tm_map(d.corpus, removeNumbers) #移除數字
d.corpus <- tm_map(d.corpus, function(word) { # 移除大小寫
  gsub("[A-Za-z0-9]", "", word)
})

# 進行斷詞，並依照作者建立文本矩陣 TermDocumentMatrix

mixseg = worker()
#斷詞
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


# TDM
n = length(seg)
TDM = tokens[[1]]
colNames <- c('白居易','杜甫','李白','孟浩然','王維')
#colNames <- names(seg)
#colNames <- gsub(".txt", "", colNames) #取代
for( id in c(2:n) )
{
  TDM = merge(TDM, tokens[[id]], by="d", all = TRUE)
  names(TDM) = c('d', colNames[1:id])
}

TDM[is.na(TDM)] <- 0 #將NA填0
TDM

library(knitr)
kable(head(TDM))


kable(tail(TDM))


# 將已建好的 TDM 轉成 TF-IDF
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

#刪除不重要(td-idf=0)的字
stopLine = rowSums(doc.tfidf[,2:(n+1)])
delID = which(stopLine == 0)

kable(head(doc.tfidf[delID,1]))

kable(tail(doc.tfidf[delID,1]))

#final result
TDM = TDM[-delID,]
doc.tfidf = doc.tfidf[-delID,]


######
# TF-IDF 文章取得的重要關鍵字
TopWords = data.frame()
for( id in c(1:n) )
{
  dayMax = order(doc.tfidf[,id+1], decreasing = TRUE)
  showResult = t(as.data.frame(doc.tfidf[dayMax[1:10],1]))
  TopWords = rbind(TopWords, showResult)
}
rownames(TopWords) = colnames(doc.tfidf)[2:(n+1)]
TopWords = droplevels(TopWords)
kable(TopWords)

# Query of Words

query.tfidf <- function(q){
  q.tfidf <- doc.tfidf[doc.tfidf$d==q, ]
  return (q.tfidf)
}

query=c("美酒", "夕陽", "故人", "老翁", "西風", "琵琶")
result = query.tfidf(query[1])
for ( id in c(2:length(query)) )
{
  q.tfidf = query.tfidf(query[id])  
  result = rbind(result, q.tfidf)
}
result

# Cosine Similiarity

cos <- function(x, y){
  return (x %*% y / sqrt(x %*% x * y %*% y))[1, 1]
}
# compare with first doc
docs.cos.sim <- apply(doc.tfidf[,2:6], 2, cos, y = doc.tfidf[,4])
docs.cos.sim

#8. 文字雲
library(wordcloud)
rownames(doc.tfidf) = doc.tfidf$d
f <- sort(rowSums(doc.tfidf[,2:6]), decreasing = T)
docs.df <- data.frame(
  word = names(f),
  freq = f
)
wordcloud(docs.df$word, docs.df$freq, scale=c(5,0.1),max.words=100,
          random.order=TRUE, random.color=FALSE, 
          rot.per=.1, colors=brewer.pal(8, "Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

######

# PCA
t = t(doc.tfidf)
# names(t) = t[1,]
colnames(t) <- t[1,]
t = t[-1,]

# t = apply(t[,1:10], 2, as.numeric)
t = apply(t, 2, as.numeric)
pcat = prcomp(t)
g <- ggbiplot(pcat, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE)
g

fviz_eig(pcat)

fviz_pca_ind(pcat, geom= c("point","text","arrow"), col.ind = "cos2")

fviz_pca_var(pcat, col.var = "contrib")



# Kmeans
kmeansData = pcat$x[,1:2]
rownames(kmeansData) <- c('白居易','杜甫','李白','孟浩然','王維')

cl <- kmeans(kmeansData, 3)
plot(kmeansData, col = cl$cluster)
text(kmeansData, labels = rownames(kmeansData), col = cl$cluster)
points(cl$centers, col = 1:3, pch = 8, cex = 2)
# pch 點樣式, cex 點大小


