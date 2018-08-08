###
# EXAMPLE 2 (tag)
###

#1. 建立文本資料結構與基本文字清洗
d.corpus <- Corpus(DirSource("./data/tang300/by_tag", encoding = "UTF-8"))
d.corpus <- tm_map(d.corpus, stripWhitespace) #消除空格
d.corpus <- tm_map(d.corpus, removeNumbers) #移除數字
d.corpus <- tm_map(d.corpus, removePunctuation) #移除標點符號
d.corpus <- tm_map(d.corpus, function(word) { # 移除英數
  gsub("[A-Za-z0-9]", "", word)
})

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
# View(TDM)

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


rownames(doc.tfidf) = doc.tfidf$d
doc.tfidf <- doc.tfidf[,1:n+1]

############################


str ='葡萄美酒夜光杯，

欲飲琵琶馬上催。

醉臥沙場君莫笑，

古來征戰幾人回？'


str %>% stripWhitespace %>% removeNumbers %>% removePunctuation 

function(word) { # 移除英數
  gsub("[A-Za-z0-9]", "", word)
}



#5. 找前10相似

#Cosine Similiarity
cos <- function(x, y){
  return (x %*% y / sqrt(x %*% x * y %*% y))[1, 1]
}
#compare with k th doc
k = 1
docs.cos.sim <- apply(doc.tfidf, 2, cos, y = doc.tfidf[,k])
sort(docs.cos.sim, decreasing = TRUE)[1:10]


