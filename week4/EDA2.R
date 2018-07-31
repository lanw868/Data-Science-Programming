library(tidyverse)
library(ggplot2)
library(ggridges)
library(plotly)
library(tidytext)
library(Hmisc)


zomato <- read.csv('./data/zomato-restaurants-data/zomato.csv')
country_code <- read.csv('./data/zomato-restaurants-data/Country-Code.csv')

# 合併兩資料
df <- merge(zomato, country_code, by.x="Country.Code", by.y="Country.Code")

# 刪去多餘欄位
drops <- c("Country.Code")
df <- df[ , !(names(df) %in% drops)]

head(df)
# df %>% summary()

## 以印度為分析目標
india <- df %>% filter(Country=='India') 

india$Price.range <- as.factor(india$Price.range)

#將底下的圖設定為黑白配色（theme_bw）
old <- theme_set(theme_bw())
#####################################################################################
#  Longitude, Latitude, Average.Cost.for.two, Price.range, Aggregate.rating, Votes  #
#####################################################################################


  ## Part A. Average.Cost.for.two vs Price.range

  
### 首先從不同Price.range的Average.Cost.for.two盒鬚圖開始，並計算其信賴區間

ggplot(data = india, aes(x = Price.range, y = Average.Cost.for.two)) + 
  geom_boxplot() + 
  coord_flip() +
  labs( y = 'Average cost for two', x = 'Price range', 
        title = 'Average cost for two Box')

# 以下函式計算95%信賴區間
with(india, 
     tapply(Average.Cost.for.two, Price.range,
            function(x) 
              c(mean(x) + c(-2, 2) * sd(x)/sqrt(length(x)))))

### 以下用 t-test 檢驗不同性別是否存在數學成績差異

#此函數會預設進行 Welch 校正，以處理兩樣本變異數不相同的問題
t.test(Average.Cost.for.two ~ Price.range, data = india)

#可加上參數 var.equal=TRUE 來假設變異數同值(不做Welch校正)
t.test(Average.Cost.for.two ~ Price.range, data = india, var.equal = TRUE)



  ## Part B. Aggregate.rating vs Rating.text
  
### 首先從不同Rating.text的Aggregate.rating盒鬚圖開始，並計算其信賴區間

# 照順序排放
india$Rating.text <- factor(india$Rating.text, 
                                 levels = c('Not rated',
                                            'Poor',
                                            'Average',
                                            'Good', 
                                            'Very Good',
                                            'Excellent'))
#看不同的Rating.text下的Rating分數平均數
tapply(india$Aggregate.rating, india$Rating.text, mean)


  
ggplot(data = india, aes(x = Rating.text, y = Aggregate.rating)) + 
  geom_boxplot() + 
  coord_flip() +
  labs( y = 'Aggregate rating', x = 'Rating text', 
        title = 'Aggregate rating Box')

#以下函式計算95%信賴區間
with(india, 
     tapply(Aggregate.rating, Rating.text,
            function(x) 
              c(mean(x) + c(-2, 2) * sd(x)/sqrt(length(x)))))



#同父母教育程度下的數學分數平均數，加上信賴區間
#看不同的Rating.text下的Rating分數平均數
ggplot(data = india, 
       aes(x = Rating.text, y = Aggregate.rating)) +
  stat_summary(fun.data = 'mean_cl_boot', size = 1)+
  scale_y_continuous(breaks = seq(1, 5, by = 0.5)) +
  geom_hline(yintercept = mean(india$Aggregate.rating) , 
             linetype = 'dotted') +
  labs(x = 'Rating text', y = 'Aggregate rating') +
  coord_flip()

  
anova(m1 <- lm(Aggregate.rating ~ Rating.text, data = india))



# 1. Cost vs  Longitude/Latitude
# 2. Votes vs Cost 
# 3. (Votes, Switch.to.order.menu, Has.Table.booking, Has.Online.delivery





