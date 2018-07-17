# week2_task1
library(ggplot2)

data()

head(airquality)
airquality$Month <- factor(airquality$Month)
summary(airquality)


ggplot(data = airquality, aes(x = Month)) +
  geom_bar(fill="lightblue", colour = "black")



ggplot(data = airquality, aes(x =Temp)) +
  geom_histogram() + facet_wrap(Month~.)


ggplot(data = airquality, aes(x = Temp, y=Ozone, color=Month)) +
  geom_point()

ggplot(data = airquality, aes(x = Temp, y=Ozone, color=Month)) +
  geom_point() + facet_wrap(Month~.)

ggplot(data = airquality, aes(x = Wind, y=Ozone, color=Month)) +
  geom_point()



ggplot(data = airquality, aes(x = Month, y = Temp)) +
  geom_boxplot()


ggplot(data = airquality, aes(x = Temp, y = Ozone, color = Month)) +
  geom_point() + facet_wrap(Month~.)

ggplot(data = airquality, aes(x = Temp, y = Ozone, color=Month, size=Wind)) +
  geom_point()

## BONUS

library(ggmap)
library(mapproj)

map <- get_map(location = 'Taiwan', zoom = 7, language = "zh-TW")
ggmap(map)

# toner-lite  type
map <- get_map(location = c(lon = 120.233937, lat = 22.993013),
               zoom = 10, language = "zh-TW", maptype = "toner-lite")
ggmap(map)


# read csv
itaiwan <- read.csv("itaiwan-3.csv")

itaiwan


library(ggmap)
map <- get_map(location = 'Kaohsiung', zoom = 13, language = "zh-TW")
ggmap(map, darken = c(0.1, "white")) + geom_point(aes(x = longitude, y = latitude), data = itaiwan) 
+ ggtitle("高雄iTaiwan分布圖(局部)") + theme(plot.title = element_text(hjust = 0.5))



