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


