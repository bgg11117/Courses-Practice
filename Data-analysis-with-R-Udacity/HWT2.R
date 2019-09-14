# Your first task is to create a
# scatterplot of price vs x.
# using the ggplot syntax.
library(ggplot2)
library(dplyr)
library(gridExtra)
data(diamonds)
ggplot(aes(x=x,y=price),data=diamonds)+
  geom_point(alpha=1/20)+
  coord_cartesian(xlim=c(3,10))+
  scale_y_continuous(breaks=seq(0,20000,2000))

# What is the correlation between price and x?
cor.test(diamonds$x,diamonds$price) 
#or-- with(diamonds,cor.test(x,price))

# What is the correlation between price and y?
cor.test(diamonds$y,diamonds$price) 

# What is the correlation between price and z?
cor.test(diamonds$z,diamonds$price) 

# Create a simple scatter plot of price vs depth.
ggplot(aes(x=depth,y=price),data=diamonds)+
  geom_point(alpha=1/20)

# Change the code to make the transparency of the
# points to be 1/100 of what they are now and mark
# the x-axis every 2 units. 
ggplot(aes(x=depth,y=price),data=diamonds)+
  geom_point(alpha=1/100)+
  scale_x_continuous(breaks=seq(min(diamonds$depth),max(diamonds$depth),2))

# Based on the scatterplot of depth vs. price, 
#most diamonds are between what values of depth?
quantile(diamonds$depth,0.025)
quantile(diamonds$depth,0.975)

# What's the correlation of depth vs. price?
with(diamonds,cor.test(depth,price))

# Create a scatterplot of price vs carat and omit the top 1% of price and carat values.
ggplot(aes(x=carat,y=price),data=diamonds)+
  geom_point(alpha=1/20)+
  scale_x_continuous(limits=c(0,quantile(diamonds$carat,0.99)))+
  scale_y_continuous(breaks= seq(0,18000,2000),
                     limits=c(0,quantile(diamonds$price,0.99)))

# Create a scatterplot of price vs. volume (x * y * z).
# This is a very rough approximation for a diamond's volume.
ggplot(aes(x=x*y*z,y=price),data=diamonds)+
  geom_point()
#or---
diamonds2 <- diamonds %>%
  mutate(volume=x*y*z)
ggplot(aes(x = volume, y = price),diamonds2)+ 
  geom_point()

# What's the correlation of price and volume?
# Exclude diamonds that have a volume of 0 or that are greater than or equal to 800.
with(subset(diamonds2,!(volume==0 | volume >=800)),cor.test(price,volume))

# Subset the data to exclude diamonds with a volume
# greater than or equal to 800. Also, exclude diamonds
# with a volume of 0. Adjust the transparency of the
# points and add a linear model to the plot.
smaller <- diamonds2 %>%
  filter(volume!=0 , volume<=800)

ggplot(aes(x=volume,y=price),data=smaller)+
  geom_point(alpha=1/20)+
  geom_smooth(method='lm',se=TRUE)
#or--
ggplot(aes(x=volume,y=price),data=subset(diamonds2,!(volume=0 | volume>=800)))+
  geom_point(alpha=1/20)+ geom_smooth(method='lm',se=TRUE)
#se=T就是在擬合曲線周圍帶上那個灰色的置信區間帶，se=F就是只畫出擬合曲線本身

# Use the function dplyr package
# to create a new data frame containing
# info on diamonds by clarity.
# Name the data frame diamondsByClarity
# The data frame should contain the following
# variables in this order.
#       (1) mean_price
#       (2) median_price
#       (3) min_price
#       (4) max_price
#       (5) n
# where n is the number of diamonds in each
# level of clarity.
diamondsbyclarity <- diamonds %>%
  group_by(clarity)%>%
  summarize(mean_price = mean(price),
            median_price = median(price),
            min_price = min(price),
            max_price = max(price),
            n = n()) %>%
  arrange(clarity)

# Your task is to write additional code to create two bar plots
# on one output image using the grid.arrange() function from the package
# gridExtra.
diamonds_by_clarity <- diamonds%>%
  group_by(clarity)%>%
  summarize(mean_price = mean(price))

diamonds_by_color <- diamonds%>% 
  group_by(color)%>%
  summarize(mean_price = mean(price))

c1 <- ggplot(aes(x=clarity, y=mean_price, fill=clarity),data=diamonds_by_clarity) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_brewer(palette="Set3") + 
  guides(fill = guide_legend(ncol=2, title.hjust=0.3))

c2 <- ggplot(diamonds_by_color, aes(x=color, y=mean_price, fill=color)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_brewer(palette="Set2") + 
  guides(fill = guide_legend(ncol=2, title.hjust=0.4))

grid.arrange(c1, c2)


