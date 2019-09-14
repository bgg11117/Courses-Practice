library('ggplot2')
library(dplyr)
library(scales)
library(xlsx)
library(reshape2)
library(lubridate)
library(ggthemes)
library(gridExtra)

# a) Load the 'diamonds' data set in R Studio. 
# How many observations are in the data set?
diamonds
nrow(diamonds)

# b) How many variables are in the data set?
ncol(diamonds)

# c) How many ordered factors are in the set?
str(diamonds)

# d) What letter represents the best color for a diamonds?
#help(diamonds)
levels(diamonds$color)

# Create a histogram of the price of
# all the diamonds in the diamond data set.
ggplot(aes(x = price),data=diamonds) + 
  geom_histogram(color = "black", fill = "DarkOrange", binwidth = 500) + 
  scale_x_continuous(breaks = seq(0, 20000, 1000)) + 
  theme(axis.text.x = element_text(angle = 45)) + 
  xlab("Price") + ylab("Count")

#Describe the shape and center of the price distribution. 
summary(diamonds$price)

# a) How many diamonds cost less than $500?
#method 1
#ddd <-NA
#ddd <-ifelse(diamonds$price<500,1,0)
#ddd <- factor(ddd)
#summary(ddd)}
diamonds %>%
  filter(price < 500) %>%
  summarize(n = n())

# a) How many diamonds cost less than $250?
diamonds %>%
  filter(price < 250) %>%
  summarize(n = n())

# c) How many diamonds cost more than $15,000?
diamonds %>%
  filter(price >= 15000) %>%
  summarize(n = n())
# Explore the largest peak in the
# price histogram you created earlier.
# Try limiting the x-axis, altering the bin width,
# and setting different breaks on the x-axis.
ggplot(aes(x = price),data=diamonds) + 
  geom_histogram(color = "black", fill = "DarkOrange", binwidth = 25) + 
  scale_x_continuous(labels=dollar,breaks = seq(0, 2000, 100)) + 
  theme(axis.text.x = element_text(angle = 45)) + 
  coord_cartesian(c(0,2000))
  xlab("Price") + ylab("Count")
# Break out the histogram of diamond prices by cut.
# You should have five histograms in separate
# panels on your resulting plot.
ggplot(aes(x = price),data=diamonds) + 
  geom_histogram(color = "black", fill = "DarkOrange", binwidth = 250) + 
  scale_x_continuous(breaks = seq(0, 5000, 250)) + 
  theme(axis.text.x = element_text(angle = 45)) + 
  coord_cartesian(c(0,5000)) +
  facet_wrap(~cut,scales="free")
# a) Which cut has the highest priced diamond?
# Premium
# by(diamonds$price, diamonds$cut, max)
# by(diamonds$price, diamonds$cut, min)
# by(diamonds$price, diamonds$cut, median)
#by(diamonds$price,diamonds$cut,max)
diamonds %>%
  group_by(cut) %>%
  summarise(max_price = max(price),
            min_price = min(price),
            median_price = median(price))

# Create a histogram of price per carat
# and facet it by cut. You can make adjustments
# to the code from the previous exercise to get
# started.
# Adjust the bin width and transform the scale
# of the x-axis using log10.
# Submit your final code when you are ready.
ggplot(aes(x = price/carat),data=diamonds) + 
  geom_histogram(color = "black", fill = "DarkOrange", binwidth = 0.02) + 
  scale_x_log10(expression(paste(Log[10], " of Price")),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  theme(axis.text.x = element_text(angle = 45)) + 
  facet_wrap(~cut,scales="free")

# Investigate the price of diamonds using box plots,
# numerical summaries, and one of the following categorical
# variables: cut, clarity, or color.
ggplot(aes(x=clarity,y=price,color=clarity),data=diamonds)+
  geom_boxplot()+
  facet_wrap(~color)
# a) What is the price range for the middle 50% of the diamonds with color D?
# c) What is the IQR for diamonds with the best color?
#IQR(subset(diamonds, price <1000)$price) 
diamonds %>%
  group_by(color) %>%
  filter(color == "D") %>%
  summarize(Quartile.25 = quantile(price, 0.25),
            Quartile.75 = quantile(price, 0.75),
            IQR = Quartile.75 - Quartile.25)
# Investigate the price per carat of diamonds across
# the different colors of diamonds using boxplots.
ggplot(aes(x=color,y = price/carat,color=color),data=diamonds) + 
  geom_boxplot()+coord_cartesian(ylim=c(1000,6000))+
  scale_y_continuous(labels=dollar)+
  xlab("color") + ylab("Price per Carat")

# Investigate the weight of the diamonds (carat) using a frequency polygon.
#Use different bin widths to see how the frequency polygon changes. 
#What carat size has a count greater than 2000? Check all that apply.
sizes = c(0.1, 0.3, 0.8, 1.01, 1.6, 2.0, 3.0, 5.0)
summary(diamonds$carat)
ggplot(aes(x=carat),data=diamonds)+
  geom_freqpoly(binwidth=0.1,alpha=0.75)+ 
  scale_x_continuous(breaks=sizes, expand = c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  geom_vline(xintercept = c(0.1, 0.8, 1.6, 2.0, 3.0, 5.0), color = "red", linetype="dashed", alpha = 0.75) +
  geom_vline(xintercept = c(0.3, 1.01), color = "forestgreen", linetype = "twodash") +
  geom_hline(yintercept = 2000, color = "brown", linetype="longdash", alpha = 0.5) + 
  xlab("Carat Size") + ylab("Count")

# The Gapminder website contains over 500 data sets with information about
# the world's population. Your task is to download a data set of your choice
# and create 2-5 plots that make use of the techniques from Lesson 3.
# http://spreadsheets.google.com/pub?key=rdCufG2vozTpKw7TBGbyoWw&output=xls

#---------------------------
hours <- as.data.frame(read.xlsx("indicator_hours per week.xlsx", sheetName="Data", header=TRUE))

hours <- hours %>%
  select(-NA.) %>% # Remove NA column carried over from xlsx
  rename(Country = Working.hours.per.week) %>%
  filter(Country != "NA") # Remove <NA> row carried over from xlsx

hours.long <- melt(hours, id=c("Country"), value.name="Hours", variable.name="Year")
hours.long <- as.data.frame(hours.long)

hours.long <- hours.long %>%
  mutate(Year = as.character(Year), # Convert to character
         Year = substr(Year, 2, 5), # Slice out the X, leaving last 4 digits; R added X since initially since column names can't start with numbers.
         Year = as.numeric(Year))   # Cast as numeric

yearStats <- hours.long %>%
  group_by(Year) %>%
  summarise(median = median(Hours, na.rm=TRUE),
            mean = mean(Hours, na.rm=TRUE),
            lower = min(Hours, na.rm=TRUE),
            upper = max(Hours, na.rm=TRUE),
            se = sd(Hours, na.rm=TRUE)/sqrt(length(Hours)),
            avg_upper = mean + (2.101*se),
            avg_lower = mean - (2.101*se),
            quant.25 = quantile(Hours, na.rm=TRUE, 0.25),
            quant.75 = quantile(Hours, na.rm=TRUE, 0.75))

yearStats <- round(yearStats, 2)

p <- ggplot(yearStats, aes(x=Year, y=median)) + 
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  geom_linerange(yearStats, mapping=aes(x=Year, ymin=lower, ymax=upper), colour = "wheat2", alpha=1, size=5) + 
  geom_linerange(yearStats, mapping=aes(x=Year, ymin=quant.25, ymax=quant.75), colour = "wheat4", size=5) +
  geom_line(yearStats, mapping=aes(x=Year, y=median, group=1)) +
  geom_vline(xintercept = 1980, colour = "wheat4", linetype=1, size=1) + 
  geom_hline(yintercept=seq(26, 56, 2), color="white", linetype=1) 

p
dottedYears <- seq(1980, 2007, 5) # Pick the years to draw dotted vertical lines on

p <- p + geom_vline(xintercept = dottedYears, color="wheat4", linetype=3, size=0.5)

p <- p + coord_cartesian(ylim = c(26,58))+
  scale_y_continuous(breaks=seq(26, 60, 2)) +
  scale_x_continuous(breaks=seq(1980, 2005, 5), expand=c(0,0) )
p
p <- p + geom_line(data = subset(hours.long, Country == "United States"), aes(x = Year, y = Hours, group = Country), color ="brown") +
  annotate("segment", x=2000, xend=2002, y=35.5, yend=36, color="brown") +
  annotate("text", x=2003, y=36.3, label="U.S. Hours", size=3.5, color="brown") + 
  annotate("segment", x=2000, xend=2001, y=33.5, yend=32) + 
  annotate("text", x=2002, y=31.7, label="World Medians", size=3.5)
p

p1 <- p + annotate("text", x=1999.9, y=56, label="Data represents hours worked per week for 52 countries   ", size=3, color="gray30") + 
  annotate("text", x=2000, y=55, label="from 1980 to 2007. Outer lighter bands show the min/max  ", size=3, color="gray30") +
  annotate("text", x=2000, y=54, label="hours for each year, and inner darker bands show the IQR.", size=3, color="gray30") + 
  ggtitle("World's Working Hours") +
  theme(plot.title=element_text(face="bold",hjust=.95,vjust=.8,color="#3C3C3C",size=20)) + 
  annotate("text", x=1994.6, y=57.5, label="Weekly", size=4, fontface="bold")

p1

# Zoom in a bit to the Inter-Quartile Range(IQR)
p + coord_cartesian(ylim=c(30, 38)) + 
  ggtitle("Inter-Quartile Range of Weekly Hours Worked Per Year Per Country") +
  theme(plot.title=element_text(face="bold",color="#3C3C3C",size=12)) + 
  geom_text(data = hours.long[hours.long$Country == "United States",], aes( x = Year, y = Hours, color = Country, group = Country, label = round(Hours, 2)), hjust = -.1, vjust = -1.2, size = 2, color = "brown") +
  geom_text( aes(x = Year, y = median, label = median), hjust = -.1, vjust = 1.2, size = 2, color = "black") 

ggplot(aes(x=Country, y=Hours, fill=Country), data=subset(hours.long, Country %in% c("Switzerland", "United States", "Japan", "United Kingdom", "France"))) +
  geom_boxplot() + 
  stat_summary(fun.y = mean, geom="point", shape = 5) +
  ylab("Hours Worked per Week") + xlab("") +
  theme(legend.position="none") + 
  ggtitle("Box plots of Hours Worked per Week for 5 Countries") + 
  theme(plot.title=element_text(face="bold",color="#3C3C3C",size=12))

ggplot(aes(x = Year, y = Hours),data=subset(hours.long, Country %in% c("Switzerland", "United States", "Japan", "United Kingdom", "France"))) + 
  geom_line(aes(color = Country, group = Country)) + 
  theme(axis.text.x = element_text(angle = 45)) + 
  scale_y_continuous(breaks=seq(26, 56, 2)) +
  scale_x_continuous(breaks=seq(1980, 2005, 5)) + 
  ylab("Hours Worked per Week") + xlab("") + 
  theme(plot.title=element_text(face="bold",color="#3C3C3C",size=12)) +
  ggtitle("Change in Weekly Hours Worked from 1980-2007 for 5 Countries")

#---------------------------------
# How many birthdays are in each month?

# Which day of the year has the most number of birthdays?

# Do you have at least 365 friends that have birthdays on everyday
# of the year?

birthdays <- as.data.frame(read.csv("birthdaysExample.csv"))

tempDates <- mdy(birthdays$dates)

Sys.setlocale("LC_TIME", "English")

birthdays <- birthdays %>%
  mutate(Birthday = tempDates,
         Year = year(tempDates),
         Month = month(tempDates, label=TRUE, abbr=FALSE),
         Day = day(tempDates),
         Weekday = weekdays(tempDates, abbr=FALSE))


birthdays$Weekday <- factor(birthdays$Weekday, levels=c('Monday', 'Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'), ordered=TRUE)

# ifelse didn't seem to work here for creating 'optional' since the else would create a NULL vector within the function and R complained.
# creates optional ggplot argument only if Day is passed in.
# aes_string takes in a string as the column name for plotting. Perfect for our arguments we pass in as strings.
# Paste our argument in for custom X and Y-axis labeling

makePlots<- function(TimeLength){
  bgg<- NULL
  if(TimeLength == "Day")  bgg<- scale_x_continuous(breaks = seq(1, 31, 1)) 
  ggplot(aes_string(x = TimeLength, fill = TimeLength),data=birthdays) + 
    geom_bar() + theme(legend.position='none')+
    stat_count(aes(label=..count..), vjust=-.1, 
             geom="text", position="identity", size=3) + 
    xlab(TimeLength)+
    ylab(paste0("Number of Birthdays per ", TimeLength))+
    bgg
}
makePlots("Month")
makePlots("Day")
makePlots("Weekday")
#----or
ggplot(aes(x=Month,fill=Month),data=birthdays)+
  geom_bar()+theme(legend.position='none')+
  stat_count(aes(label=..count..), vjust=-0.5, 
             geom="text", position="identity", size=3)+
  #scale_fill_brewer(palette="Paired")
  xlab("Month")+
  ylab("Number of Birthdays per Month")
  

