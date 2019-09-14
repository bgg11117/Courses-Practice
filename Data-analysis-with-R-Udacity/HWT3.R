data(diamonds)
# Create a histogram of diamond prices.
# Facet the histogram by diamond color
# and use cut to color the histogram bars.

# The plot should look something like this.
# http://i.imgur.com/b5xyrOu.jpg

# Note: In the link, a color palette of type
# 'qual' was used to color the histogram using
# scale_fill_brewer(type = 'qual')
library(ggplot2)
library(dplyr)
ggplot(aes(x = price, fill = cut),data=diamonds) +
  geom_histogram() +
  facet_wrap(~ color) +
  scale_fill_brewer(type = 'qual') +
  scale_x_log10(expression(paste(Log[10], " of Price"))) +
  ylab("Count")

# Create a scatterplot of diamond price vs.
# table and color the points by the cut of
# the diamond.

# The plot should look something like this.
# http://i.imgur.com/rQF9jQr.jpg

# Note: In the link, a color palette of type
# 'qual' was used to color the scatterplot using
# scale_color_brewer(type = 'qual')
ggplot(aes(x=table,y=price,,color=cut),data=diamonds)+
  geom_jitter(size=3)+
  scale_x_continuous(breaks=seq(50,80,2),
                     limits=c(50,80))+
  scale_color_brewer(type='qual')+
  theme_minimal() 

## What is the typical table range for the majority of diamonds of ideal cut?
# 53 to 57
## What is the typical table range for the majory of diamonds of premium cut?
# 58 to 62
## Use the graph that you created from the previous exercise to see the answer. You do not need to run summaries.

# Create a scatterplot of diamond price vs.
# volume (x * y * z) and color the points by
# the clarity of diamonds. Use scale on the y-axis
# to take the log10 of price. You should also
# omit the top 1% of diamond volumes from the plot.
# Note: In the link, a color palette of type
# 'div' was used to color the scatterplot using
# scale_color_brewer(type = 'div')

diamonds <- diamonds%>%
  mutate(volume = x*y*z)

ggplot(aes(x = volume, y = price, color = clarity),data=subset(diamonds, volume <= quantile(volume, 0.99) & volume > 0 )) +
  geom_jitter(size = 3) + 
  scale_y_log10() +
  scale_color_brewer(type = 'div') + 
  theme_minimal()

#We might wonder how much of a person's network on
# a service like Facebook the user actively initiated. Two users
# with the same degree (or number of friends) might be very
# different if one initiated most of those connections on the
# service, while the other initiated very few. So it could be
# useful to consider this proportion of existing friendships that
# the user initiated. This might be a good predictor of how active
# a user is compared with their peers, or other traits, such as
# personality (i.e., is this person an extrovert?).

# Your task is to create a new variable called 'prop_initiated'
# in the Pseudo-Facebook data set. The variable should contain
# the proportion of friendships that the user initiated.

pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
#pf<- tbl_df(read.table('pseudo_facebook.tsv',header = TRUE))

pf<-pf %>%
  mutate(prop_initiated= friendships_initiated/friend_count)

# Create a line graph of the proportion of
# friendships initiated ('prop_initiated') vs.
# tenure and color the line segment by
# year_joined.bucket.

# Recall, we created year_joined.bucket in Lesson 5
# by first creating year_joined from the variable tenure.
# Then, we used the cut function on year_joined to create
# four bins or cohorts of users.

# (2004, 2009]
# (2009, 2011]
# (2011, 2012]
# (2012, 2014]

pf <- pf %>%
  mutate(year_joined = floor(2014-tenure/365),
         year_joined_bucket = cut(year_joined,breaks = c(2004,2009,2011,2012,2014)))
pf <- pf %>%
  mutate(prop_initiated = ifelse(friend_count > 0, friendships_initiated/friend_count, 0))

ggplot(aes(x=tenure, y=prop_initiated,color=year_joined_bucket),data= subset(pf, tenure > 0)) +
  geom_line(stat='summary', fun.y=mean) + 
  theme_minimal()

# Smooth the last plot you created of
# of prop_initiated vs tenure colored by
# year_joined.bucket. You can bin together ranges
# of tenure or add a smoother to the plot.
ggplot(aes(x=tenure, y=prop_initiated,color=year_joined_bucket),data= subset(pf, tenure > 0)) +
  geom_line(stat='summary', fun.y=mean) + 
  geom_smooth()+
  theme_minimal()

## For the group with the largest proportion of 
## friendships initated, what is the group's average 
## (mean) proportion on friendships initiated?
pf %>%
  filter(year_joined_bucket=='(2012,2014]') %>%
  summarize(avg = mean(prop_initiated,na.rm=TRUE))
#subs<-subset(pf, year_joined > 2012)
#mean(subs$prop_initiated, na.rm = T)


## Why do you think this group's proportion of friendships initiated is higher than the others?
# They have newer accounts. To get started after first creating an account, 
# you have to build your online network of friends based on your existing network of friends. 
# After this initial population, someone may add/acquire new friends 
# at a lower rate allowing the proportion to even out slightly.
# The younger cohort may also have fundamentally different patterns of acquisition;
# 2005 wasn't too long ago and Facebook was first targeted at a college-age demographic, 
# who are now in their late twenties and early thirties.
# Rates might have already been lower in general for this group, 
# who never were in their mid-teens and starting ubiquitous Facebook accounts.

# Create a scatter plot of the price/carat ratio
# of diamonds. The variable x should be
# assigned to cut. The points should be colored
# by diamond color, and the plot should be
# faceted by clarity.
ggplot(aes(x = cut, y = price/carat, color = color),data=diamonds) + 
  geom_jitter(alpha=1/2) + 
  facet_wrap(~clarity) + 
  scale_color_brewer(type = 'div')