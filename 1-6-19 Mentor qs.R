library(dplyr)
library(tidyr)
View(nycflights13::flights)


##### REMOVING NAs #####
# Main question: Is this the best way to filter NAs?

#Want to calculate the average arrival delay (arr_delay) to each destination
nycflights13::flights %>% group_by(dest) %>%
  summarise(avg_delay = mean(arr_delay))

#This returned a 105x2 tbl with a lot of NAs 

#I added a filter line 
nycflights13::flights %>% group_by(dest) %>%
  summarise(avg_delay = mean(arr_delay)) %>% 
  filter(avg_delay != "NA")

#This returned a 5x2 tbl with no NAs. 

#So I moved the NA filter up in my code, which seemd to work.
nycflights13::flights %>% group_by(dest) %>%
  filter(arr_delay != "NA") %>% 
  summarise(avg_delay = mean(arr_delay))


#### BUSIEST PLANES ####
#I wanted to figure out which planes (denoted by tailnum)
#had the most flights in 2013 and where they flew. I tried 
#two approaches and am curious why each produced the results they did

#Method 1
nycflights13::flights %>%
  group_by(tailnum, dest) %>%
  summarise(trips = n()) %>%
  arrange(-trips) %>%
  filter(tailnum != "NA")

#Returned a tbl (44,396 x 3). Busiest plane was N328AA with 313 trips to LAX.
#I wondered why LAX was predominant destination, so I tried different approach

#Method 2 (removed "dest" from group_by)
nycflights13::flights %>%
  group_by(carrier, tailnum) %>%
  summarise(trips = n(), dest_count = n_distinct(dest)) %>%
  arrange(-trips) %>%
  filter(tailnum != "NA")

#Returned a tbl (4,060 x 4). Busiest plane was N725MQ with 575 trips to 10 destinations.

#Here are some of the things I'm wondering:
#What exactly is group_by doing to my results?
#Can I be confident that these results are exactly what I'm looking for? (Were MQ airline 
#planes really the busiest?)
#When should I filter NAs?