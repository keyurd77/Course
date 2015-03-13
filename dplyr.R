install.packages("dplyr")
library(dplyr)
msleep<-read.csv("msleep_ggplot2.csv")
head(msleep)
#dplyr verbs  Description
#select()	select columns
#filter()	filter rows
#arrange()	re-order or arrange rows
#mutate()	create new columns
#summarise()	summarise values
#group_by()	allows for group operations in the “split-apply-combine” concept
browseVignettes(package = "dplyr")
sleepData<-select(msleep, name, sleep_total)
head(sleepData)
head(select(msleep, -name))
head(select(msleep, name:order))
head(select(msleep, starts_with("sl")))
filter(msleep, sleep_total >= 16)
filter(msleep, sleep_total >= 16, bodywt >= 1)
filter(msleep, order %in% c("Perissodactyla", "Primates"))
msleep %>% #pipe operator
  select(name, sleep_total) %>% 
  head
msleep %>% arrange(order) %>% head
msleep %>% 
  select(name, order, sleep_total) %>%
  arrange(order, sleep_total) %>% 
  head
msleep %>% 
  select(name, order, sleep_total) %>%
  arrange(order, sleep_total) %>% 
  filter(sleep_total >= 16)
msleep %>% 
  select(name, order, sleep_total) %>%
  arrange(order, desc(sleep_total)) %>%#descending order 
  filter(sleep_total >= 16)
msleep %>% #new column
  mutate(rem_proportion = sleep_rem / sleep_total) %>%
  head
msleep %>% #add multiple columns with ,
  mutate(rem_proportion = sleep_rem / sleep_total, 
         bodywt_grams = bodywt * 1000) %>%
  head
msleep %>% 
  summarise(avg_sleep = mean(sleep_total))
msleep %>% 
  summarise(avg_sleep = mean(sleep_total), 
            min_sleep = min(sleep_total),
            max_sleep = max(sleep_total),
            total = n())
msleep %>% 
  group_by(order) %>%
  summarise(avg_sleep = mean(sleep_total), 
            min_sleep = min(sleep_total), 
            max_sleep = max(sleep_total),
            total = n())
msleep %>%
  mutate(rem_proportion = sleep_rem/sleep_total) %>%
  group_by(order) %>%
  summarise(avg_sleep = mean(sleep_rem)) %>%
  arrange(avg_sleep) %>%
  head
msleep %>%
  mutate(rem_proportion = sleep_rem/sleep_total) %>%
  group_by(order) %>%
  summarise(medremprop = median(rem_proportion)) %>%
  arrange(medremprop) %>%
  head (3) #0.09433962
