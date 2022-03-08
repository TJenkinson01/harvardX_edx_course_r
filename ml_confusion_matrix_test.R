library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type


#question 1a

inclass <- mutate(dat, sex = sex, type = type, inperson = dat$type == "inclass")

total_inclass <- sum(inclass$inperson == "TRUE")
total_fem_inclass <- sum(inclass$sex == "Female" & inclass$inperson == "TRUE")

total_inclass

total_fem_inclass

q1a <- total_fem_inclass/total_inclass

#question 1b

online <- mutate(dat, sex = sex, type = type, online = dat$type == "online")

total_online <- sum(online$online == "TRUE")

total_fem_online <- sum(online$sex == "Female" & online$online == "TRUE")

q1b <- total_fem_online/total_online

q1a

q1b


# correct answer

dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))

# question 2

# Based on Question 1, the most prevalent sex in "inclass" is Female, and "Online" is Male. 

ifelse(x == "inclass", "Female", "Male") %>% 
  factor(levels = levels(y)) -> y_hat 

mean(y_hat==y)


#question 3

confusionMatrix(y_hat, y)

#question 6

prev <- mean(y == "Female")

prev

