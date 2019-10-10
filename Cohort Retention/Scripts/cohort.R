pacman::p_load(tidyverse,lubridate,readr,reshape2,ggplot2,plotly,htmlwidgets)
# data
high_field <- "Became a Lead Date"
low_field <- "Became a Customer Date"


lead_to_customer <-  data %>% select(high_field,
                                     low_field,
                                     "Contact ID")

names(lead_to_customer) <- c("lead","customer","ID")

lead_to_customer <- lead_to_customer %>% 
  filter(year(lead_to_customer$lead) >= 2018) %>% arrange(lead)

leads <- lead_to_customer %>% select(lead ,ID) %>% na.omit()
customers <- lead_to_customer %>% select(customer,ID) %>% na.omit()
names(leads) <- c("date","ID")
names(customers) <- c("date","ID")

# leads tiene users by cohort
# customers tiene users and month of activity
# cohort_size tiene cohort y size
# user_survival tiene user and timeframe for activity
leads$date <-
  floor_date(leads$date, unit = "month")
customers$date <-
  floor_date(customers$date, unit = "month")



time_interval <- full_join(leads, customers, "ID") %>% na.omit()

user_survival <- time_interval %>%
  mutate(timediff = interval(date.x,date.y) %/% months(1)) %>% 
  select(ID,timediff)
cohort_size <- leads %>% group_by(date) %>% summarise(size = n())

retention_table <- full_join(leads, user_survival,"ID") %>%
  na.omit() %>% 
  group_by(date,timediff) %>% summarise(count = n())


cohort_retention <- right_join(cohort_size, retention_table, by = "date") %>% 
  mutate(prc = (round((count/size)*100,2))) %>% 
  select(date,size,timediff,prc)

accumulated <- cohort_retention %>% group_by(date) %>% summarise(accumulated = sum(prc))

final_table <- left_join(cohort_retention, accumulated, "date") %>%
  select(date,size,accumulated,timediff,prc) %>% spread(key = timediff, value = prc)
final_table$date <- format(final_table$date, format = "%Y-%m")

library(plotly)

p <- plot_ly(
  type = 'table',
  header = list(
    values = c("<b>Cohorts</b>", names(final_table %>% select(-date))),
    align = c('left', rep('center', ncol(final_table))),
    line = list(width = 1, color = '#506784'),
    fill = list(color = '#119DFF'),
    font = list(family = "Arial", size = 14, color = "white")
  ),
  cells = list(
    values = rbind(
      final_table$date, 
      t(as.matrix(unname(final_table %>% select(-date))))
    ),
    align = c('left', rep('center', ncol(final_table %>% select(-date)))),
    line = list(color = '#506784', width = 1),
    fill = list(color = c('#25FEFD', 'rgba(228, 222, 249, 0.65)')),
    font = list(family = "Arial", size = 12, color = c("black"))
  ))
p
htmlwidgets::saveWidget(p, "test.html")
