#loading the necessary libraries

pacman::p_load(tidyverse,lubridate,gtools,plotly,htmlwidgets)

#### open file - it will open the correct file if you name it correclty ####
# añadir en el script renombrar el archivo correctamente

data_path <- paste(getwd(), "/data", sep="")
file_name <- as.Date(Sys.time())
file_name <- gsub("-",  "", file_name)
file_name <- list.files("data", pattern = paste("*", file_name, ".csv", sep=""))
data <- read.csv((paste(data_path,"/", file_name, sep = "")),na.strings = "")

# patch for problems with the top function.
# data <- read.csv(file = "data/contactos_20191009.csv",na.strings = "")

## preproceso desde el raw

data$Course <- na.replace(as.character(data$Course), "none") %>%  as.factor()
data$IP.Country <- na.replace(as.character(data$IP.Country), "none") %>%  as.factor()
summary(data$Course)


data2 <- data %>% filter(is.na(Became.an.Evangelist.Date))
data2 <- data2 %>% filter(Course != "SOC") %>% 
  filter(Course != "Desarrollo Web con JavaScript") %>% 
  filter(Course != "web dev event") %>%
  filter(IP.Country != "portugal")

cohort_retention <- function(dat,
                             high_field = "Create.Date",
                             low_field = "Became.a.Customer.Date",
                             id = `Contact ID`,
                             year_to_start = 2018){
  start <- "99"
  end <- "99"
  while(!(start %in% names(dat)))
    { # start 
     # loop while incorrect column
    cat("Enter strarting field \n 1 for Create.Date \n 2 for list of options\n")
    start <- readline(prompt="Input:")
    # filling the condition stated
    if(start == "1"){start <- "Create.Date"}
    #  error message
    if (!(start %in% names(dat))) {
      print(names(dat))
      print("That name was not valid, chose one of the printed above")
    }
  }
  while(!(end %in% names(dat)))
  { # start 
    # loop while incorrect column
    cat("Enter ending field\n 1 for Became.a.Customer.Date\n 2 for list of options\n")
    end <- readline(prompt="Input:")
    # filling the condition stated
    if(end == "1"){end <- "Became.a.Customer.Date"}
    #  error message
    if (!(end %in% names(dat))) {
      print(names(dat))
      print("That name was not valid, chose one of the printed above")
    }
  }
  
  lead_to_customer <-  data2 %>% select(start,# = "Create.Date",
                                       end,# = "Became.a.Customer.Date",
                                       "Contact.ID")
  
  names(lead_to_customer) <- c("lead","customer","ID")
  
  lead_to_customer <- lead_to_customer %>% 
    filter(year(lead_to_customer$lead) >= year_to_start) %>% arrange(lead)
  
  leads <- lead_to_customer %>% select(lead ,ID) %>% na.omit()
  customers <- lead_to_customer %>% select(customer,ID) %>% na.omit()
  names(leads) <- c("date","ID")
  names(customers) <- c("date","ID")
  
  leads$date <- as.Date(leads$date)
  customers$date <- as.Date(customers$date)

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
  if (ncol(final_table)>15) {
    left <- final_table[,1:15]
    right <- tibble("12+" = rowSums(x = final_table[,15:ncol(final_table)],na.rm = TRUE))
    right[right$`12+` == 0,1] <- NA 
    final_table <- cbind(left,right)
  }
  final_table$date <- format(final_table$date, format = "%Y-%m")
  # final_table2 <- final_table %>% apply(2,as.character) %>% na.replace()
  p <- plot_ly(
    type = 'table',
    header = list(
      values = c("<b>Cohorts</b>",
                 "Cohort Size",
                 "Retention",
                 paste(names(final_table %>% select(-date))[-(1:2)],"Months")),
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
  htmlwidgets::saveWidget(p, "test.html")
  
  return(p)
}
cohort_retention(dat = data2)
