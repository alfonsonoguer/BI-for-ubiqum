#loading the necessary libraries

pacman::p_load(tidyverse,lubridate,gtools,plotly,htmlwidgets,xlsx)
# get the necesary data all is done with data2


# undestanding the attributes
# data2 %>% select(contains("country")) %>% names()

setwd("C:/Users/Alfonso/Desktop/Ubiqum/Proyecto Final/Ubiqum Analytics/")
getwd()
#### open file - it will open the correct file if you name it correclty ####
# añadir en el script renombrar el archivo correctamente

data_path <- paste(getwd(), "/data/", sep="")
file_name <- as.Date(Sys.time())
file_name <- gsub("-",  "", file_name)
file_name <- list.files("data", pattern = paste("*", file_name, ".csv", sep=""))
# data <- read.csv((paste(data_path,"/", file_name, sep = "")),na.strings = "")
# data <- read.csv("c:/Users/danie/Desktop/contact.csv", na.strings="")
# patch for problems with the top function.
data <- read.csv(file = paste(data_path, file_name, sep = ""),na.strings = "")

## preproceso desde el raw

data$Course <- na.replace(as.character(data$Course), "none") %>%  as.factor()
data$IP.Country <- na.replace(as.character(data$IP.Country), "none") %>%  as.factor()
# summary(data$Course)


data2 <- data %>% filter(is.na(Became.an.Evangelist.Date))
data2 <- data2 %>% filter(Course != "SOC") %>% 
  filter(Course != "Desarrollo Web con JavaScript") %>% 
  filter(Course != "web dev event") %>%
  filter(IP.Country != "portugal")

# seleccionar canal
matrix_list <- list()
canal <- c("Paid Search",
           "Organic Search",
           "Direct Traffic",
           "Other Campaigns")


# this loop calculates the cohort retention for the selected chanels --------
# it is a bastardized version of the cohort retention script, 
for (i in 1:length(canal)){
  lead_to_customer <- data2 %>% filter(Original.Source == canal[i]) %>% 
    select("Create.Date", "Became.a.Customer.Date", "Contact.ID")

  
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

  final_table<- final_table %>%
    select(-size) %>%
    filter(date > ymd("2018-08-02")) %>% 
    filter(date < ymd("2019-08-02")) %>%
    mutate(month = month(date)) %>% arrange(month)
  final_table$date <- format(final_table$date, format = "%Y-%m")
  matrix_list[[canal[i]]] <- final_table
}
# and returns matrix list
matrix_list


# we load the formatet forecast ---------------------------------------------------------------------


# and formateit nicely
predicted_leads <- read.csv("data/LEAD_PREDICTION.csv")

leads_to_calculate <- predicted_leads[-4,] %>% as_tibble()
leads_to_calculate$X <- canal
leads_to_calculate %>% ncol()

names(leads_to_calculate) <- c("origin",
                               month(seq(ymd("2019-09-01"), by = "month", length.out = 16)))
dates_leads <- c("origin",
                 format(seq(ymd("2019-09-01"),
                            by = "month",
                            length.out = 16),#closes seq
                        format = "%Y-%m"))
# we run a double loop to multiply all the predicted leads by the respective retention matrix
list_chanels <- list()
list_dates <- list()
for (i in 1:4){#loop the chanels
  for (j in 2:17) {#loop the predictions
    # we select the month
    mes <- names(leads_to_calculate)[j]#we save the month of the forecast
    cohort <- matrix_list[[i]] %>% filter(month == mes) %>% select(-date,-month) #we select the retention of the month
    nombre <- dates_leads[j]#we save the name that will have 
    cohort <- (cohort[1,]/100) * as.integer(leads_to_calculate[i,j])#we multiply the number of leads by the retention
    cohort <- add_column(.data = cohort,.before = 1,cohort = nombre)#we add the column chort and the date of the cohort in the first column
    list_dates[[j-1]] <- cohort#we save the results in a list
  }
  list_chanels[[canal[i]]]<- bind_rows(list_dates)#merge the list into a tibble and save it into another list!
}
