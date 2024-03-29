#loading the necessary libraries

pacman::p_load(tidyverse,lubridate,gtools,plotly,htmlwidgets,xlsx)
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
  # filtering by city the logic here is going to be a swich that will return
  # filtered  lead_to_customer by the category we want.
  
  select_origin <- "99"

  while(!(select_origin %in% c(1:4)))
  { # start
    # loop while incorrect column
    cat("Select an option \n 1 for no filter \n 2 for Course.City  \n 3 for IP.City \n 4 for IP.Country \n")
    select_origin <- readline(prompt="Input:")
  }

  
  filter_nothing <- function(dataframe){
    return(dataframe %>% select("Create.Date",
                                "Became.a.Customer.Date",
                                "Contact.ID"))
  }

  # this function loops until you select a valid city, then filters the dataset 
  # the way we want it
  filter_Course_City <- function(dataframe, something = "random_anoying noise"){
    
    comparison <- data2
    
    while(!(something %in% as.vector(comparison$Course.City))){
      # loop while incorrect column
      
      cat("Enter a city")
      something <- readline(prompt="Input:")
      
      if (!(something %in% comparison$Course.City)) {
        print(unique(comparison$Course.City))
        print("That country was not valid, chose one of the printed above")
      }
    }
    return(comparison %>% filter(Course.City == something) %>% select("Create.Date",
                                                                      "Became.a.Customer.Date",
                                                                      "Contact.ID"))
  }
  
  # this function loops until you select a valid city, then filters the dataset 
  # the way we want it
  filter_IP_City <- function(dataframe, something = "random_anoying noise"){
    while(!(something %in% dataframe$IP.City)){
      # loop while incorrect column
      
      cat("Enter a city")
      something <- readline(prompt="Input:")
      
      if (!(something %in% dataframe$IP.City)) {
        print(unique(dataframe$IP.City))
        print("That city was not valid, chose one of the printed above")
      }
    }
    return(dataframe %>%filter(IP.City == something) %>% select("Create.Date",
                                                                "Became.a.Customer.Date",
                                                                "Contact.ID"))
  }
  
  # this function loops until you select a valid city, then filters the dataset 
  # the way we want it
  filter_IP_Country <- function(dataframe, something = "random_anoying noise"){
    comparison <- dataframe
    while(!(something %in% comparison$IP.Country)){
        # loop while incorrect column
      
        cat("Enter a city")
      something <- readline(prompt="Input:")
         
        if (!(something %in% comparison$IP.Country)) {
          print(unique(comparison$IP.Country))
          print("That country was not valid, chose one of the printed above")
        }
    }
    return(dataframe %>% filter(IP.Country == something) %>% select("Create.Date",
                                "Became.a.Customer.Date",
                                "Contact.ID"))
  }

  # filter_IP_City(data2) test that it works
  
  result <- switch(select_origin,
                   "1" = filter_nothing(dat),
                   "2" = filter_Course_City(dat),
                   "3" = filter_IP_City(dat),
                   "4" = filter_IP_Country(dat))
  
  
  lead_to_customer <-  result %>% select("Create.Date",
                                       "Became.a.Customer.Date",
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
  
  
  ##### AÑADIR! #####
  final_table <- final_table %>% mutate(customers = round(size * accumulated/100, digits = 0))
  final_table <- final_table[,c(1,2,length(final_table),3,4:(length(final_table)-1))]
  time <- Sys.time()
  time <- time %>% gsub(pattern = ":",replacement = "-") %>% gsub(pattern = " ",
                                                                  replacement = "_")
  mainDir <- getwd()
  subDir <- "cohort_retention_output"
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  setwd(file.path(mainDir, subDir))
  final_table <- final_table %>% na.replace(replace = "")
  write.csv(final_table, paste0(time,"_","cohort_retention_csv.csv"), row.names = F)
  write.xlsx(as.data.frame(final_table),
             file = paste0(time,"_","cohort_retention_excel.xlsx"),
             sheetName = "cohort", 
             col.names = FALSE, row.names = FALSE, append = FALSE)
  # No se porque no lo define bien
  setwd(mainDir)
  print(p)
  return(final_table)  

  
  ##### salvar en distintos directorios #####
  ##### preparar para excel: puntos por comas, csv con sep ; replace NA por empty
}
tabla <- cohort_retention(dat = data2)

a <- timestamp()

# write.csv(final_table, "matrix.csv", row.names = F)
# 
# getwd()
# 
# test_df <- data.frame(a = c(1,NA,3,4))
# 
# 
# test_df %>% na.replace(replace = "")
