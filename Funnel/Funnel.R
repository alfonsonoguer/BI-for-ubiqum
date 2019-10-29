pacman::p_load(tidyverse,lubridate)

# the data we get from the main project

# first level "Create.Date"
# second level "Became.a.Marketing.Qualified.Lead.Date"
# third level "Became.a.Sales.Qualified.Lead.Date" 
# fourth level "Became.an.Opportunity.Date"
# fifth level "Became.a.Customer.Date" 

# first filter "Original.Source" 
nombres_categorias <- c("Create.Date",
                        "Became.a.Marketing.Qualified.Lead.Date",
                        "Became.a.Sales.Qualified.Lead.Date",
                        "Became.an.Opportunity.Date",
                        "Became.a.Customer.Date")
results_df <- data.frame(Categories = nombres_categorias,
                         Amount = rep(0,5),
                         Proportion = c(100,rep(0,4)))
# here we select the data to use.
data_funnel <-  data

for(i in nombres_categorias){
  data_funnel[,i] <- as.Date(data_funnel[,i])
}

# pre process -------------------------------------------------------------
# select origin
filtro <- "gibberish"
while(!(filtro %in% data_funnel$Original.Source) & filtro != "0"){ # start
  # loop while incorrect value
  print("options:")
  print(as.character(unique(data_funnel$Original.Source)))
  cat("Write name of original source(0:no filter) \n")
  filtro <- readline(prompt="Filter:")
}
if (filtro == 0) {
  funnel <-  data_funnel
}else{funnel <-  data_funnel %>% filter(Original.Source == filtro)}
# funnel$Original.Source %>% table()
# select date interval 

cat("select starting date format dd-mm-yyyy \n")
start_date <- readline(prompt="start date:") %>% dmy()
cat("select final date format dd-mm-yyyy \n")
final_date <- readline(prompt="final date:") %>% dmy()




# funnel calculation ------------------------------------------------------

date <-  which(funnel$Create.Date > start_date &
                 funnel$Create.Date < final_date)


results_df[1,2] <- funnel[date,] %>% nrow()


results_df[2,2] <-  sum(!is.na(funnel$Became.a.Marketing.Qualified.Lead.Date[date]))


results_df[3,2] <- sum(!is.na(funnel$Became.a.Sales.Qualified.Lead.Date[date]))


results_df[4,2] <- sum(!is.na(funnel$Became.an.Opportunity.Date[date]))



results_df[5,2] <- sum(!is.na(funnel$Became.a.Customer.Date[date]))

# users <- funnel %>%  filter(identifier_users == users) %>% 
#   filter(Became.a.Customer.Date > start_date) %>%
#   select(Identifier_users)

for (i in 2:5){
  results_df[i,3] <- (results_df[i,2]/results_df[1,2])*100
}
results_df[,3] <- round(results_df[,3],2)


funnel_plot <- ggplot(data=results_df,
                      aes(x=reorder(Categories, -Amount), y=Amount)) +
  geom_bar(stat="identity",aes( fill= Categories)) +
  scale_fill_manual(values = 
                      c("#ed553b",
                        "#20639B",
                        "#3caea3",
                        "#f6d55c",
                        "#173F5F")) +
  geom_text(aes(label=Proportion), vjust=-0.3, size=3.5)+
  theme_minimal() + 
  labs(x =  element_blank(),title = paste(filtro,start_date,final_date)) +
  theme(axis.text.x = element_text(angle = -25, hjust = 0.1))
funnel_plot

mainDir <- getwd()
subDir <- "funnel_plots"
dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
setwd(file.path(mainDir, subDir))

ggsave(filename = paste0(filtro,"_from_",start_date,"_to_",final_date,"_at_",Sys.Date(),".png"))

# No se porque no lo define bien
setwd(mainDir)
