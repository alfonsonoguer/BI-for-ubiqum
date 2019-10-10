
#loading the necessary libraries
pacman::p_load(tidyverse,lubridate,readr,reshape2,gridExtra,plotly,processx,webshot)


# data$`Became a Lead Date`
# # str(data)
# # names(data)
# "Became an Evangelist Date"
# "Became a Customer Date" 
# "Became an Opportunity Date" 
# "Became a Sales Qualified Lead Date" 
# "First Deal Created Date" 
# "Contact ID"  

lead_to_customer <-  data %>% select(`Became a Lead Date`,
                                      `Became a Customer Date`,
                                      "Contact ID")

names(lead_to_customer) <- c("lead","customer","ID")

lead_to_customer <- lead_to_customer %>% 
  filter(year(lead_to_customer$lead) >= 2018) %>% arrange(lead)

leads <- lead_to_customer %>% select(lead ,ID)
customers <- lead_to_customer %>% select(customer,ID) %>% na.omit()
names(leads) <- c("date","ID")
names(customers) <- c("date","ID")
to_matrix <- rbind(leads,customers)

to_matrix$date <- format(to_matrix$date, format = "%Y-%m")
to_matrix %>% arrange(date)
cohort <- to_matrix %>% #store data in table called cohort
  group_by(ID) %>% #group all the users/clients together
  mutate(first = min(date)) %>% #for every user/client take the first period
  group_by(first, date) %>% #group by this first period + other periods
  summarise(users = n()) %>% #for each combinations, count the number of users
  spread(date, users) #make columns with the period names
cohort[23,]




save.image(grid.table(cohort))
#align the table created a bove to the left like a normal 
# cohort table starting from column 2
shiftrow <- function(v){
  #put a vactor in, strip off leading NA values, 
  # and place that amount at the end
  first_na_index <- min(which(!is.na(v)))
  
  #return that bit to the end, and pad with NA's
  c(v[first_na_index:length(v)], rep(NA, first_na_index-1))
}

#create a new dataframe, with shifted rows(ad keep the first row)
shifted <- data.frame(
  cohort = cohort$first,
  t(apply(select(as.data.frame(cohort), 2:ncol(cohort)),
          #from the 2nd column to the end
          1, #for every row
          shiftrow
  ))
)



#make the column names readable
#first should be "cohort" and the rest dates

colnames(shifted) <-c("cohort",
                      "leads",
                      sub("","month.",
                          str_pad(1:(ncol(shifted)-2),2,
                                  pad = "0")))



#percentages
#we want every year and month to be expressed as a percentange
#create new table for this . We divide all year,month columns by the first year,
# month of that row
shifted_pct <- data.frame(
  cohort = shifted$cohort, #first row,
  leads = shifted$leads, #first row
  round(shifted[,3:nrow(shifted)] / shifted[["leads"]],4)*100 #rest:divide by year&month1
)

accumulated <- rowSums(shifted_pct %>% select(-(1:2)),na.rm = TRUE)

shifted_pct <- cbind(shifted_pct,accumulated)



######################################################################################################################
#                                             PLOTS
######################################################################################################################

#ggplot loves long data.Let's melt it. One for the absolute values, one for the pcts
plotdata_abs <- gather(shifted,     "cohort_age", "people"  ,2:ncol(shifted))
plotdata_pct <- gather(shifted_pct, "cohort_age", "percent" ,2:ncol(shifted_pct))

# now add some data.. we need pretty labels..
# first bit is the length of the width of the wide column (minus 1, that's the cohort name)
# that contains the absolute numbers
# last bit is the rest, those are percentages.
labelnames <- c( plotdata_abs$people[1:(ncol(shifted)-1)],
                 plotdata_pct$percent[(ncol(shifted)):(nrow(plotdata_pct))])

# we need pretty labels.
pretty_print <- function(n) {
  case_when( n <= 1  ~ sprintf("%1.0f %%", n*100),
             n >  1  ~ as.character(n),
             TRUE    ~ " ") # for NA values, skip the label
}

# create the plot data
plotdata <- data.frame(
  cohort     = plotdata_pct$cohort,
  cohort_age = plotdata_pct$cohort_age,
  percentage = plotdata_pct$percent,
  label      = labelnames
)

#plot (with reordered y axis, oldesr group on top)
p <- ggplot(plotdata, aes(x = cohort_age, y = reorder(cohort, desc(cohort)))) +
  geom_raster(aes(fill = percentage)) +
  #scale_fill_gradient(low = "white", high = "red") + coord_fixed() +
  scale_fill_continuous(guide = FALSE) + coord_equal(ratio = 1) + # no legend
  geom_text(aes(label = label), size = 4, color = "white") +
  xlab("cohort age") + ylab("cohort") + 
  ggtitle(paste("Retention table (cohort) for E-Commerse Business"))+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(p)


save.image(p)
