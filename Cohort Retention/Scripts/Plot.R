
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