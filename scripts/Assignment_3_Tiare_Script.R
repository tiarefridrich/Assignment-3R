library(tidyverse)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(dplyr) 

#######################################################
# The file (portal_mammals.sqlite) has been downloaded manually

# Connect to the portal SQLite database
mammals <- DBI::dbConnect(RSQLite::SQLite(), "data/portal_mammals.sqlite")

# Information about database
src_dbi(mammals)

# Reading tables from database
surveys <- tbl(mammals, "surveys")
plots <- tbl(mammals, "plots")
species <- tbl(mammals, "species")
surveys
plots
species
######################################################

surveys <- read_csv("data/portal_data_joined.csv")
#remove the NAs
surveys_NAsrem <- surveys %>% 
  filter(!is.na(weight), !is.na(hindfoot_length), !is.na(sex), species_id != "")
View(surveys_NAsrem)

sex <- surveys_NAsrem$sex
levels(sex) <- c("missing", "Female", "Male")

surveys$date <- ymd(paste(surveys$year,surveys$month,surveys$day,sep="-"))
View(surveys_NAsrem)

#to select columns, use select()
#to look at rows, use filter()

#to look at those individuals with weights less than 5g 
surveys_small <- surveys_NAsrem %>%
  filter(weight < 5) %>%
  select(species_id, sex, weight)

#changed the weight from weight in g to kg
surveys_NAsrem %>% mutate(weight_kg = weight / 1000)

#the split-apply-combine technique is used with the group_by() function
#for example to view the mean weight by sex by species: 

surveys_NAsrem %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight), max_weight = max(weight), min_weight = min(weight))

#numbers of observations found in each factor
surveys_NAsrem %>% group_by(sex) %>% tally()

#to save the new datasets and share them, use write.csv()

View(surveys_NAsrem)
write.csv(surveys_NAsrem, file="data_output/surveys_NAsrem.csv", row.names=FALSE)

#bind the plot to a specific data frame using the data argument
ggplot(data = surveys_NAsrem)

#define aesthetics that maps variables in data to axes on the plot
#or to plotting size, shape color, etc.
#add geoms - geographical representation of the data in plot points 
ggplot(data = surveys_NAsrem, aes(x=weight, y=hindfoot_length)) + geom_point()
#you can create a plot and assign it to a variable and then just play around with it with the 
# + geom_point() + function

#make it transparent and blue 
ggplot(data = surveys_NAsrem, aes(x=weight, y=hindfoot_length)) + geom_point(alpha = 0.1, color = "blue")

#make it transparent and each species a different color
#then make it a plot template to play around with or save
plot_species_weightvshindfoot <- ggplot(data = surveys_NAsrem, aes(x=weight, y=hindfoot_length)) + geom_point(alpha = 0.1, aes(color = species_id))

#distribution of weight within each species 
weightbyspecies_boxpot <- ggplot(data = surveys_NAsrem, aes(x = species_id, y = hindfoot_length)) +
  geom_boxplot()
# -->> what if we looked at it on a taxa level (question for later)

#plotting time series data 
#number of counts per year per each species 
#group data and then count the records, assign to yearly_counts
yearly_counts <- surveys_NAsrem %>% group_by(year, species_id) %>% tally()
#then look at it using line plots in ggplot, make sure to group by species 
#so that it is separated by species
#assign this plot to a variable 
yearlycounts_linepot <- ggplot(data = yearly_counts, aes(x=year, y=n, group = species_id, color = species_id)) + 
  geom_line() + facet_wrap(~ species_id)

#look at the time series of each species by month, to see if any of the species are seasonal
species_by_month <- surveys_NAsrem %>% group_by(month, species_id) %>% tally() 
#this one shows them all in the same graph
ggplot(data = species_by_month, aes(x=month, y=n, group = species_id, color = species_id, group = species_id)) + geom_point()
#this one shows them all separated by species 
ggplot(data = species_by_month, aes(x=month, y=n, group = species_id, color = species_id, group = species_id)) + geom_line() + facet_wrap(~ species_id)

#faceting: split plots by sex 
yearly_sex_counts <- surveys_NAsrem %>% group_by(year, species_id, sex) %>%
  tally
yearly_sexcounts_lineplot <- ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = sex, group = sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  theme_bw()

