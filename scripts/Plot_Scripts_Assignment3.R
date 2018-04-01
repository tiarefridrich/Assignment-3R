library(tidyverse)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(dplyr)
install.packages("ggpubr")
library(ggpubr)
install.packages("Hmisc")
library(Hmisc) 

surveys <- read_csv("data/portal_data_joined.csv")
#remove the NAs
surveys_NAsrem <- surveys %>% 
  filter(!is.na(weight), !is.na(hindfoot_length), !is.na(sex), species_id != "")

#bind the plot to a specific data frame using the data argument
#then use GGplot to look at the data using plots
ggplot(data = surveys_NAsrem)

#mean weight of sex by species 
surveys_NAsrem %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight), max_weight = max(weight), min_weight = min(weight))

#Distribution of number of species throughout years - or specific species x 
#Number of species in each taxa in one year or over the years (only one taxa when take out NAs)
#The number of species in each plot type 
#Species, taxa, and years all together in one year;  two graphs in one year -  species and taxa through time 
#Weight change through time trend - in winter they eat less in spring they eat more 
#relationship between sex and weight - box plot 
#relationship between high foot length and species or sex x
#Look at the ranges of size 
#weight and hindfoot length trend between males and females x
#handoff length vs sex x
#mean weight and hindfoot length of different sex vs plot id 
#Number of individuals seen over a year, by month or over years to see trends x

#PLOT 1 JEEBAN
#Relationship between sex and hindfoot length
#creates two red outlined boxplotes with the mean being blue of sexes regardless of species
ggplot(surveys_NAsrem, aes(x=sex, y=hindfoot_length))+
  geom_boxplot(alpha = 1,color='blue')+xlab('Sex')+ylab('Length of hindfoot (inch)')+
  stat_summary(fun.y=mean, color="red", geom="point", shape=15, size=1,show_guide = T, show.legend = T) + theme_bw()

#PLOT 2 JEEBAN
#Number of individuals of each of the species classfied by sex
#shows the species ID on the x-axis and the number of individuals recorded on the y
#stacked bar chart with the number of individuals of each sex stacked 
#angles of the text on x-axis were changed 
ggplot(surveys_NAsrem, aes(species_id))+geom_bar(aes(fill=sex))+
  ylab('Number of individuals recorded')+xlab('Species ID')+theme(axis.text.x = element_text(angle = 90)) 

#PLOT 3 JEEBAN
#Correlationship between weight and hindfoot length using a scatterplot to find the linear relationship
#shows that it is significant, p<0.05
#correlation dots were changed from all dark black to circles that are just outlines
ggscatter(surveys_NAsrem, x = "weight", y = "hindfoot_length", color = "black", shape = 21, size = 2, add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",  xlab = "Hindfoot Length (inch)", ylab = "Weight (gram)")

#PLOT 4 JEEBAN
#Hindfoot length of different species
#Box plot of hindfoot length of different species. The blue dot indicates the average (mean) hindfoot length.
ggplot(surveys_NAsrem, aes(x = species_id, y = hindfoot_length)) +
  geom_boxplot(alpha=1, color= 'black')+xlab('Species ID')+ylab('Hindfoot length (inch)')+
  stat_summary(fun.y=mean, color="blue", geom="point",  shape=15, size=1,show_guide = T, show.legend = T)+
  theme(axis.text.x = element_text(angle = 90))

#PLOT 5 
#yearly counts, faceted and separated into species TIARE
ggplot(data = yearly_counts, aes(x=year, y=n, group = species_id, color = species_id)) + 
  geom_line() + facet_wrap(~ species_id) + theme_bw() +ylab('Number of Individuals') + xlab('Year')
#number of species found yearly MAMOON
yearly_counts <- surveys_NAsrem %>% group_by(year, species_id) %>% tally()
ggplot(year_counts, aes(x=year, y=n, color=species_id)) + geom_col()+facet_wrap(~species_id) 
#this one is filled in as opposed to a line, have to decide which one we like better 
                                                 
#PLOT 6 MAMOON
#number of species found in each plot type 
Plot_counts <- surveys_NAsrem %>% group_by(year, plot_type) %>% tally()
ggplot(Plot_counts, aes(x=year, y=n, color=plot_type)) + geom_point()+facet_wrap(~plot_type)+
  theme(axis.text.x = element_text(angle = 90))
#switched to point instead of column


#PLOT 7 MAMOON ************* PROBLEMS
#males and females average weight yearly
spe_meanweight <- surveys_NAsrem %>% group_by(year, species_id, sex) %>%
  summarize(mean_weight = mean(weight)) #this doesnt work 
ggplot(spe_meanweight, aes(x=year, y=mean_weight, color=sex)) +
  geom_line()+facet_wrap(~species_id) + theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(axis.text.x = element_text(colour = "grey20", size = 10, angle = 90, hjust = 0.5, vjust = 0.5))

#PLOT 8 
#Count of species observed 
ggplot(surveys_NAsrem) + stat_count(aes(x = species_id, fill = species_id)) #just number of individuals of each species

#PLOT 9
ggplot(surveys_NAsrem) + stat_count(aes(x = species_id, fill = sex), position = "dodge") #number of individuals of each species by sex, already have this one over time 

#PLOT 10
#relationship between species weight and sex
ggplot(surveys_NAsrem) + geom_boxplot(aes(x = species_id, y = weight, fill = sex)) +
  coord_flip()

ggplot(surveys_NAsrem) + geom_boxplot(aes(x = species_id, y = weight, fill = sex)) + facet_wrap(~species_id)

#PLOT 11 
#these are two ways of showing the same thing: 
#the number of individuals surveys depending on the month of the year
#I prefer the first one
ggplot(data = species_by_month, aes(x=month, y=n, group = species_id, color = species_id, group = species_id)) + 
  geom_line() +ylab('Number of Individuals')+ xlab('Month of the Year')
ggplot(data = species_by_month, aes(x=month, y=n, group = species_id, color = species_id, 
                                    group = species_id)) + geom_line() + facet_wrap(~ species_id)

#PLOT 12
#Number of individuals seen per year, separated by sex 
yearly_sex_counts <- surveys_NAsrem %>% group_by(year, species_id, sex) %>%
  tally
ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = sex, group = sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  theme_bw() +ylab('Number of Individuals') + xlab('Year')+ theme(axis.text.x = element_text(angle = 90))


