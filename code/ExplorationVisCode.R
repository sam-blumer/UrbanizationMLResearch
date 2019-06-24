install.packages("sparklyr")
install.packages("dbplyr")
install.packages("DBI")
library(sparklyr)
library(dplyr)
library(dbplyr)
library(DBI)
library(purrr)
library(tidyr)
library(ggplot2)
library(reshape2)

options(scipen = 999)

#DATA EXPLORATION
###With the file cleaned and assembled, we can begin to explre the data itself.


#Explore populations of rural and urban areas over time (2010-2018)

par(mfrow=c(2,1))

county_char$Metro2013[county_char$Metro2013 == 1] <- "Urban" 
county_char$Metro2013[county_char$Metro2013 == 0] <- "Rural"

pops_type <- merge(pops, county_char)

#isolating time series values
series <- pops_type %>% group_by(Metro2013) %>% 
  summarise_at(3:11, sum)

#filter by urban areas
urban_series <- pops_type %>% group_by(Metro2013) %>% 
  filter(Metro2013 == "Urban") %>% 
  summarise_at(3:11, sum)

#filter by rural areas
rural_series <- pops_type %>% group_by(Metro2013) %>% 
  filter(Metro2013 == "Rural") %>% 
  summarise_at(3:11, sum)

#melt data series from above
rural <- melt(rural_series)
urban <- melt(urban_series)

#create time series
rts <- ts(rural$value, start=c(2010), end=c(2018), frequency=1)  
uts <- ts(urban$value, start=c(2010), end=c(2018), frequency=1)

#melt data frame for viz
meltdf <- melt(series)

#plot rural time series
rts_plot <- plot((rts/1000000), main="Rural Populations Over Time",
                 xlab="", ylab="Population in Millions")

#plot urban time series
uts_plot <- plot((uts/1000000), main="Urban Populations Over Time",
                 xlab="", ylab="Population in Millions")


meltts <- ggplot(meltdf) +
  geom_bar(aes(x = str_remove(variable, "pop"), y = (value/1000000), fill = Metro2013),
           stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#468189", "#9DBEBB")) +
  theme_classic(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, vjust = 1))+
  labs(subtitle="Population Totals", 
       y="Population in Millions", 
       x="Year", 
       title="Rural vs Urban", 
       caption = "Source: USDA Economic Research Service")+
  theme(legend.title = element_blank())


rts_plot
uts_plot
meltts


###################
###RURAL POP DENSITY###

#filter populations and create density columns for rural areas

pop_density_rural <- county_char %>% 
  group_by(State, Metro2013) %>%
  filter(Metro2013 == 'Rural') %>% 
  mutate(area = sum(LandAreaSQMiles2010)) %>% 
  mutate(total_pop = sum(TotalPopEst2011)) %>% 
  mutate(density = sum(total_pop/area)) %>% 
  select(State, area, Metro2013, total_pop, density)

#unqiue columns
pop_density_rural <- unique(pop_density_rural)


#plot rural pop density by state
ggrural <- ggplot(pop_density_rural, aes(x=area, y=(total_pop/1000000))) + 
  geom_point(aes(col=density, size=density)) + 
  geom_text(aes(label=ifelse(area>100000,as.character(State),'')),hjust=0,vjust=0) +
  geom_text(aes(label=ifelse(total_pop>1500000,as.character(State),'')),hjust=0,vjust=0) +
  geom_smooth(method="loess", se=F) + 
  labs(subtitle="Area Vs Population", 
       y="Population Totals in Millions", 
       x="Area Totals (Sq Miles)", 
       title="Rural Population Density", 
       caption = "Source: USDA Economic Research Service")


#########################

###URBAN POP DENSITY###

#filter populations and create density columns for urban areas
pop_density_urban <- county_char %>% 
  group_by(State, Metro2013) %>%
  filter(Metro2013 == 'Urban') %>% 
  mutate(area = sum(LandAreaSQMiles2010)) %>% 
  mutate(total_pop = sum(TotalPopEst2011)) %>% 
  mutate(density = sum(total_pop/area)) %>% 
  select(State, area, Metro2013, total_pop, density)

#unqique rows
pop_density_urban <- unique(pop_density_urban)


##plot urban pop density by state
ggurban <- ggplot(pop_density_urban, aes(x=area, y=(total_pop/1000000))) + 
  geom_point(aes(col=density, size=density)) + 
  geom_text(aes(label=ifelse(area>25000,as.character(State),'')),hjust=0,vjust=0) +
  geom_text(aes(label=ifelse(total_pop>15000000,as.character(State),'')),hjust=0,vjust=0) +
  geom_smooth(method="loess", se=F) + 
  labs(subtitle="Area Vs Population", 
       y="Population in Millions", 
       x="Area Totals (Sq Miles)", 
       title="Urban Population Density", 
       caption = "Source: USDA Economic Research Service")


#########################


###OVERALL POP DENSITY###

pop_density <- county_char %>% 
  group_by(State, Metro2013) %>%
  mutate(area = (LandAreaSQMiles2010)) %>% 
  mutate(total_pop = (TotalPopEst2011)) %>% 
  mutate(density = (total_pop/area)) %>% 
  select(State, area, Metro2013, total_pop, density)

pop_density <- unique(pop_density)

library(ggplot2)
gg <- ggplot(pop_density, aes(x=area, y=(total_pop/1000000))) + 
  geom_point(aes(col=Metro2013, size=density)) + 
  geom_text(aes(label=ifelse(area>150000,as.character(State),'')),hjust=0,vjust=0) +
  geom_text(aes(label=ifelse(total_pop>15000000,as.character(State),'')),hjust=0,vjust=0) +
  geom_smooth(method="loess", se=F) + 
  xlim(0,50000) +
  ylim(0,5.5) +
  labs(subtitle="Urban vs Rural by State Total", 
       y="Population in Millions", 
       x="Area Totals (Sq Miles)", 
       title="Population Density", 
       caption = "Source: USDA Economic Research Service")+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position="top")


#show all pop density plots
plot(ggrural)
plot(ggurban)
plot(gg)




##################
######DATA EXPLORATION############

#First we want to explore how some of our metrucs relate to Rural areas:

#convert necessary columns to factors
names <- c(1,
           4:5,
           8:10,
           14,
           53:57)
county_char[,names] <- lapply(county_char[,names] , factor)

#group columns by rural/urban and get mean for all numeric columns
metro_comp <- county_char %>% group_by(Metro2013) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

#filter our rural
Rural <- metro_comp %>% 
  filter(Metro2013 == "Rural")

#filter our urban
Urban <- metro_comp %>% 
  filter(Metro2013 == "Urban")

#How have populations in Urban and Rural areas changed between 2000 and 2010

#pop differences (%) between urban and rural over time
ggplot(metro_comp) +
  geom_bar(aes(x = Metro2013, y = PopChangeRate0010, fill = Metro2013),
           stat = "identity", position = "dodge") +
  labs(subtitle="Urban vs Rural Areas", 
       y="Percent Change", 
       x="County Type", 
       title="Population Change 2000 - 2010", 
       caption = "Source: USDA Economic Research Servic")+
  theme(legend.title = element_blank())


#melt data for plots
metro_melt <- melt(metro_comp)

#################WORK TYPES########################
melt_work_types <- metro_melt[65:84,]

workplot <- ggplot(melt_work_types) +
  geom_bar(aes(x = variable, y = value, fill = Metro2013),
           stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#468189", "#9DBEBB")) +
  theme_classic(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, vjust = 1)) +
  labs(subtitle="Urban vs Rural", 
       y="Percent Employment", 
       x="Employment Type", 
       title="Employment Type", 
       caption = "Source: US Bureau of Economic Analysis")+
  theme(legend.title = element_blank())

#################EDUCATION TYPES########################
melt_edu_types <- metro_melt[47:56,]


eduplot <- ggplot(melt_edu_types) +
  geom_bar(aes(x = variable, y = value, fill = Metro2013),
           stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#468189", "#9DBEBB")) +
  theme_classic(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, vjust = 1)) +
  labs(subtitle="Urban vs Rural", 
       y="Percent Attainment", 
       x="Education Level", 
       title="Education Attainment Level", 
       caption = "Source: US Bureau of Economic Analysis")+
  theme(legend.title = element_blank())

#################ETHNICITY########################

melt_com_types <- metro_melt[9:20,]

complot <- ggplot(melt_com_types) +
  geom_bar(aes(x = variable, y = value, fill = Metro2013),
           stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#468189", "#9DBEBB")) +
  theme_classic(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, vjust = 1)) +
  labs(subtitle="Urban vs Rural", 
       y="Percent of County", 
       x="Identity", 
       title="Ethnic Make-Up", 
       caption = "Source: US Bureau of Economic Analysis")+
  theme(legend.title = element_blank())


##################POVERTY TYPES#######################

melt_pov_types <- metro_melt[57:64,]

povplot <- ggplot(melt_pov_types) +
  geom_bar(aes(x = variable, y = value, fill = Metro2013),
           stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#468189", "#9DBEBB")) +
  theme_classic(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, vjust = 1)) +
  labs(subtitle="Urban vs Rural", 
       y="Poverty Rate", 
       x="Poverty Type", 
       title="Poverty Levels", 
       caption = "Source: US Bureau of Economic Analysis")+
  theme(legend.title = element_blank())

#show all plots
workplot
eduplot
complot
povplot

#########################################
##################HH INCOME#######################

melt_income_types <- metro_melt[35:36,]

incomeplot <- ggplot(melt_income_types) +
  geom_bar(aes(x = variable, y = value, fill = Metro2013),
           stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#468189", "#9DBEBB")) +
  theme_classic(base_size = 18) +
  theme(axis.text.x = element_text(angle = 0, 
                                   hjust = .5, vjust = 0),
        axis.line = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(subtitle="Urban vs Rural", 
       y="Household Income (USD)", 
       x="", 
       title="Household Income", 
       caption = "Source: US Bureau of Economic Analysis")+
  theme(legend.title = element_blank())

#########################################
#Plot education level by poverty rate
# Basic scatter plot
# Change the point size, and shape

corplot <- ggplot(county_char, aes(x=Pov.All, y=HSOnly, color=Metro2013)) +
  geom_point()+
  labs(subtitle="Urban vs Rural", 
       y="% High School Diploma Only", 
       x="Poverty Rate", 
       title="High School Diploma by Poverty Rate", 
       caption = "Source: US Bureau of Economic Analysis")+
  theme(legend.title = element_blank())

grid.arrange(incomeplot, corplot, ncol=2)
##########################3333

##########Count of Low Edu and Low Emp by County Type##############


low_edu <- county_char %>% group_by(Metro2013) %>% 
  summarise(Low.Education = sum(as.numeric(Low.Education)))

low_emp <- county_char %>% group_by(Metro2013) %>% 
  summarise(Low.Employment = sum(as.numeric(Low.Employment)))


low_e <- merge(low_edu, low_emp, by = "Metro2013")

low_e <- melt(low_e)


ggplot(low_e) +
  geom_bar(aes(x = variable, y = value, fill = Metro2013),
           stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#468189", "#9DBEBB")) +
  theme_classic(base_size = 18) +
  theme(axis.text.x = element_text(angle = 0, 
                                   hjust = .5, vjust = 0),
        axis.line = element_blank(),
        axis.ticks.x = element_blank())+
  labs(subtitle="Urban vs Rural", 
       y="Number of Counties", 
       x="", 
       title="Low Education and Employment", 
       caption = "Source: US Bureau of Economic Analysis")+
  theme(legend.title = element_blank())

