knitr::opts_chunk$set(fig.width=12, fig.height=8) 


#INSTALL PACKAGES
install.packages("sparklyr",repos = c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
install.packages("dbplyr",repos = c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
install.packages("DBI",repos = c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
install.packages("standardize",repos = c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
install.packages("Matrix",repos = c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
install.packages("psycho",repos = c("http://rstudio.org/_packages", "http://cran.rstudio.com"), dependencies = TRUE)
install.packages("shiny",repos = c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
install.packages("reshape2",repos = c("http://rstudio.org/_packages", "http://cran.rstudio.com"), dependencies = TRUE)
install.packages("grid",repos = c("http://rstudio.org/_packages", "http://cran.rstudio.com"), dependencies = TRUE)
library(grid)
library(gridExtra)
library(shiny)
library(knitr)
library(standardize)
library(psycho)
library(tidyverse)
library(sparklyr)
library(caret)
library(dplyr)
library(dbplyr)
library(DBI)
library(ggplot2)
library(randomForest)
library(naniar)
library(reshape2)



#Spark installation and configuration
spark_install(version = "2.1.0")

conf <- spark_config()
conf$`sparklyr.cores.local` <- 2
conf$`sparklyr.shell.driver-memory` <- "12G"
conf$spark.memory.fraction <- 0.9


#start local spark instance
sc <- spark_connect(master = "local", version = "2.1.0", config = conf)


#Load Data insto Spark

access2010 <- spark_read_csv(sc, name = 'access2010', path = 'C:/Users/blume/Desktop/practicum/data/2010/access2010.csv', header = TRUE, delimeter = ',', stringsAsFactors = FALSE)
assistance2010 <- spark_read_csv(sc, name = 'assistance2010', path = 'C:/Users/blume/Desktop/practicum/data/2010/assistance2010.csv', header = TRUE, delimeter = ',', stringsAsFactors = FALSE)
health2010 <- spark_read_csv(sc, name = 'health2010', path = 'C:/Users/blume/Desktop/practicum/data/2010/health2010.csv', header = TRUE, delimeter = ',', stringsAsFactors = FALSE)
insecurity2010 <- spark_read_csv(sc, name = 'insecurity2010', path = 'C:/Users/blume/Desktop/practicum/data/2010/insecurity2010.csv', header = TRUE, delimeter = ',', stringsAsFactors = FALSE)
local2010 <- spark_read_csv(sc, name = 'local2010', path = 'C:/Users/blume/Desktop/practicum/data/2010/local2010.csv', header = TRUE, delimeter = ',', stringsAsFactors = FALSE)
restaurants2010 <- spark_read_csv(sc, name = 'restaurants2010', path = 'C:/Users/blume/Desktop/practicum/data/2010/restaurants2010.csv', header = TRUE, delimeter = ',', stringsAsFactors = FALSE)
socioeconomic2010 <- spark_read_csv(sc, name = 'socioeconomic2010', path = 'C:/Users/blume/Desktop/practicum/data/2010/socioeconomic2010.csv', header = TRUE, delimeter = ',', stringsAsFactors = FALSE)
store2010 <- spark_read_csv(sc, name = 'store2010', path = 'C:/Users/blume/Desktop/practicum/data/2010/store2010.csv', header = TRUE, delimeter = ',', stringsAsFactors = FALSE)
populations <- spark_read_csv(sc, name = 'populations', path = 'C:/Users/blume/Desktop/practicum/data/county_populations.csv', header = TRUE, delimeter = ',', stringsAsFactors = FALSE)



#Load smaller files directly into R

food_desert <- read.csv("C:/Users/blume/Desktop/practicum/data/fooddesert.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
pops <- read.csv('C:/Users/blume/Desktop/practicum/data/county_populations.csv', header = TRUE, sep = ',', stringsAsFactors = FALSE)
county_char <- read.csv("C:/Users/blume/Desktop/practicum/data/county_chars.csv", header = TRUE, sep = ',', stringsAsFactors = FALSE)


#DATA PRE-PROCESSING


#change columns names
colnames(food_desert)[colnames(food_desert)=="ï..State"] <- "State"
colnames(county_char)[colnames(county_char)=="ï..FIPS"] <- "FIPS"
names(pops) <- c("State", "County", "pop2010", "pop2011", "pop2012", "pop2013", "pop2014", "pop2015", "pop2016", "pop2017", "pop2018")


#isolate necessary values from food desert file
food_desert_factor <- food_desert %>% select(State, County, Desert)

#remove uncessary columns
county_char$Continuum_Description <- NULL

#merge all files from Spark based off mathching FIPS, State and County
output2010 <- Reduce(function(...) merge(..., all=TRUE), list(access2010, assistance2010, health2010, insecurity2010, local2010, restaurants2010, socioeconomic2010, store2010))

#Data type corrections
output2010$FIPS <- as.character(output2010$FIPS)
output2010$METRO <- as.factor(output2010$METRO)
output2010$SNAP_OAPP <- as.factor(output2010$SNAP_OAPP)
output2010$SNAP_FACEWAIVER <- as.factor(output2010$SNAP_FACEWAIVER)
output2010$SNAP_VEHEXCL <- as.factor(output2010$SNAP_VEHEXCL)
output2010$SNAP_BBCE <- as.factor(output2010$SNAP_BBCE)
output2010$SNAP_REPORTSIMPLE <- as.factor(output2010$SNAP_REPORTSIMPLE)
output2010$FOODHUB <- as.factor(output2010$FOODHUB)
output2010$FARM_TO_SCHOOL <- as.factor(output2010$FARM_TO_SCHOOL)
output2010$PERPOV <- as.factor(output2010$PERPOV)
output2010$PERCHLDPOV <- as.factor(output2010$PERCHLDPOV)


#remove Puerto Rican counties
output2010 <- subset(output2010, nchar(as.character(State)) <= 2)


#MISSING DATA FIX


output2010[is.na(output2010)] <- 0

output2010 <- na.omit(output2010)

#Merge spark flat file to local food desert file and turn NAs into 0s
outfd <- merge(output2010, food_desert_factor, by = c("State", "County"), all.x = TRUE)



outfd$Desert[is.na(outfd$Desert)] <- 0

#convert remaining data types

outfd$MEDHHINC <- as.numeric(outfd$MEDHHINC)
outfd$CHILDPOVRATE <- as.numeric(outfd$CHILDPOVRATE)
outfd$POVRATE <- as.numeric(outfd$POVRATE)


#Missing data check
  
vis_miss(outfd)
final <- na.omit(outfd)



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

########################
#MODEL PREPARATION

#create formual for model evaluation
vars <- paste("",colnames(final[,4:62]),sep="")
fla <- paste("Desert ~", paste(vars, collapse="+"))
as.formula(fla)

#use standardize to scale all numeric values in a subset

z_outfd <- dplyr::select_if(final, is.numeric) %>% 
  psycho::standardize() #standardize numeric variables

nonnum <- final %>% select_if(negate(is.numeric)) #separate all non-numeric values
z_outfd <- as.data.frame(z_outfd) 
nonnum <- as.data.frame(nonnum)
scaled_final <- cbind(nonnum, z_outfd) # join dataframe back together after scaling

#RANDOM FOREST FEAUTRE EXPLORATION 
#FEATURE ENGINEERING
#get feature importance
set.seed(123)
fit_rf = randomForest(as.formula(fla), data=scaled_final, importance = TRUE, na.action = na.roughfix) #fit the model using the formula created above


# Create an importance based on mean decreasing gini

varImp <- varImp(fit_rf)
varImp
impplot <- varImpPlot(fit_rf)
#results
fit_rf


#^^^^THIS IS FOR VARIABLE IMPORTANCE __ NOT MAKING ANY PREDICTIONS

#shows random forest plots 
plot(randomForest(as.formula(fla), data=scaled_final, keep.forest=FALSE, ntree=100), log="y")


#copy the scaled data frameback to spark
scaled_final = sdf_copy_to(sc, scaled_final, "scaled_final", overwrite = TRUE)


#create testing and training data sets
set.seed(123)
partitions <-  scaled_final %>%
  sdf_random_split(training = 0.75, test = 0.25, seed = 123)

scaled_final_training <- partitions$training
scaled_final_test <- partitions$test



#fit the first probit model
mysparkprobit <- scaled_final_training %>% 
  ml_logistic_regression(fla, data = scaled_final, family = "binomial",  maxit = 100)

#make predictions
pred <- ml_predict(mysparkprobit, scaled_final_test,  type="response") 


#view predictions in liklihood of being a food desert order
predsvis <- pred %>% arrange(desc(prediction)) %>% 
  select(State, County, FIPS, Desert, prediction, probability_0, probability_1, METRO) %>% 
  collect()



#SPARK FEATURE IMPORTANCE
set.seed(123)
spark_decision_tree <- ml_decision_tree(scaled_final_training, fla) #train model

spark_decision_tree #view model

feat_importance <- ml_tree_feature_importance(spark_decision_tree) #variable stores results

feat_importance #view results

sum(feat_importance$importance[1:10])


#create multiple models for evaluation

set.seed(123)
## Logistic Regression
ml_log <- mysparkprobit #this was our baseline from earlier

## Decision Tree
ml_dt <- ml_decision_tree(scaled_final_training, fla)

## Random Forest
ml_rf <- ml_random_forest(scaled_final_training, fla)

## Gradient Boosted Tree
ml_gbt <- ml_gradient_boosted_trees(scaled_final_training, fla)



#Score the test data with the trained models.

ml_models <- list(
  "Logistic" = ml_log,
  "Decision Tree" = ml_dt,
  "Random Forest" = ml_rf,
  "Gradient Boosted Trees" = ml_gbt
)

# Create a function for scoring
score_test_data <- function(model, data=scaled_final_test){
  pred <- ml_predict(model, data)
  select(pred, Desert, prediction)
}

# Score all the models
ml_score <- lapply(ml_models, score_test_data)


#ml_score


###Model lift
Lift compares how well the model predicts food deserts compared to random guessing. Use the function below to estimate model lift for each scored decile in the test data. The lift chart suggests that the tree models (random forest, gradient boosted trees, or the decision tree) will provide the best prediction.


# Lift function
calculate_lift <- function(scored_data) {
  scored_data %>%
    mutate(bin = ntile(desc(prediction), 10)) %>% 
    group_by(bin) %>% 
    summarize(count = sum(Desert)) %>% 
    mutate(prop = count / sum(count)) %>% 
    arrange(bin) %>% 
    mutate(prop = cumsum(prop)) %>% 
    select(-count) %>% 
    collect() %>% 
    as.data.frame()
}

# Initialize results
ml_gains <- data.frame(bin = 1:10, prop = seq(0, 1, len = 10), model = "Base")

# Calculate lift
for(i in names(ml_score)){
  ml_gains <- ml_score[[i]] %>%
    calculate_lift %>%
    mutate(model = i) %>%
    rbind(ml_gains, .)
}

# Plot results
lift_all_vars <- ggplot(ml_gains, aes(x = bin, y = prop, colour = model)) +
  geom_point() + geom_line() +
  labs(subtitle="All Variables Included", 
       y="", 
       x="", 
       title="Lift Chart for Predicting Food Deserts")





#calculate accuracy
calc_accuracy <- function(data, cutpoint = 0.5){
  data %>% 
    mutate(prediction = if_else(prediction > cutpoint, 1.0, 0.0)) %>%
    ml_multiclass_classification_evaluator("prediction", "Desert", "accuracy")
}

# Calculate AUC and accuracy
perf_metrics <- data.frame(
  model = names(ml_score),
  AUC = 100 * sapply(ml_score, ml_binary_classification_evaluator, "Desert", "prediction"),
  Accuracy = 100 * sapply(ml_score, calc_accuracy),
  row.names = NULL, stringsAsFactors = FALSE)

# Plot results
acc_all_vars <- gather(perf_metrics, metric, value, AUC, Accuracy) %>%
  ggplot(aes(reorder(model, value), value, fill = metric)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() +
  labs(y="Accuracy Rate", 
       x="Model", 
       title="Performance Metrics - All Variables")+
  theme(legend.title = element_blank())


#adapted from
#https://cfss.uchicago.edu/notes/sparklyr/



#crate formula using only the top ten features from our feature engineering above  
set.seed(123)

head(feat_importance[1:10,])
#updated formula
vars1 = feat_importance$feature[1:10]
sum(feat_importance$importance[1:10])
vars1 <- replace(vars1, vars1=="SNAP_OAPP_0", "SNAP_OAPP")
vars1 <- replace(vars1, vars1=="SNAP_OAPP_1", "SNAP_OAPP")
fla1 <- paste("Desert ~", paste(vars1, collapse="+"))
as.formula(fla1)

##############################UPDATED FEATURE MODELS##########################################
##############################################################################################
##############################################################################################


#https://cfss.uchicago.edu/notes/sparklyr/
set.seed(123)
## Logistic Regression
ml_log_ftr <- ml_logistic_regression(scaled_final_training, fla1)

## Decision Tree
ml_dt_ftr <- ml_decision_tree(scaled_final_training, fla1)

## Random Forest
ml_rf_ftr <- ml_random_forest(scaled_final_training, fla1)

## Gradient Boosted Tree
ml_gbt_ftr <- ml_gradient_boosted_trees(scaled_final_training, fla1)



#Score the test data with the trained models.

ml_models_ftr <- list(
  "Logistic" = ml_log_ftr,
  "Decision Tree" = ml_dt_ftr,
  "Random Forest" = ml_rf_ftr,
  "Gradient Boosted Trees" = ml_gbt_ftr
)

# Create a function for scoring
score_test_data_ftr <- function(model, data=scaled_final_test){
  pred <- ml_predict(model, data)
  select(pred, Desert, prediction)
}

# Score all the models
ml_score_ftr <- lapply(ml_models_ftr, score_test_data_ftr)


#ml_score_ftr


###Model lift
Lift compares how well the model predicts food deserts compared to random guessing. Use the function below to estimate model lift for each scored decile in the test data. The lift chart suggests that the tree models (random forest, gradient boosted trees, or the decision tree) will provide the best prediction.


# Lift function
#https://cfss.uchicago.edu/notes/sparklyr/

calculate_lift <- function(scored_data) {
  scored_data %>%
    mutate(bin = ntile(desc(prediction), 10)) %>% 
    group_by(bin) %>% 
    summarize(count = sum(Desert)) %>% 
    mutate(prop = count / sum(count)) %>% 
    arrange(bin) %>% 
    mutate(prop = cumsum(prop)) %>% 
    select(-count) %>% 
    collect() %>% 
    as.data.frame()
}

# Initialize results
ml_gains_ftr <- data.frame(bin = 1:10, prop = seq(0, 1, len = 10), model = "Base")

# Calculate lift
for(i in names(ml_score_ftr)){
  ml_gains_ftr <- ml_score_ftr[[i]] %>%
    calculate_lift %>%
    mutate(model = i) %>%
    rbind(ml_gains_ftr, .)
}

# Plot results
lift_ten_vars <- ggplot(ml_gains_ftr, aes(x = bin, y = prop, colour = model)) +
  geom_point() + geom_line() +
  labs(subtitle="Top 10 Variables", 
       y="", 
       x="", 
       title="Lift Chart for Predicting Food Deserts", 
       caption = "Source: https://cfss.uchicago.edu/notes/sparklyr/")






calc_accuracy <- function(data, cutpoint = 0.5){
  data %>% 
    mutate(prediction = if_else(prediction > cutpoint, 1.0, 0.0)) %>%
    ml_multiclass_classification_evaluator("prediction", "Desert", "accuracy")
}

# Calculate AUC and accuracy
perf_metrics <- data.frame(
  model = names(ml_score_ftr),
  AUC = 100 * sapply(ml_score_ftr, ml_binary_classification_evaluator, "Desert", "prediction"),
  Accuracy = 100 * sapply(ml_score_ftr, calc_accuracy),
  row.names = NULL, stringsAsFactors = FALSE)

# Plot results
acc_ten_vars <- gather(perf_metrics, metric, value, AUC, Accuracy) %>%
  ggplot(aes(reorder(model, value), value, fill = metric)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() +
  labs(y="Accuracy Rate", 
       x="Model", 
       title="Performance Metrics - Top 10 Variables", 
       caption = "Source: https://cfss.uchicago.edu/notes/sparklyr/")+
  theme(legend.title = element_blank())

#https://cfss.uchicago.edu/notes/sparklyr/



#show plot results
grid.arrange(lift_all_vars, lift_ten_vars, ncol = 1)
grid.arrange(acc_all_vars, acc_ten_vars, ncol = 1)



#________________#_________________#_________________#
#________________#_________________#_________________#
#________________#_________________#_________________#


#IMPORT 2015 TESTING DATA

#all process for 2015 data match processes for 2011 data ^above^

access2015 <- spark_read_csv(sc, name = 'access2015', path = 'C:/Users/blume/Desktop/practicum/data/2015/access2015.csv', header = TRUE, delimeter = ',', stringsAsFactors = FALSE)
assistance2015 <- spark_read_csv(sc, name = 'assistance2015', path = 'C:/Users/blume/Desktop/practicum/data/2015/assistance2015.csv', header = TRUE, delimeter = ',', stringsAsFactors = FALSE)
health2015 <- spark_read_csv(sc, name = 'health2015', path = 'C:/Users/blume/Desktop/practicum/data/2015/health2015.csv', header = TRUE, delimeter = ',', stringsAsFactors = FALSE)
insecurity2015 <- spark_read_csv(sc, name = 'insecurity2015', path = 'C:/Users/blume/Desktop/practicum/data/2015/insecurity2015.csv', header = TRUE, delimeter = ',', stringsAsFactors = FALSE)
local2015 <- spark_read_csv(sc, name = 'local2015', path = 'C:/Users/blume/Desktop/practicum/data/2015/local2015.csv', header = TRUE, delimeter = ',', stringsAsFactors = FALSE)
restaurants2015 <- spark_read_csv(sc, name = 'restaurants2015', path = 'C:/Users/blume/Desktop/practicum/data/2015/restaurants2015.csv', header = TRUE, delimeter = ',', stringsAsFactors = FALSE)
socioeconomic2015 <- spark_read_csv(sc, name = 'socioeconomic2015', path = 'C:/Users/blume/Desktop/practicum/data/2015/socioeconomic2015.csv', header = TRUE, delimeter = ',', stringsAsFactors = FALSE)
store2015 <- spark_read_csv(sc, name = 'store2015', path = 'C:/Users/blume/Desktop/practicum/data/2015/store2015.csv', header = TRUE, delimeter = ',', stringsAsFactors = FALSE)


output2015 <- Reduce(function(...) merge(..., all=TRUE), list(access2015, assistance2015, health2015, insecurity2015, local2015, restaurants2015, socioeconomic2015, store2015))



#remove any rows where state > 2 charachters

output2015 <- subset(output2015, nchar(as.character(State)) <= 2)



output2015$FIPS <- as.character(output2015$FIPS)
output2015$METRO <- as.factor(output2015$METRO)
output2015$SNAP_OAPP <- as.factor(output2015$SNAP_OAPP)
output2015$SNAP_FACEWAIVER <- as.factor(output2015$SNAP_FACEWAIVER)
output2015$SNAP_VEHEXCL <- as.factor(output2015$SNAP_VEHEXCL)
output2015$SNAP_BBCE <- as.factor(output2015$SNAP_BBCE)
output2015$SNAP_REPORTSIMPLE <- as.factor(output2015$SNAP_REPORTSIMPLE)
output2015$FOODHUB <- as.factor(output2015$FOODHUB)
output2015$FARM_TO_SCHOOL <- as.factor(output2015$FARM_TO_SCHOOL)
output2015$PERPOV <- as.factor(output2015$PERPOV)
output2015$PERCHLDPOV <- as.factor(output2015$PERCHLDPOV)

output2015$MEDHHINC <- as.numeric(output2015$MEDHHINC)
output2015$CHILDPOVRATE <- as.numeric(output2015$CHILDPOVRATE)
output2015$POVRATE <- as.numeric(output2015$POVRATE)



output2015[is.na(output2015)] <- 0

output2015 <- na.omit(output2015)

vis_miss(output2015)
output2015 <- na.omit(output2015)



#scale continuous variables

outfd2015 <- dplyr::select_if(output2015, is.numeric) %>% 
  psycho::standardize()

nonnum2015 <- output2015 %>% select_if(negate(is.numeric))

finaloutput2015 <- cbind(nonnum2015, outfd2015)


#copy to Spark

finaloutput2015 = sdf_copy_to(sc, finaloutput2015, "finaloutput2015", overwrite = TRUE)

#final prediction
set.seed(123)

final_probit <- scaled_final %>% 
  ml_logistic_regression(fla1, data = finaloutput2015, family = "binomial",  maxit = 100)

final_probit_pred <- ml_predict(final_probit, finaloutput2015, num_trees = 5, min_info_gain = 1, type = "classification")

#separate the prediction information from the demographic info


set.seed(123)
#final prediction

final_rf <- scaled_final %>% 
  ml_logistic_regression(fla, data = finaloutput2015, family = "binomial",  maxit = 100, num_trees = 5, min_info_gain = 1, type = "classification")

final_rf_pred <- ml_predict(final_rf, finaloutput2015)

#separate the prediction information from the demographic info

final_vis <- final_rf_pred %>% 
  select(FIPS, State, County, prediction, probability_1, probability_0)


#________________#_________________#_________________#
#________________#_________________#_________________#
#________________#_________________#_________________#

#RESULTS



#VIZ AND ANALYSIS FOR COUNTIES LIKELY TO CONTAIN FOOD DESERTS#

#THOUGHTS
# cOUNT OF COUNTY TYPE (DONE)
# POVERTY LEVELS 
# TYPES OF EMPLOYEMNT
# POPULATIONS OVER TIME
# ETHNIC MAKEUP
# AGE MAKEUP

#STEP 1: MERGE THE FINAL PREDICTION OUTPUT WITH THE COUNTY CHARS
options(scipen = 999)

pred_chars <- merge(output2015, final_vis, by = c("State", "County", "FIPS")) %>% collect()


#pred_chars$prediction <- as.factor(pred_chars$prediction)

pred_chars$prediction[pred_chars$prediction == 1] <- "Food Desert"
pred_chars$prediction[pred_chars$prediction == 0] <- "Non-Food Desert"

pred_chars$METRO <- as.numeric(pred_chars$METRO)

pred_chars$METRO[pred_chars$METRO == 2] <- "Urban"
pred_chars$METRO[pred_chars$METRO == 1] <- "Rural"



#THIS GIVES DIFFS BETWEEN METRO FD/URBAN FD/METRONFD/URBAN NFD

pred_chars_melt <- pred_chars %>% group_by(prediction, METRO) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

pred_chars_melt <- melt(pred_chars_melt, id = c("prediction", "METRO"))

melt_results <- transform(pred_chars_melt, factor=paste(prediction, METRO, sep="-"))

melt_results$prediction <- NULL
melt_results$METRO <- NULL

#########################################################

#Count of each type of county

pred_chars_cnt <- pred_chars %>% group_by(prediction, METRO) %>%
  tally()

ggplot(pred_chars_cnt) +
  geom_bar(aes(x = prediction, y = n, fill = METRO),
           stat = "identity", position = "dodge") +
  theme_classic(base_size = 18) +
  labs(subtitle="Urban vs Rural", 
       y="County Count", 
       x="", 
       title="Food Deserts by County Type", 
       caption = "Source: Institute for Agriculture & Trade Policy")+
  theme(legend.title = element_blank())

sum()# ACCESS BY FOOD DESERT vs NON FOOD DESERT URBAN AND RURAL


melt_pred_access_types <- melt_results[1:20,]

ggplot(melt_pred_access_types) +
  geom_bar(aes(x = variable, y = value, fill = factor),
           stat = "identity", position = "dodge") +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, vjust = 1)) +
  labs(subtitle="", 
       y="Percent", 
       x="Low Access Type", 
       title="Low Access to Food", 
       caption = "Source: Institute for Agriculture & Trade Policy")+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position="top")



#STORE TYPES
pred_stores_melt <- pred_chars %>% 
  group_by(prediction, METRO) %>%
  summarise_at(56:59, sum)
melt_store <- transform(pred_stores_melt, factor=paste(prediction, METRO, sep="-"))
melt_store$prediction <- NULL
melt_store$METRO <- NULL

# ACCESS BY FOOD DESERT vs NON FOOD DESERT URBAN AND RURAL
melt_pred_store_types <- melt(melt_store)

ggplot(melt_pred_store_types) +
  geom_bar(aes(x = variable, y = value, fill = factor),
           stat = "identity", position = "dodge") +
  theme_classic(base_size = 14) +
  labs(y="Store Count", 
       x="Store Type", 
       title="Store Counts by County Type", 
       caption = "Source: USDA Economic Research Service")+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position="top")
#######################################




pred_farm_melt <- pred_chars %>% 
  group_by(prediction, METRO) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)
farm_store <- transform(pred_farm_melt, factor=paste(prediction, METRO, sep="-"))
farm_store$prediction <- NULL
farm_store$METRO <- NULL

# ACCESS BY FOOD DESERT vs NON FOOD DESERT URBAN AND RURAL
melt_pred_farm_types <- melt(farm_store)

melt_pred_farm_types <- melt_pred_farm_types[c(65:68,93:96, 101:104, 109:112, 117:120),]

ggplot(melt_pred_farm_types) +
  geom_bar(aes(x = variable, y = value, fill = factor),
           stat = "identity", position = "dodge") +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, vjust = 1)) +
  labs(subtitle="", 
       y="Total Count", 
       x="Farm Types", 
       title="Farm Type by Food Desert", 
       caption = "Source: USDA Economic Research Service")+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position="top")



#######################################

melt_pred_pov_types <- melt_results[165:172,]

ggplot(melt_pred_pov_types) +
  geom_bar(aes(x = variable, y = value, fill = factor),
           stat = "identity", position = "dodge") +
  theme_classic(base_size = 14) +
  labs(y="Percent", 
       x="Poverty Type", 
       title="Poverty by County Type & Food Desert", 
       caption = "Source: USDA Economic Research Service")+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position="right")




pred_cnty_chars <- merge(county_char, final_vis, by = c("State", "County", "FIPS"))


#pred_cnty_chars$prediction <- as.factor(pred_cnty_chars$prediction)

pred_cnty_chars$prediction[pred_cnty_chars$prediction == 1] <- "Food Desert"
pred_cnty_chars$prediction[pred_cnty_chars$prediction == 0] <- "Non-Food Desert"

#pred_cnty_chars$Metro2013 <- as.numeric(pred_cnty_chars$Metro2013)

pred_cnty_chars$Metro2013[pred_cnty_chars$Metro2013 == 2] <- "Urban"
pred_cnty_chars$Metro2013[pred_cnty_chars$Metro2013 == 1] <- "Rural"



#THIS GIVES DIFFS BETWEEN METRO FD/URBAN FD/METRONFD/URBAN NFD
#pred_cnty_chars <- merge(pred_cnty_chars, )
pred_chars_cnty_melt <- pred_cnty_chars %>% group_by(prediction, Metro2013) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

pred_chars_cnty_melt <- melt(pred_chars_cnty_melt, id = c("prediction", "Metro2013"))

cnty_melt_results <- transform(pred_chars_cnty_melt, factor=paste(prediction, Metro2013, sep="-"))




#################################

melt_pred_eth_types <- cnty_melt_results[17:40,]

ggplot(melt_pred_eth_types) +
  geom_bar(aes(x = variable, y = value, fill = factor),
           stat = "identity", position = "dodge") +
  theme_classic(base_size = 14) +
  # rotate x-axis and remove superfluous axis elements
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, vjust = 1)) +
  labs(y="Percent", 
       x="Ethnicity Type", 
       title="Ethnicity by County Type & Food Desert", 
       caption = "Source: US Bureau of Economic Analysis")+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position="top")




#################################

melt_pred_unemp_types <- cnty_melt_results[85:88,]

ggplot(melt_pred_unemp_types) +
  geom_bar(aes(x = variable, y = value, fill = factor),
           stat = "identity", position = "dodge") +
  theme_classic(base_size = 14) +
  labs(y="Percent",
       x="",
       title="Unemployment Rates", 
       caption = "Source: US Bureau of Economic Analysis")+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position="right")





#################################

melt_pred_emp_types <- cnty_melt_results[129:168,]

ggplot(melt_pred_emp_types) +
  geom_bar(aes(x = variable, y = value, fill = factor),
           stat = "identity", position = "dodge") +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, vjust = 1)) +
  labs(y="Percent", 
       x="Industries", 
       title="Industry Employment", 
       caption = "Source: US Bureau of Economic Analysis")+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position="top")



#################################

melt_pred_edu_types <- cnty_melt_results[93:112,]

ggplot(melt_pred_edu_types) +
  geom_bar(aes(x = variable, y = value, fill = factor),
           stat = "identity", position = "dodge") +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, vjust = 1)) +
  labs(y="Percent", 
       x="Education Type", 
       title="Education Levels", 
       caption = "Source: US Bureau of Economic Analysis")+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position="right")





pop_density_res <- pred_cnty_chars %>% 
  mutate(density = (TotalPopEst2011/LandAreaSQMiles2010)) %>% 
  select(County, LandAreaSQMiles2010, Metro2013, TotalPopEst2011, density, prediction)


pop_density_res <- transform(pop_density_res, factor=paste(prediction, Metro2013, sep="-"))

ggplot(pop_density_res, aes(x=LandAreaSQMiles2010, y=(TotalPopEst2011/10000000))) + 
  geom_point(aes(col=factor, size=density)) + 
  geom_smooth(method="loess", se=F) + 
  xlim(c(0, 10000)) + 
  ylim(c(0, .125)) + 
  labs(subtitle="Urban vs Rural by State Total", 
       y="Population in Millions", 
       x="Area Totals (Sq Miles)", 
       title="Population Density", 
       caption = "Source: US Bureau of Economic Analysis")+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.position="right")




