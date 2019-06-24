#knitr::opts_chunk$set(fig.width=12, fig.height=8) 


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
