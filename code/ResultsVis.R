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

####
#of the counties that had a lower population between X and Y, what percent were food deserts vs counties 
#that gained people

# filter counties that lost people 
#count the total of this new group
#what percent were food deserts
final_vis <- final_vis %>% collect()

pop_change <- pops %>%
  mutate(net = (pop2015-pop2011)) %>% 
  select(State, County, net)
fd_county_dif <- merge(pop_change, food_desert_factor, all.x = TRUE)

fd_county_dif$Desert[is.na(fd_county_dif$Desert)] <- 0

pop_loss_fd <- fd_county_dif %>% 
  filter(net < 0 & Desert == 1) %>% 
  mutate(type = "PopLoss|FoodDesert")

pop_gain_fd <- fd_county_dif %>% 
  filter(net > 0 & Desert == 1) %>% 
  mutate(type = "PopGain|FoodDesert")

pop_loss_nfd <- fd_county_dif %>% 
  filter(net > 0 & Desert == 0) %>% 
  mutate(type = "PopLoss|NonFoodDesert")

pop_gain_nfd <- fd_county_dif %>% 
  filter(net > 0 & Desert == 0) %>% 
  mutate(type = "PopGain|NonFoodDesert")


pop_fd_comp <- rbind(pop_loss_fd, pop_gain_fd, pop_loss_nfd, pop_gain_nfd)
pop_fd_comp <- unique(pop_fd_comp)

pop_fd_comp <- pop_fd_comp %>% group_by(type) %>% 
  tally()

pop_fd_comp

pop_fd_comp <- melt(pop_fd_comp)
dev.off()


ggplot(pop_fd_comp) +
  geom_bar(aes(x = type, y = value, fill = type),
           stat = "identity", position = "dodge") +
  theme_classic(base_size = 18)+
  theme(axis.text.x=element_blank())+
  labs(subtitle="Urban vs Rural", 
       y="County Count", 
       x=" ", 
       title="Food Deserts by County Type")
theme(legend.title = element_blank())



