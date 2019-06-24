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
