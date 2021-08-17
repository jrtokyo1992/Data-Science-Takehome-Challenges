library(dplyr)
library(lubridate)
library(Matrix)
library(gamlr)
library(purrr)
library(randomForest)
library(iml)
library(ROCR)
library(ggplot2)

df_raw = read.csv('Fraud_data.csv')

# we need to check whether there are outliers
# in this example, an eyeball observation is enough.

# purchase time may be relevant
df_reg = df_raw %>%
    mutate (day = weekdays(as_date (purchase_time))) %>%
    mutate (user_duration = as.numeric(difftime(purchase_time,signup_time, units = 'days') )) %>%
    select (- device_id,-ip_address,- signup_time,-purchase_time, -user_id)%>%
    mutate (across(c('class','source','browser','sex','day'), ~factor(.)) ) 

# check the imbalance
sum(df_reg$class ==1)/nrow(df_reg)

# Some EDA 
summary(df_reg)

# Although this is a prediction task, since we are asked to explain the story behind 
# the prediction, it is better to check the feature correlation 

ggplot(data = df_reg, aes (x = log(purchase_value))) + geom_density()
ggplot(data = df_reg, aes (x = user_duration)) + geom_density()

# We are worried about that purchase value depends on user characterisics
lm (log(purchase_value) ~.-class, data = df_reg)%>%summary(.)
lm (purchase_value ~.-class, data = df_reg)%>%summary(.)
# It seems that there is no correlation between purchase_value and other characteristics

# Split the data into train data ( model training) and test data.
set.seed(109)
sample <- sample.int(n = nrow(df_reg), size = floor(.9*nrow(df_reg)), replace = F)
df_train <- df_reg[sample, ]
df_test  <- df_reg[-sample, ]

df_train_under <- ovun.sample(class~., data=df_train, method = "under")$data


#-------------------------------------------------------------------------
# Method 1: use lasso regression.
df_x = df_train_under%>%select(- class) 
x = sparse.model.matrix( ~ ., data=df_x, contrasts.arg = 
                             lapply(df_x[,sapply(df_x, is.factor)],contrasts, contrasts = FALSE))

fit_res = cv.gamlr(x, df_train_under$class, family = 'binomial', standardize = TRUE)
fit_coef = coef (fit_res, select = 'min')%>%drop(.)%>%.[.>0]
# also produce the prediction
df_x_test = df_test %>%select(- class)
x_test = sparse.model.matrix( ~ ., data=df_x_test, contrasts.arg = 
                           lapply(df_x_test[,sapply(df_x_test, is.factor)],contrasts, contrasts = FALSE))

performance_lasso  = predict (fit_res$gamlr, newdata = x_test,
                type = 'response', select = fit_res$seg.min) %>%drop(.)%>%# choose the model seg.min to predict
  ROCR::prediction(., df_test$class)%>%
  ROCR::performance (., measure = 'tpr', x.measure = "fpr") 

fpr_lasso = performance_lasso@x.values[[1]]
tpr_lasso = performance_lasso@y.values[[1]]

# let's plot the ROC curve
data.frame(fpr = fpr_lasso, tpr = tpr_lasso) %>%
  ggplot(data = . , aes(x = fpr, y = tpr)) + geom_line()


# Method 2: random forest.

rf = randomForest(class ~ ., data = df_train_under, ntree = 50, mtry = 3)
# draw AUC (roc)
performance_res = predict(rf, newdata = df_test, type = 'vote')[,2]%>%
  ROCR::prediction(., df_test$class)  %>%
  ROCR::performance (., measure = 'tpr', x.measure = "fpr")

# Notice that the ROCR::prediction(prediction_vote_on_1, true_label) !!!!! important

fpr_rf = performance_res@x.values[[1]]
tpr_rf = performance_res@y.values[[1]]
# let's plot the ROC curve
data.frame(fpr = fpr_rf, tpr = tpr_rf) %>%
  ggplot(data = . , aes(x = fpr, y = tpr)) + geom_line()

# Finally, do theinterpretation of the random forest 
# since the features are mostly independent, we only need to plot the pdp

## first, get the feature importance
## method 1: directly report the feature importance from the random forest
fimp = rf[["importance"]]
fimp = fimp/sum(fimp)
## method 2: use permutation method.
predictor = df_test %>% select(-class) %>%
  iml::Predictor$new(rf, ., y = df_test$class) # This is interesting. 
fi = iml:: FeatureImp$new(predictor, loss = "ce") # This is a classification
fi_table = fi$results

## seems that purchase_value, user_duration, age are important.
predictor = df_test%>%select(- class) %>%
  iml::Predictor$new(rf, data = ., type = 'vote') # we equipp with our trained model with our data.
pdp_age= iml::FeatureEffect$new(predictor, feature = "age",method = 'pdp')
plot(pdp_age) # typically this may take a long time.

pdp_purchase_value= iml::FeatureEffect$new(predictor, feature = "purchase_value",method = 'pdp')
plot(pdp_purchase_value)

pdp_duration= iml::FeatureEffect$new(predictor, feature = "user_duration",method = 'pdp')
plot(pdp_duration)



