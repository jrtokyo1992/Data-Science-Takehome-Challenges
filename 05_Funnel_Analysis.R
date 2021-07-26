library(dplyr)
library(MASS)
library(lubridate)
library(gamlr)
library(randomForest)
library(rpart)

# Input the data

df_final= read.csv('user_table_funnel.csv')%>%
  left_join(read.csv('search_page_table.csv'), by = 'user_id') %>%
  rename (search = page) %>%
  left_join (read.csv('payment_page_table.csv'), by = 'user_id') %>%
  rename (payment = page) %>%
  left_join (read.csv('payment_confirmation_table.csv'), by = 'user_id') %>%
  rename (finalize = page) %>%
  mutate (across(c('search','payment','finalize'), ~ifelse(is.na(.), 0, 1))) %>%
  mutate (day = weekdays(as_date(date)))

rm (df_homepage, df_finalize, df_payment, df_search, df_user)

# Question 1 -------------------------------------------------------------------
df_analyze = df_final %>% group_by (device, sex, day)%>%
  summarize(homepage = n()/nrow(df_final),homepage_to_search = sum(search)/n(), 
            search_to_payment= sum(payment)/sum(search),
            payment_to_finalize = sum(finalize)/sum(payment),
            )

#in general, the mobile tends to have a higher conversion rate. 

# Question 2 -------------------------------------------------------------------

df_reg = df_final %>% 
  mutate (status = ifelse(search ==0, 'homepage', ifelse( payment ==0, 'search' ,
                                                       ifelse (finalize ==0, 'payment','finalize')   ))) %>%
  dplyr::select (-search, -payment, -finalize, -user_id) %>%
  dplyr::mutate (date = as_date(date),
                 status = factor(status, levels = c('homepage','search','payment','finalize'),ordered = TRUE)) %>%
  dplyr::mutate (time = as.numeric(date - min(date))) 
 
## Method 1: Let's try the ordered logit regression.

ordinal_res <- polr(status ~ time + sex + device, data = df_reg, Hess=TRUE)
## Pay attention to the interpretation of the model

## Method 2: Let's do the ordinary logit regression

df_reg2 = df_reg%>% 
  dplyr::mutate (conversion = ifelse (status == 'finalize', 1, 0 )) %>%
  dplyr::mutate (across(c('device','day','sex'), factor)) %>%
  dplyr::select ( - status, - date)

### for safety, let's check the correlations between features
table(df_reg2$device, df_reg2$sex) %>% chisq.test(.)
table(df_reg2$day, df_reg2$sex) %>% chisq.test(.)
table(df_reg2$day, df_reg2$device) %>% chisq.test(.)

## It seems that there is a problem of imbalanced label.
# https://stackoverflow.com/questions/8704681/random-forest-with-classes-that-are-very-unbalanced
# We can use under-sampling to alleviate the problem.
set.seed(109)
conversion_1 = df_reg2%>% filter (conversion == 1)
conversion_0 = df_reg2%>% filter (conversion == 0) %>%
  slice_sample (n = nrow(conversion_1) * 10)
new_data = bind_rows(conversion_1, conversion_0) 
sample <- sample.int(n = nrow(new_data), size = floor(.9*nrow(new_data)), replace = F)
df_train <- new_data[sample, ]
df_test  <- new_data[-sample, ]

df_x = df_train%>%dplyr::select (- conversion)
x = sparse.model.matrix( ~ ., data=df_x, contrasts.arg = 
                           lapply(df_x[,sapply(df_x, is.factor)],contrasts, contrasts = FALSE))

fit_res = cv.gamlr(x, df_train$conversion, family = 'binomial', standardize = TRUE)
fit_coef = coef (fit_res, select = 'min')%>%drop(.)%>%.[.>0]


## Method 3: random forest.

rf = df_train %>% dplyr::mutate(conversion = factor(conversion)) %>%
  randomForest(conversion ~ ., data = ., ntree = 50, mtry = 3)
rf_vote = rf[["votes"]][,'1'] # get the vote 


# Finally, do theinterpretation of the random forest 
# since the features are mostly independent, we only need to plot the pdp

## first, get the feature importance
## method 1: directly report the feature importance from the random forest
fimp = rf[["importance"]]
fimp = fimp/sum(fimp)
## method 2: use permutation method.
predictor = df_test %>% select(-conversion) %>%
  iml::Predictor$new(rf, ., y = df_test$class) # This is interesting. 
fi = iml:: FeatureImp$new(predictor, loss = "ce") # This is a classification
fi_table = fi$results

predictor = df_test%>%dplyr::select(- conversion) %>%
  iml::Predictor$new(rf, data = ., type = 'vote') # we equip with our trained model with our data.
pdp_sex= iml::FeatureEffect$new(predictor, feature = "sex",method = 'pdp')
plot(pdp_sex) # typically this may take a long time.

pdp_device = iml::FeatureEffect$new(predictor, feature = "device",method = 'pdp')
plot(pdp_device)

pdp_day= iml::FeatureEffect$new(predictor, feature = "day",method = 'pdp')
plot(pdp_day)

pdp_time= iml::FeatureEffect$new(predictor, feature = "time",method = 'pdp')
plot(pdp_time)


