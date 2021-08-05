library(dplyr)
library(Matrix)
library(gamlr)
library(randomForest)
library(fastDummies)
library(rpart)
library(iml)

df_email = read.csv('email_table.csv')
df_open = read.csv('email_opened_table.csv')
df_click = read.csv('link_clicked_table.csv')

df_final = df_email %>% 
  mutate (status = ifelse(email_id %in% df_click$email_id, 'click',
                          ifelse(email_id %in% df_open$email_id, 'open','none'))) %>%
  mutate (response = ifelse (status == 'click', 1, 0)) %>%
  dplyr:: select (- email_id, - status) %>%
  mutate (across(c('email_text','email_version','hour','weekday','user_country'), factor))

sum(df_final$response ==1)/nrow(df_final)

# Need to check the correlation between features 
lm(user_past_purchases ~.- email_text - email_version - response, data = df_final)%>%summary(.)
table(df_final$hour, df_final$user_country)%>%chisq.test(.)
table(df_final$weekday, df_final$user_country)%>%chisq.test(.)
# It seems that features are not correlated.

# First try the lasso regression
x = sparse.model.matrix( response~ .  , data=df_final, contrasts.arg = 
                           lapply(df_final[,sapply(df_final, is.factor)],contrasts, contrasts = FALSE))

fit_res = cv.gamlr(x, df_final$response, family = 'binomial', standardize = TRUE)
fit_coef_min = coef (fit_res, select = 'min')%>%drop(.)%>%.[.>0]
fit_coef_1se = coef (fit_res, select = '1se')%>%drop(.)%>%.[.>0]

fit_coef_min
fit_coef_1se
# Next we do the random forest 
# Split the data into train data ( model training) and test data.
set.seed(109)
sample <- sample.int(n = nrow(df_final), size = floor(.9*nrow(df_final)), replace = F)
df_train <- df_final[sample, ]
df_test  <- df_final[-sample, ]

rf = df_train%>%
   mutate (response = factor(response)) %>%
   randomForest( response  ~ ., data = ., ntree = 50, mtry = 3)

feature_imp = rf[["importance"]]/sum(rf[["importance"]]) 
feature_imp
# Hour, week, user_past_purchase matters. 

## Also do some partial dependence plot. 
predictor = df_test%>%dplyr::select(- response) %>%
  iml::Predictor$new(rf, data = ., type = 'vote') # we equip with our trained model with our data.
pdp_weekday= iml::FeatureEffect$new(predictor, feature = "weekday",method = 'pdp')
plot(pdp_weekday) 
pdp_purchase= iml::FeatureEffect$new(predictor, feature = "user_past_purchases",method = 'pdp')
plot(pdp_purchase) 

# The second question ask us to design an algorithm to optimize the 
# the profit of email compaign. Therefore we need to assume the
# possible benefit and cost of sending emails. 
# Suppose that 
## - If we do not send email, we get 0
## - If we send an email but no response, we get -1
## - If we send and email and get response, we get 1

profit = function (threshold, model_vote, real_result, cost, revenue){
  send_or_not = 1*(model_vote>threshold)
  sum((send_or_not*real_result) == 1)*revenue - sum(send_or_not == 1 )*cost
}

profit (0.7,rf[["votes"]][,'1'], df_test$response, 1, 3 )

