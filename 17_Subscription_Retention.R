library(dplyr)
library(tidyr)
library(Matrix)
library(glmnet)
library(parallel)
library(distrom)
library(gamlr)
library(ggplot2)

## This is another retention rate analysis. 
## in the exercise of employee retention, we analyze
## the data of all the join and quit history. i.e., we are analyzing various cohorts.
## In this example, we are analyzing one cohort

## For example, if we want to check the effect of 
## department on employee retention, we can :
## a given cohort, we plot the monthly retention rate within each department.
## for each department and each cohort, we have a curve
## for each department, we average curves across cohorts, get an average curve for this deparment
## by comparing such a curve, we know the effect of department on retention rate. 

df_raw = read.csv('subscription.csv') %>%
  mutate (billing_cycles = as.numeric(billing_cycles))

df_1 = df_raw %>%
  group_by (subscription_monthly_cost, billing_cycles) %>%
  summarize (num = n()) %>%
  ungroup(.) %>%
  filter (billing_cycles !=8) %>% # 8 does not provides any information.
  group_by (subscription_monthly_cost) %>%
  mutate (loss = cumsum(num)) %>%
  ungroup(.) %>%
  select (- num) %>%
  pivot_wider (., values_from = 'loss', names_from = 'billing_cycles')

df_2 = df_raw %>%  
  group_by (subscription_monthly_cost) %>%
  summarize (num = n()) %>%
  ungroup(.) %>%
  left_join(df_1, by = 'subscription_monthly_cost') %>%
  mutate (across(-c('subscription_monthly_cost','num'), 
                 ~(num-.)/num))

## df_2 records the time series of retention rate. Use moving average to predict...

cat_var = c('subscription_monthly_cost','country','source')

df_final = df_raw %>%
  mutate (across(all_of(cat_var), factor)) %>%
  select (- user_id, -subscription_signup_date, -is_active)

## The definition fo retention itself depends on the time horizon. 
## also, user characteristics affects its propensity to choose a high price.

chi_test = function(var1,var2,df){
  a = table(df[[var1]],df[[var2]])
  a[1,] = a[1,]/sum(a[1,])
  a[2,] = a[2,]/sum(a[2,])
  chisq.test(a)
}

cat_corr_res = expand.grid(cat_var, cat_var) %>%
  filter (Var1 != Var2) %>%
  rowwise (.) %>%
  mutate (pvalue = chi_test(Var1,Var2,df_final)[['p.value']])
# fortunately, the three features are not correlated

# Next we construct a model. 
# We first need to define 'active'
ggplot(df_final, aes(x = billing_cycles)) + geom_density()

map(1:8, ~sum(df_final$billing_cycles <=.)/nrow(df_final)) %>%unlist(.)
# It seems that setting threshold to 1-3 would be fine.

# Let's try a random forest 
# 

# create a function to plot the effect of 
my_pdp = function(predictor,var){
  iml::FeatureEffect$new(predictor, feature = var,method = 'pdp')#%>%
   # plot(.)
}


model_res = function (threshold, df){

df = df%>%
  mutate (retent = ifelse(billing_cycles > threshold, 1,0)) %>%
  mutate (retent = as.factor(retent)) %>%
  select (- billing_cycles)

set.seed(109)
train_ind <- sample(nrow(df), size =  nrow(df)*0.8 )
df_train <- df[train_ind, ]
df_test <- df[-train_ind, ]

rf = df %>%
  randomForest( retent ~ ., data = ., ntree = 40, mtry = 2)

## need the pdp plot 
predictor = df_test%>%dplyr::select(- retent) %>%
  iml::Predictor$new(rf, data = ., type = 'vote') # Since this is a regression, we use 'response'

map(cat_var, ~my_pdp(predictor,.)) # plot the pdp for all features.

}

a = model_res (2, df_final)


## one disadvantage of this approach may be that the we need to run a model for 
## different time interval. 
## A more intuitive method would be following question 1. 
## Sometimes, visualization is enough. 

df_1_by_country = df_raw %>%
  group_by (country, billing_cycles) %>%
  summarize (num = n()) %>%
  ungroup(.) %>%
  filter (billing_cycles !=8) %>% # the information on 8 does not provides any information.
  group_by (country) %>%
  mutate (loss = cumsum(num)) %>%
  ungroup(.) %>%
  select (- num) %>%
  pivot_wider (., values_from = 'loss', names_from = 'billing_cycles')

df_2_by_country = df_raw %>%  
  group_by (country) %>%
  summarize (num = n()) %>%
  ungroup(.) %>%
  left_join(df_1_by_country, by = 'country') %>%
  mutate (across(-c('country','num'), 
                 ~(num-.)/num)) %>%
  select (- num) %>%
  pivot_longer (-c('country'),values_to = 'retention_rate', names_to = 'month') %>%
  mutate (month = as.numeric(month))

df_2_by_country %>%
  ggplot(data = ., mapping = aes(x = month, y = retention_rate,color = country)) + 
  geom_line ()



## Suppose that price depends on country and source.
## We may study a multi-nomial regression to estimate 
## But it would be hard to interpret
## An easier way is to analyze within each price group.
##df_reg = df_final
##x= sparse.model.matrix(~country, data = df_reg)
##cl = makeCluster(4)
##glmnet_res = dmr(cl ,covars = x, counts = df_reg$subscription_monthly_cost,family='multinomial' )

##predict = predict(glmnet_res, newdata = x, type = 'response')
##actual = model.matrix(~subscription_monthly_cost, data = df_reg)
##colnames(actual) = c('29','49','99')
##residual = as.data.frame(actual - predict) %>%
##  setNames(c('price_29','price_49','price_99'))
##df_reg_2 = bind_cols(df_reg, residual) %>%
##  select(- subscription_monthly_cost) 

# Now we use df_reg_2 to do the final analaysis. 

##threshold = 4
##df_reg_3 = df_reg_2 %>%
##  mutate (retent = ifelse (billing_cycles >threshold, 1, 0)) %>%
##  select (-is_active,- user_id, -subscription_signup_date, - billing_cycles) %>%
##  mutate (across(c('country','source'), ~factor(.))) 



## we can do the same thing for source.

