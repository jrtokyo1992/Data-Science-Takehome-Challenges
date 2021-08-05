library(lmtest)
library(sandwich)
library(rpart)
library(dplyr)
library(MatchIt)
library(tidyr)
library(lubridate)
library(ggplot2)
library(gamlr)
library(plm)

df_raw = read.csv('referral.csv')

# Data pre-check 
# It is important to check the data structure, especially when dealing with causal inference.
df_check = df_raw %>%
  group_by (user_id) %>%
  summarize(referral_level = length(unique(is_referral))) %>%
  ungroup(.)

# A user may be referral = 1 or 0 on different record. 
# However, we cannot use the DID method: 
# Even if a user is referral = 1, he could be referral = 0 in the next purchase.
# Panel regression is also not applicable here: (user_id, date) is not unique

df_check = df_raw %>%
  mutate (date = as_date(date)) %>%
  mutate (day = weekdays(date)) %>%
  mutate (date = as.numeric(date - min(date)) )

# the plot below shows that there is a large un-overlapping area of date
# For the two groups
ggplot(df_check, aes(x = date, group = is_referral)) + geom_density()

# Also check the trend of money spent with date for treated and untreated group.

df_check %>%
  filter (is_referral == 0) %>%
  group_by (date) %>%
  summarize (money = mean(money_spent)) %>% ungroup(.) %>%
  ggplot(., aes (x = date, y = money)) + geom_line()
# A clear jump of money !

df_check %>%
  filter (is_referral == 1) %>%
  group_by (date) %>%
  summarize (money = mean(money_spent)) %>% ungroup(.) %>%
  ggplot(., aes (x = date, y = money)) + geom_line()

# Given the above plot, it is dangerous to directly compare the 
# referral 1 and 0 group. 
# Date is a big issue 
# we have two approaches 
# Approach 1: simply drop the date < start_date.
# Approach 2: define date_dummy = 1(date < start_date )
# We only do the approach 2. Compare to approach 1, approach 2 uses more data information, 
# but have the risk of high estimation uncertainty (the date and treatment dummy are correlated)
# For each approach, we do both individual and record level.
start_date = min(df_experiment$date)
########################################################
# record level
df_record = df_check %>%
  #filter (date >= start_date) %>%
  mutate (date = ifelse (date>=start_date, 1, 0)) %>%
  select ( - device_id) 

ggplot(data = df_record, aes(x = log(money_spent)))+ geom_density()

## Check the randomization of the referral.
### Tree
random_check_tree_record = df_record %>%
  select (-user_id, -money_spent) %>%
  mutate (is_referral = factor(is_referral)) %>%
  rpart(is_referral ~. , data = .)
### Regression
random_check_reg_record = df_record %>%
  select ( -money_spent) %>%
  lm(is_referral~ . - user_id, data = .) %>%
   coeftest(., vcov = vcovCL, cluster = ~user_id)
### Lasso
df_temp = df_record %>%
  select ( -money_spent, - user_id) %>%
  mutate (across(c('country','day','is_referral'), factor)) 
x =   sparse.model.matrix(is_referral ~ . , data=df_temp, contrasts.arg = 
     lapply(df_temp[,sapply(df_temp, is.factor)],contrasts, contrasts = FALSE))
random_check_lasso = cv.gamlr( x = x, y = df_temp$is_referral, family = 'binomial') 
coef(random_check_lasso)

## Different randomness check methods seems to give different conclusions
## For safety, we put all the control variable into the regression

ols_res_record = lm( log(money_spent)~ country + is_referral + date, data = df_record ) %>%
  coeftest(., vcov = vcovCL, cluster = ~user_id)
# Seems that the is_referral is not significant

# we also try lasso 
df_temp = df_record %>%
  select (  - user_id) %>%
  mutate (across(c('country','day'), factor)) 
x =   sparse.model.matrix(money_spent ~ is_referral*., data=df_temp, contrasts.arg = 
                            lapply(df_temp[,sapply(df_temp, is.factor)],contrasts, contrasts = FALSE))
lasso_res_record = cv.gamlr( x = x, y = log(df_temp$money_spent), faily = 'gaussion') 
coef(lasso_res_record)

# Simply calculating the group difference is dangerous. 
# we need take the within group correlation into consideration.
# Therefore, we do not give a simple comparison
#----------------------------------------------------------------------

# Next, we do the analysis based on 'individual'. 
df_country_info = df_check %>%
  mutate (date = ifelse (date>=start_date, 1, 0)) %>%
  #filter (date >=start_date) %>%
  group_by (user_id, country) %>%
  summarize (money = sum (money_spent), record = n(), 
             referral = mean(is_referral)) %>%
  ungroup(.) %>%
  group_by (user_id ) %>%
  mutate (country_ratio = record /sum(record), money = mean(money)) %>%
  ungroup(.) %>%
  select (- record) %>%
  pivot_wider ( values_from = 'country_ratio', names_from = 'country' ) %>%
  mutate (across(everything(), ~ifelse(is.na(.), 0 ,.)))

df_day_info = df_check%>%
  mutate (date = ifelse (date>=start_date, 1, 0)) %>%
 # filter (date >=start_date) %>%
  group_by (user_id, day) %>%
  summarize (record = n(), earliest = min(date)) %>%
  ungroup(.) %>%
  group_by (user_id) %>%
  mutate (day_ratio = record/sum(record), earliest = min(earliest)) %>%
  ungroup(.)%>%
  select(-record) %>%
  pivot_wider ( values_from = 'day_ratio', names_from = 'day')  %>%
  mutate (across(-earliest, ~ifelse(is.na(.), 0 ,.)))

df_ind = inner_join(df_country_info, df_day_info, by = 'user_id') %>%
  mutate (earliest = as.numeric(earliest- min(earliest))) 

# The data shows that the earliest purchase date of the referral = 0 group
# is 2021-10-3, earlier than that of the referral == 1 group, 2021-10-27
# Therefore, it would be misleading to simply compared the outcome between two groups
# when there is time trend of purchase. 

# now lets consider the endogeneity of is_referral
random_check_reg_ind = lm(referral~ .-user_id-money, data = df_ind ) %>%
  coeftest(., vcov = vcovHC(., type="HC1")) # seems that country plays a good part (?)

random_check_corr_ind = df_ind %>%
  select(-user_id, - money) %>%
  cor(.)  

random_check_tree_ind = df_ind %>%
  select (-user_id, -money) %>%
  rpart(referral ~. , data = .) # totally random

df_temp_ind = df_ind %>%
  select ( -money, - user_id)
x =   sparse.model.matrix(referral ~ . , data=df_temp_ind, contrasts.arg = 
                            lapply(df_temp_ind[,sapply(df_temp_ind, is.factor)],contrasts, contrasts = FALSE))
random_check_lasso_ind = cv.gamlr( x = x, y = df_temp_ind$referral, family = 'gaussian') 

# There are two methods dealing with this .

## Method 1: regression. use country as a control 
## For simplicity, here I use the whole covariates as control 
ols_res_ind = lm( log(money)~. - user_id , data = df_ind ) %>%
  coeftest(., vcov = vcovHC(., type="HC1"))
## It seems that the referal is not very effective. 
## Also want to see the heterogeneous effect 
ols_res_ind_hte = df_ind %>%
  select (-user_id) %>%
  lm( log(money)~ referral*., data = . ) %>%
  coeftest(., vcov = vcovHC(., type="HC1"))
## we can also apply lasso to complete the analysis. 
df_temp_ind = df_ind %>%
  select ( - user_id)
x =   sparse.model.matrix(money~ referral*. , data=df_temp_ind, contrasts.arg = 
                            lapply(df_temp_ind[,sapply(df_temp_ind, is.factor)],contrasts, contrasts = FALSE))
lasso_res_ind = cv.gamlr( x = x, y = log(df_temp_ind$money), family = 'gaussian') 

