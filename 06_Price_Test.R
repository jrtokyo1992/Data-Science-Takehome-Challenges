library(dplyr)
library(rpart)
library(lubridate)
library(gamlr)
library(Matrix)
library(AER)

# Input the data
df_user = read.csv('user_table_pricing_test.csv')
df_test = read.csv('test_results.csv')

# we find out that some instances where test == 1 and price = 39. 

df_final = df_test %>%
  left_join (df_user, by = 'user_id') %>%
  mutate (day = weekdays(date(timestamp))) %>%
  mutate (date = as.numeric(date(timestamp) - min(date(timestamp))), 
          revenue = price * converted) %>%
  dplyr::select (-timestamp, -country, - user_id, -city) 


# First, check randomness
randomness_check = df_final %>% 
  na.omit(.) %>%
  dplyr::select (- converted, - price) %>%
  rpart (test ~., data = .) 
# The tree does not split: the test is randomized!

# Calculate the ATE of the price on conversion
ate_conversion  = df_final %>%
  na.omit(.) %>%
  summarize (avg_effect = mean (converted[test==1])- mean (converted[test==0]),
             se = sqrt(var(converted[test==1])/sum(test==1)+var(converted[test==0])/sum(test==0)),
             avg_effect_low = avg_effect - 1.96*se,
             avg_effect_high = avg_effect + 1.96*se)
# It seems that the effect on conversion is very small.

# Calculate the ATE of the price test on revenue.
ate_revenue = df_final %>%
  na.omit(.) %>%
  summarize(avg_effect = mean (revenue[test==1])- mean (revenue[test==0]),
            se = sqrt(var(revenue[test==1])/sum(test==1)+var(revenue[test==0])/sum(test==0)),
            avg_effect_low = avg_effect - 1.96*se,
            avg_effect_high = avg_effect + 1.96*se)
 
  
# ---------- 
ate_conversion_by_device = df_final%>%
  na.omit(.)%>%
  group_by(device) %>%
  summarize(avg_effect = mean (converted[test==1])- mean (converted[test==0]),
            se = sqrt(var(converted[test==1])/sum(test==1)+var(converted[test==0])/sum(test==0)),
            avg_effect_low = avg_effect - 1.96*se,
            avg_effect_high = avg_effect + 1.96*se)


ate_revenue_by_device = df_final%>%
  na.omit(.)%>%
  group_by(device) %>%
  summarize(avg_effect = mean (revenue[test==1])- mean (revenue[test==0]),
            se = sqrt(var(revenue[test==1])/sum(test==1)+var(revenue[test==0])/sum(test==0)),
            avg_effect_low = avg_effect - 1.96*se,
            avg_effect_high = avg_effect + 1.96*se)



#------------also check the effect by system.
ate_conversion_by_system = df_final%>%
  na.omit(.)%>%
  group_by(operative_system) %>%
  summarize(avg_effect = mean (revenue[test==1])- mean (revenue[test==0]),
            se = sqrt(var(revenue[test==1])/sum(test==1)+var(revenue[test==0])/sum(test==0)),
            avg_effect_low = avg_effect - 1.96*se,
            avg_effect_high = avg_effect + 1.96*se)

ate_revenue_by_system = df_final%>%
  na.omit(.)%>%
  group_by(operative_system) %>%
  summarize(avg_effect = mean (revenue[test==1])- mean (revenue[test==0]),
            se = sqrt(var(revenue[test==1])/sum(test==1)+var(revenue[test==0])/sum(test==0)),
            avg_effect_low = avg_effect - 1.96*se,
            avg_effect_high = avg_effect + 1.96*se)


#------------- by the source

ate_conversion_by_source = df_final%>%
  na.omit(.)%>%
  group_by(source) %>%
  summarize(avg_effect = mean (revenue[test==1])- mean (revenue[test==0]),
            se = sqrt(var(revenue[test==1])/sum(test==1)+var(revenue[test==0])/sum(test==0)),
            avg_effect_low = avg_effect - 1.96*se,
            avg_effect_high = avg_effect + 1.96*se)

ate_revenue_by_source = df_final%>%
  na.omit(.)%>%
  group_by(source) %>%
  summarize(avg_effect = mean (revenue[test==1])- mean (revenue[test==0]),
            se = sqrt(var(revenue[test==1])/sum(test==1)+var(revenue[test==0])/sum(test==0)),
            avg_effect_low = avg_effect - 1.96*se,
            avg_effect_high = avg_effect + 1.96*se)


# finally, we study the heterogeneous effect using a regression
## Method 1 : Lasso Regression
df_reg = df_final %>%
  mutate (across(c('source','device','operative_system'), factor)) %>%
  select (- price, - converted, -long, -lat) 

x = sparse.model.matrix( revenue~ test + test:., data=df_reg, contrasts.arg = 
                           lapply(df_reg[,sapply(df_reg, is.factor)],contrasts, contrasts = FALSE))

reg_res = cv.gamlr(x, df_reg$revenue, family = 'gaussian', standardize = TRUE)
coef(reg_res)

## Method 2: Tobit Model
### we may also be cautious about the potential data censored problem here 
sum(df_reg$revenue == 0)/nrow(df_reg)
### two many zero values!
### lets try Tobit model
tobit_res = AER::tobit(revenue ~ .,left = 0,right = Inf,
                       dist = 'gaussian',
                       data = df_reg)

## As usual, the mechanism that determines 'whether or not to buy' may be different from 
## 'how much to buy'.
## Therefore we can also try cregg's model.
