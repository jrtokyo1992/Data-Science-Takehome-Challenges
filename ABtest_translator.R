library(dplyr)
library(lubridate)
library(rpart)
library(Matrix)
library(gamlr)

df_test = read.csv('test_table_problem2.csv')
df_user = read.csv('user_table_problem2.csv')

# Data Processing
df_reg = df_test %>%
  inner_join (df_user, by = 'user_id') %>%
  mutate (date = as_date (date))%>%
  mutate (day = weekdays(date)) %>% # get the day of the week.
  mutate (date= as.numeric(date - min(date))) %>%
  mutate (ads_channel = ifelse(is.na(ads_channel), 'None', ads_channel))


# First check the randomness of the test.
randomness_check = df_reg %>% 
  select (- conversion ) %>%
  rpart (test ~., data = .)  
## The result shows that test is not random; it actually depends on the country.

# Now let's explore the effect of test on conversion.
# Method 1: Compare the ATE within each country. (i.e., control the country)
 
df_ate_by_country  = df_reg %>%
  filter (country != 'Spain') %>%
  group_by( country)%>%
  summarize (avg_effect = mean (conversion[test==1])- mean (conversion[test==0]),
             se = sqrt(var(conversion[test==1])/sum(test==1)+var(conversion[test==0])/sum(test==0)),
             avg_effect_low = avg_effect - 1.96*se,
             avg_effect_high = avg_effect + 1.96*se)
# The result shows that the effect of test is not significant.

# Method 2: Let's run a rigorous lasso regression. 

df_x = df_reg%>% select(-user_id, - conversion) %>%
  mutate (across(-c('test','age','date'), factor)) 

## considered all the possible interaction terms. 
## first train a lasso model that predict test 
x = sparse.model.matrix( test~ ., data=df_x, contrasts.arg = 
                           lapply(df_x[,sapply(df_x, is.factor)],contrasts, contrasts = FALSE))
test_pred = cv.gamlr(x, df_reg$test, family = 'binomial')
test_pred = predict(test_pred, newdata = x,type = 'response')

d_x = sparse.model.matrix( ~ test*., data=df_x, contrasts.arg = 
                           lapply(df_x[,sapply(df_x, is.factor)],contrasts, contrasts = FALSE))

lasso_logit = cv.gamlr(cbind(test_pred, d_x), df_reg$conversion, family = 'binomial')
coef(lasso_logit)  # The result shows that test is not significant.

lasso_linear = cv.gamlr(cbind(test_pred, d_X), df_reg$conversion, family = 'gaussian')
coef(lasso_linear)



