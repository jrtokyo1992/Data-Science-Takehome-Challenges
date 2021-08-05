library(lubridate)
library(dplyr)
library(isotree)
library(Rcpp)

# Data Input
df_1 = read.csv('cc_info.csv')
df_2 = read.csv('transactions.csv')

# The answer to the first question.
monthly_summarize = df_2 %>% 
  mutate (month =strftime(date,  format="%Y-%m") ) %>%
  group_by(credit_card,  month) %>%
  summarize (money = sum(transaction_dollar_amount)) %>%
  ungroup(.)
  
df_analyze = monthly_summarize %>% 
  left_join (df_1, by = 'credit_card') %>%
  mutate (over_limit = ifelse (money >credit_card_limit, 1, 0)) %>%
  group_by(credit_card) %>%
  summarize ( over_limit_times = sum(over_limit)) %>%
  ungroup(.) %>%
  filter (over_limit_times == 0)

# The answer to the second question
 
find_alert = function (df_record, df_user, date_input){

month_start = floor_date(ymd(date_input), "month")

df_record %>% 
  filter( date <= date_input & date>= month_start ) %>%
  group_by(credit_card) %>%
  summarize ( monthly_current = sum(transaction_dollar_amount)) %>%
  ungroup(.) %>%
  left_join(df_user, by = 'credit_card') %>%
  mutate (over_limit = ifelse (monthly_current >credit_card_limit, 1, 0)) %>%
  filter(over_limit ==1) %>%
  .[['credit_card']]
  
}

find_alert(df_2,df_1, '2015-10-23')

# The answer to the third question
# We apply isolation tree here. 

df_final = left_join(df_2, df_1, by = 'credit_card') %>%
  select (-Long,-Lat,-zipcode, -state) %>%
  mutate (date = as_date (date)) %>%
  mutate (days= weekdays(date)) %>%
  mutate (months = month(date)) %>%
  mutate (month_calendar =strftime(date,  format="%Y-%m") ) %>%
  group_by(credit_card,  month_calendar) %>%
  mutate (times = row_number())

anomaly_detection = isolation.forest(df_final,
  ntrees = 500)
