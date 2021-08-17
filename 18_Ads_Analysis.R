library(lubridate)
library(prophet)

# Data Input
df_raw = read.csv('ad_table.csv') %>%
  mutate (cost = clicked * avg_cost_per_click) 

ggplot(df_raw, aes(x = conversion_rate)) + geom_density()

cor(df_raw$cost, df_raw$conversion_rate)

# Answer to the question 1: 
## since cost - revenue is very important, 
## we try to create a metric which is a function of cost and revenue
df_1 = df_raw %>%
  group_by (ad) %>%
  summarize (avg_revenue = mean(total_revenue),
             avg_cost = mean (cost)) %>%
  ungroup(.) %>%
  mutate (performance = avg_revenue - avg_cost) %>%
  slice_max (., performance,n=5)

# Answer to question 2
## we use prophet to make a simple prediction.
df_2 = df_raw%>%
  select (date, shown, ad) %>%
  rename (ds = date, y = shown) %>%
  group_by (ad) %>%
  summarize (pred = prophet_predict(., '2015-12-25'))
  
prophet_predict = function(df, future){
  m = prophet(df)
  future = data.frame(ds = future)
   predict(m, future) }

# Answer to question 3
## To avoid the zero value, we first aggregate on week level.
df_3 = df_raw %>%
  mutate (week = week(date)) %>%
  group_by (ad, week) %>%
  summarize (avg_cost = mean(avg_cost_per_click)) %>%
  ungroup(.) %>%
  group_by(ad) %>%
  summarize (cost_change = 
               mean ( (avg_cost - lag(avg_cost))/ lag(avg_cost) , na.rm = TRUE ))

ggplot(data = df_3, aes(x = cost_change)) + geom_density()
