library(dplyr)
library(ggplot2)
library(lubridate)
library(gamlr)
library(randomForest)
library(rpart)
library(tidyr)

df_count = read.csv('video_count.csv') %>%
  mutate (date = as_date(date))
df_feature = read.csv('video_features.csv')


df_1 = df_count%>%
  group_by(video_id) %>%
  summarize ( avg_count = mean(count), 
              #count_growth = (count[date== max(date)] - count[date== min(date)]),
              count_growth = mean((count- lag(count))/lag(count),na.rm = TRUE) ,
              last_date = max(date),
              first_date = min(date),
              record_day = n())

# it seems that the windows are the same.
# But different videos are upload at different times.
# Time is a big effect!
# Intuitively, this is a short period, 
# in reality, we may also want to narrow down the time interval into this...
ggplot(data = df_1, aes(x = log(avg_count))) + geom_density()
ggplot(data = df_1, aes(x = log(count_growth))) + geom_density()
# It seems that there are two hills in the log(count_growth) distribution.
# The 'valley' is around log(count_growth) = -2.5

df_2 = df_1 %>% 
  left_join (df_feature, by = 'video_id') %>%
  select (- video_id, -last_date, -first_date, -record_day, - avg_count) %>%
  mutate (video_upload_date = as_date (video_upload_date) ) %>%
  mutate (dayofweek = weekdays(video_upload_date)) %>%
  mutate (days_ago = as.numeric(as_date('2015-01-01') - as_date(video_upload_date))) %>%
  mutate (hot = ifelse ( count_growth > exp(-1), 1, 0)) %>% # according to our observation.
  select (- video_upload_date, -count_growth) %>%
  mutate (across(c('video_quality', 'video_language','dayofweek'), ~factor(.)))

# Now we want to build a model to study the features of a hot video.

# First is to check the label imbalance
sum(df_2$hot ==1)/nrow(df_2) # very balanced!

# Also check the feature correlations.
chi_test = function(var1,var2,df){
  a = table(df[[var1]],df[[var2]])
  a[1,] = a[1,]/sum(a[1,])
  a[2,] = a[2,]/sum(a[2,])
  chisq.test(a)
}

anova_test = function(num_var,cat_var,df){
  aov(df[[num_var]]~df[[cat_var]])%>%summary(.)
}

cat_var = c('video_language','video_quality','dayofweek')
num_var = c('days_ago','video_length')
# First check the correlation between categorical variable

cat_corr_res = expand.grid(cat_var, cat_var) %>%
  filter (Var1 != Var2) %>%
  rowwise (.) %>%
  mutate (pvalue = chi_test(Var1,Var2,df_2)[['p.value']])

# num and category
num_cat_corr_res = expand.grid(num_var,cat_var) %>%
  rowwise(.) %>%
  mutate (pvalue = anova_test(Var1,Var2, df_2)[[1]][["Pr(>F)"]][1])

# num and num
num_num_corr_res = df_2 %>%
  select (all_of (num_var)) %>%
  cor(.)


ggplot(data = df_2, aes(x = log(video_length))) + geom_density()
ggplot(data = df_2, aes(x = days_ago)) + geom_density()


a = lm(log(video_length) ~ video_language + days_ago + dayofweek + video_quality, data = df_2)[['residuals']]
b = lm(days_ago ~ video_language +  dayofweek + video_quality, data = df_2)[['residuals']]

df_final = df_2 %>%
  select(- video_length ) %>%
  bind_cols( list(video_length = a)) 


## we first try lasso 

x = sparse.model.matrix( hot ~ ., data=df_final, contrasts.arg = 
                           lapply(df_final[,sapply(df_final, is.factor)],contrasts, contrasts = FALSE))

lasso_fit = cv.gamlr(x = x, y = df_final$hot, family = 'binomial', standardization = TRUE)
coef(lasso_fit)
## seems that only date matters?


## next try random forest 
set.seed(109)
train_ind <- sample(nrow(df_final), size =  nrow(df_final)*0.8 )
df_train <- df_final[train_ind, ]
df_test <- df_final[-train_ind, ]

rf = df_train %>%
  mutate (hot = factor(hot)) %>%
  randomForest( hot ~ ., data = ., ntree = 50, mtry = 3)

## need the pdp plot 
predictor = df_test%>%dplyr::select(- hot) %>%
  iml::Predictor$new(rf, data = ., type = 'vote') # Since this is a regression, we use 'response'

# create a function to plot the effect of 
my_plot_pdp = function(var){
  iml::FeatureEffect$new(predictor, feature = var,method = 'pdp')%>%
    plot(.)
}

my_plot_pdp('video_language') # intuitive
my_plot_pdp('video_quality') # seems that video qualtiy does not matter much.
my_plot_pdp('dayofweek') # intuitive
my_plot_pdp('video_length')
my_plot_pdp('days_ago')


