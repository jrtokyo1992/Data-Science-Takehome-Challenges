library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(lubridate)
library(date)
library(gamlr)
library(randomForest)
library(data.table)

# Data Input
df = read.table('url_list.txt')%>%
  setNames('url_info')

# Make a clean table
df_temp = df%>%
  mutate (id = seq(1,nrow(.))) %>%
  mutate(url_info = str_remove(url_info, '^.*\\?'))%>%
  mutate(url_info = str_split (url_info, '&'))  %>%
  unnest(cols = url_info) %>%  # pay attention to the use of unnest and separate!
  separate(., col = url_info, sep = "=",
           into = c('field_names','field_value')) %>%
  mutate (field_names = ifelse (field_names == 'hotel.amenities', 
                                paste0(field_names, '_',field_value),field_names))%>%
  mutate (field_value = ifelse(grepl('amenities', field_names), 1, field_value)) %>%
  pivot_wider(names_from = 'field_names', values_from = 'field_value') #%>%

var_numeric = c('hotel.min_score', 'hotel.adults', 'hotel.search_page',
                'hotel.customMaximumPriceFilter','hotel.children','hotel.max_score','hotel.customMinimumPriceFilter')

# Answer to the first question.
df_1 = df_temp %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(c('hotel.customMaximumPriceFilter','hotel.max_score'), 
         ~ifelse(is.na(.), max(., na.rm = TRUE), .))) %>%
  mutate (across(-id, ~ifelse(.=='yes',1,.))) %>%
  mutate (across(-id, ~ifelse(is.na(.), 0 , .))) 

# Answer to the second question
df_2 = df_1 %>% 
  mutate (across(starts_with('hotel.amenities'), ~as.numeric(.))) %>%
  rowwise(.) %>%
  mutate ( sum_amenity = sum(c_across(starts_with('hotel.amenities')))) 

# Answer to the third question
# We want a metric. 
# The basic idea is to construct a model to predict the possibility that people stays at page 1. 
# This model is a function of city. 
# we want to find the effect of city on page view.

df_3 = df_1 %>%
  mutate (hotel.city = str_split(hotel.city, ',')) %>%
  unnest (col = hotel.city) %>% # expand!
  group_by(id) %>%
  mutate (rownum = row_number()) %>%
  ungroup(.) %>%
  filter (rownum == 1) %>%
  mutate (hotel.city = str_replace_all(hotel.city, '\\+',' ')) %>%
  mutate (duration = as.Date(parse_date_time(hotel.checkout,'%y-%m-%d')) 
          - as.Date(parse_date_time(hotel.checkin,'%y-%m-%d'))) %>% # generate the time diff between checkout and checkin
  mutate (one_page = ifelse(hotel.search_page == 1, 1, 0))%>% # generate a 0-1 target variable.
  select (-c('id','hotel.checkin','hotel.checkout','hotel.search_page','rownum')) %>% # get rid of some unnecessary variable
  mutate (across(- hotel.city, ~as.numeric(.))) %>% # turn variables except the city level into numeric
  mutate (hotel.city = factor(hotel.city)) %>%
  mutate (across(starts_with('hotel.stars_')|starts_with('hotel.amenities') , factor)) %>%
  mutate (across(c('hotel.couponCode','hotel.city'), factor))

chi_test = function(v1,v2){
  a = table(v1,v2)
  a[1,] = a[1,]/sum(a[1,])
  a[2,] = a[2,]/sum(a[2,])
  chisq.test(a)
}

anova_test = function(v1,v2){
  aov(v1~v2)%>%summary(.)
}

# Since we want to make interpretation on the effect of city, 
# We need to check its correlations with other features.
corr_res_1 = df_3 %>%
  summarize (across(starts_with('hotel.stars_')|starts_with('hotel.amenities') , ~chi_test(., hotel.city)[["p.value"]])) 

corr_res_2 = df_3 %>%
  summarize (across(c('hotel.couponCode') , ~chi_test(., hotel.city)[["p.value"]])) 

corr_res_3 = df_3 %>%
  summarize (across(all_of(var_numeric[var_numeric != 'hotel.search_page']) , ~ anova_test(., hotel.city)[[1]][["Pr(>F)"]]))
# seems that hotel.min_score is slightly correlated with city. But that is fine, I guess.

# check imbalanced labels or not
sum(df_3$one_page == 1)/nrow(df_3)
# seems no problem.

# first try lasso regression 
x = sparse.model.matrix(one_page ~ ., data=df_3, contrasts.arg = 
                           lapply(df_3[,sapply(df_3, is.factor)],contrasts, contrasts = FALSE))

lasso_fit = cv.gamlr(x = x, y = df_3$one_page, family = 'binomial')

coef(lasso_fit)
# The result shows that only city matters ! 
# Second, We can try random forest

df_final = df_3 %>%
  ungroup(.) %>%
  mutate (one_page = factor(one_page))

set.seed(109)
train_ind <- sample(nrow(df_final), size =  nrow(df_final)*0.8 )
df_train <- df_final[train_ind, ]
df_test <- df_final[-train_ind, ]

rf = df_final%>%
  randomForest( one_page ~ ., data = ., ntree = 50, mtry = 3)

predictor = df_test%>%dplyr::select(- one_page) %>%
  iml::Predictor$new(rf, data = ., type = 'vote') # we equip with our trained model with our data.

pdp_city= iml::FeatureEffect$new(predictor, feature = "hotel.city",method = 'pdp')
plot(pdp_city) # Given other factors, users in london has the lower probability of staying in one page.

