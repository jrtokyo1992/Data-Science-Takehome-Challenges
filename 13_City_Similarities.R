library(dplyr)
library(rjson)
library(jsonlite)
library(purrr)
library(stringr)
library(tidyr)
library(ggplot2)

# Some data processing.
df_raw <- jsonlite::fromJSON('city_searches.json') %>%
  mutate (time = as.POSIXct(as.numeric(unix_timestamp), origin="1970-01-01")) %>%
  unnest(col = user) %>% 
  unnest_wider(col = user)  # put the element of lists as columns. column names are the key words of the list.
 
df_first = df_raw%>%
  mutate (cities =str_split(cities,'\\, ')) %>%
  unnest_longer (col = cities) 
 
# The Answer to the first question.
df_1 = df_first %>%
  mutate (hour =as.numeric(strftime(time, format="%H")) )

df_1%>%
  filter (country == '') %>%
  ggplot(., aes(x = hour)) + geom_density() 

df_1%>%
  filter (country == 'US') %>%
  ggplot(., aes(x = hour)) + geom_density() 

# Compared to US, here we can guess that the unknown country is in Asia.

# Answer to the second question ------
# Build the similarity matrix.

df_2 = df_first %>%
 mutate (record = 1) %>%
 select (session_id, cities, record) %>%
 pivot_wider ( .,values_from = 'record', names_from = 'cities') 

df_2[is.na(df_2)] = 0

mat = df_2 %>%
  rowwise (.) %>%
  mutate (user_sum = sum(c_across(- session_id))) %>%
  ungroup(.) %>%
  mutate (across(-c('session_id', 'user_sum'), ~./user_sum) )%>%
  select(- session_id, - user_sum) %>% 
  as.matrix( .)

mat_final  = t(mat) %*% mat

find_most_similar = function(city, mat_final, k = 1){
  a = mat_final[,city]
  b = a[names(a)!=city]%>%
    sort(., decreasing = TRUE) %>%
    names(.)
  b[1:k]
}

find_most_similar('San Jose CA', mat_final, 5)

# For the third question, we can:
# For each user, since we can get the locations he explored, we calculate 
# the average pairwise distance (distance between any two of his locations)
# we use this metric to do the clustering 

