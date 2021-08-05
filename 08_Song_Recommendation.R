
library(tidyr)
library(dplyr)
library(jsonlite)
library(maptpx)
library(slam)
library(topicmodels)
library(rlang)
library(sjstats)

df_raw = fromJSON('song.json')%>%as.data.frame(.)

# Answer to the first question.
df_analyze_1 = df_raw %>%
  group_by(user_state ) %>%
  summarize(user_num = n_distinct(user_id) )

# Answer to the second question.
df_analyze_2 = df_raw %>% 
  group_by(user_state ) %>%
  summarize(user_num = n_distinct(user_id),
         engagement = n()/user_num   ) 

# Answer to the third question.
df_analyze_3 = df_raw %>% 
  group_by(user_state ) %>%
  mutate(user_num = n_distinct(user_id) ) %>%
  mutate (engagement = n()/user_num) %>%
  mutate (rank = order(user_sign_up_date, decreasing=FALSE)) %>%
  ungroup(.) %>%
  filter (rank == 1) %>%
  rename ( first_user = user_id) %>%
  select (user_state, user_num, engagement, first_user) 

df_analyze_4 = df_raw %>%
  group_by (user_id, song_played) %>%
  summarize (times = n()) %>%
  ungroup(.) %>%
  pivot_wider (values_from = 'times', names_from = 'song_played') %>%
  mutate (across(-c('user_id'), ~ifelse (is.na(.), 0,.)))

# Method 1: use similarity matrix

standard = function (v) {v/sum(v)}

# pr(song B | song A) = pr(B,A)/pr(A)
record_std = df_analyze_4%>% select (-user_id) %>% 
  as.matrix.data.frame(.) 

similarity =  t(record_std) %*% record_std
# We can do the recommendation based on this similarity matrix

# Method 2: we can also try the topic model
tpc = maptpx::topics (res_mat, K = 10)

# suppose that you are listenging to 'Hey Jude'
base = tpc[['theta']]
prob = base[row.names(base) == 'Hey Jude'] 

most_possible_base = base[, which(prob== max(prob))]

recommendation = names(tail(sort(most_possible_base),5)) # find the 5 most relavent songs
# or use lift!!!

# Method 3: try LDA
set.seed(10)
tpc_lda <- LDA(res_mat, k = 5, method="Gibbs", control=list(iter = 500, verbose = 25))
# the 'beta[j,k]' what is this???
# the 'gamma[i,k]': the probability that individual i pick a word from the topic k.  sum(omega[i,])==1 holds!
# check the result
terms(tpc_lda, 5) 

