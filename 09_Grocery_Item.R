library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(Matrix)
library(cluster)
library(stats)
library(factoextra)


# Data Input.
item_id = read.csv('item_to_id.csv')

df_purchase = read.csv('purchase_history.csv')

df_temp = df_purchase%>%
  group_by (user_id) %>%
  summarize (id = unlist( map(id, ~str_split(.,'\\,')) ) ) %>%
  ungroup(.) %>%
  group_by(user_id, id)%>%
  summarize (record = n()) %>%
  ungroup(.) %>%
  pivot_wider(., names_from = 'id', values_from = 'record') %>%
  mutate (across(-c('user_id'), ~replace(., is.na(.), 0))) %>%
  rowwise(.) %>%
  mutate (user_sum = sum( c_across(-user_id))) %>%
  ungroup(.) %>%
  mutate (across(-c('user_id', 'user_sum'), ~./user_sum)) %>%
  select( -user_sum)

rownames(df_temp) = df_temp$user_id

# Method 1: directly apply PCA + cluster 
df_temp_1 = df_temp %>% select (-user_id) %>%
  t(.) 

pca_res = prcomp(df_temp_1, scale = TRUE)

fviz_eig(pca_res) # Shows the percentage of variances explained by each PC.

# The following link explains in detail the how to do PCA in R
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/

score = pca_res[['x']]%>%
  as.data.frame(.) %>%
  dplyr::select (PC1:PC10)

# I randomly
fviz_nbclust(score, pam, method = "wss")
fviz_nbclust(score, pam, method = "silhouette")

# There is no obvious elbow
# it turns out that the result would not be good.
cluster_res_1 = pam(score, 6, diss = TRUE)

# Method 2: cluster based on the similarity matrix.
mat = df_temp %>% select(- user_id) %>% 
  as.matrix( .)

mat_final  = t(mat) %*% mat

fviz_nbclust(mat_final, pam, method = "wss")
fviz_nbclust(mat_final, pam, method = "silhouette")
cluster_res = pam(mat_final, 8, diss = TRUE)

# We can also try other robust method, lik DEBSCAN.
