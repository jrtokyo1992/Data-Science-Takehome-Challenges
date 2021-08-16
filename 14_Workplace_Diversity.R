library(dplyr)
library(ggplot2)
library(Matrix)
library(gamlr)
library(randomForest)
library(rlang)
library(iml)

# Input the data
df_employee = read.csv('Employee.csv')
df_company = read.csv('Company_Hierarchy.csv')

# Answer to the first question.
boss_list  = df_company$boss_id
employee_list = df_company $employee_id
ceo = employee_list [is.na(boss_list) ]
position_ceo = data.frame (id = ceo, position = 'ceo')
executive = employee_list [boss_list == ceo]%>%na.omit(.)
position_executive = data.frame (id = executive, position = 'executive')
vp = employee_list[boss_list %in% executive]%>%na.omit(.)
position_vp = data.frame (id = vp, position = 'vp')

director = employee_list[boss_list %in% vp]%>%na.omit(.)
position_director = data.frame (id = director, position = 'director')

middle = employee_list[boss_list %in% director]%>%na.omit(.)
position_middle = data.frame (id = middle, position = 'middle')

ic = employee_list[boss_list %in% middle]%>%na.omit(.)
position_ic = data.frame (id = ic, position = 'ic')

df_position = bind_rows(position_ceo, position_middle,position_director,
                        position_vp, position_ic, position_executive)

# Answer to the second question.
# Notice that some level people may not have direct subordinates.
df_ic = data.frame (id = ic, employee_num = 0)

df_names = c('df_ic','df_middle','df_director', 'df_vp', 'df_executive')

# We write a loop to generate multiple data frames.
for ( i in 2:length(df_names)) {

  a = data.frame (boss_id = executive)%>%
    left_join(df_company, by = 'boss_id') %>%
    rename (id = employee_id) #%>%
  
  # combine with the previous data frame. This is a bit tricky.
  b = paste0('a = left_join(a',',',df_names[i-1],',by = \'id\')') 
    
  eval(parse_expr(b)) 
    
  a = a%>%
    select (-id) %>%
    rename (id = boss_id) %>%
    group_by (id ) %>%
    summarize (employee_num = n() + sum(employee_num))%>%
    mutate (employee_num = ifelse(is.na(employee_num), 0, employee_num))
  
  eval(parse_expr(paste0(df_names[i],'=a')))
}

df_ceo = data.frame (id = ceo, employee_num = 10000)

df_people_charge = bind_rows(df_ic, df_middle, df_director, df_vp, df_executive,df_ceo)

# Answer to the question 3.

df_dept = df_company %>%
  rename (id = employee_id) %>%
  select (id, dept)
  
df_final = df_employee %>%
  rename (id = employee_id) %>%
  left_join (df_position, by = 'id') %>%
  left_join (df_people_charge, by = 'id') %>%
  left_join (df_dept, by = 'id' ) %>%
  select (- id, -signing_bonus)

# Check some correlations between features.

chi_test = function(v1,v2){
  a = table(v1,v2)
  a[1,] = a[1,]/sum(a[1,])
  a[2,] = a[2,]/sum(a[2,])
  chisq.test(a)
}

chi_test( df_final$degree_level,df_final$degree_level)

chi_test(df_final$degree_level, df_final$sex) 

chi_test (df_final$degree_level, df_final$dept) 

chi_test(df_final$sex, df_final$dept)

## yrs_experience is doom to be correlated with degree_level
yrs_experience = lm(log(yrs_experience) ~ degree_level, data = df_final)[['residuals']]

## also check the distribution of salary
ggplot(df_final, aes(x = salary)) + geom_density()
ggplot(df_final, aes(x= yrs_experience)) + geom_density()
## it seems that we need to turn the salary into log

## now do some data transformation 
df_reg = df_final %>%
  dplyr::mutate (across(c('salary'), ~log(.))) %>%
  select (salary, dept, sex, degree_level) %>%
  bind_cols (yrs_experience = yrs_experience) %>% # we use the residual of yrs_experience
  mutate (across(c('dept','sex','degree_level'), ~factor(.))) %>% # turn to factor
  na.omit(.)
#----------------------------First, try lasso regression-------------------------
df_x = df_reg%>%select(-salary)
x = sparse.model.matrix( ~ ., data=df_x, contrasts.arg = 
                  lapply(df_x[,sapply(df_x, is.factor)],contrasts, contrasts = FALSE))

reg_res = cv.gamlr(x, df_reg$salary)
coef(reg_res)

#--------------------------Second, try random forest------------

set.seed(109)
train_ind <- sample(nrow(df_reg), size =  nrow(df_reg)*0.8 )
df_train <- df_reg[train_ind, ]
df_test <- df_reg[-train_ind, ]

# We only run a simple model. Of course, a cross-validation hyperparameter tuning would be better.
rf = randomForest(salary ~ ., data = df_train, ntree = 50, mtry = 3)
rf[["importance"]]

predictor = df_test%>%dplyr::select(- salary) %>%
  iml::Predictor$new(rf, data = ., type = 'response') # Since this is a regression, we use 'response'

# create a function to plot the effect of 
my_plot_pdp = function(var){
  iml::FeatureEffect$new(predictor, feature = var,method = 'pdp')%>%
  plot(.)
}

my_plot_pdp('sex')
my_plot_pdp('degree_level')
my_plot_pdp('dept')
my_plot_pdp('yrs_experience')
