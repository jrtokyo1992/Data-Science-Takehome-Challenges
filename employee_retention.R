library(rpart)
library(dplyr)
library(ggplot2)
library(Matrix)
library(lubridate)
library(gamlr)

# A general experience, we want to see what affect y.
# now we have x and z. x is something we can control, while z is more like an outcome of x.
# In this situation we should first regress z on x to get the residual of z. 


df_raw = read.csv('employee_retention.csv')%>%
  mutate (across(c(join_date, quit_date), ~as_date(.)))


# Question 1 -------------------------------------------------------------------
df_join = df_raw%>%
  group_by (company_id, join_date) %>%
  summarize (join_num = n()) %>% ungroup(.)

df_quit =  df_raw%>%
  group_by (company_id, quit_date) %>%
  summarize (quit_num = n()) %>% ungroup(.)

a = seq(as.Date('2011/01/04'),as.Date('2015/12/13'), by = 'days')
b = length(unique(df_raw$company_id))
date  = a %>%
  rep(., b )

company_id =  rep(1:b, each=length(a))

df_all = data.frame( company_id, join_date = date, quit_date = date)

df_res = df_all %>%
  left_join (df_join, by = c('company_id', 'join_date' )) %>%
  left_join (df_quit, by = c('company_id', 'quit_date' )) %>%
  mutate (across(c(join_num, quit_num), ~ifelse(is.na(.), 0, .))) %>%
  mutate (across(c(join_num, quit_num), ~ cumsum(.) )) %>%
  mutate (headcount = join_num - quit_num, date = join_date) %>%
  select (company_id, date, headcount )

rm(df_join, df_quit)

# Question 2 -------------------------------------------------------------------

# lets make life easier by defining a dummy
# notice that in df_reg, some data are censored. 

df_reg = df_raw %>%
  mutate (duration = as.numeric(quit_date - join_date)) %>%
  filter ( join_date < as.Date("2015/12/13") - (365 + 31)) %>%
  mutate (quick_quit = ifelse ( is.na (duration) | duration >365, 0, 1)) %>%
  mutate (company_id = as.factor(company_id)) %>%
  select (- quit_date, -duration ) %>%
  mutate (join_date = as.numeric(join_date - min(join_date)))

# the salary must be a function of other. lets drag the residual 

ggplot(df_reg, aes(x=log(salary))) + geom_density()

# The data has a lot of company. In reality, we may want to build a model for each company.
# The reason for doing this is that, in reality, company_id does not provide any information.
# Since we need to check the factors that influence the churn,
# We may want to make the features independent.

feature_corr_check_1 = lm(log(salary)~ dept + seniority + join_date + company_id , data = df_reg) 
feature_corr_check_2 = lm(seniority~ dept + join_date + company_id , data = df_reg) 
feature_corr_check_3 = table(df_reg$dept, df_reg$company_id) %>%chisq.test(.)
# seems that salary is strongly correlated with the other covariates.
# Therefore, it would be better to extract the part of salaries that cannot be explained by other covariates.

tree_analysis = function (company_input, df) {

df_analysis = df%>%filter (company_id == company_input)%>%
  select (-company_id)

a = lm(log(salary)~ dept + seniority, data = df_analysis) %>%
  residuals(.) 

df_final = df_analysis %>%
  select (dept, seniority, quick_quit, join_date, salary) %>%
  mutate (salary = a) %>%
  mutate (dept = as.factor(dept)) %>%
  mutate (quick_quit = as.factor(quick_quit))

rpart(quick_quit~ .,df_final, #put salary
             control = rpart.control(minbucket = 30, maxdepth = 3 , cp = 0.000001))

}

rpart.plot(tree_analysis(7, df_reg))


# Also try Lasso Regression
reg_analysis = function (company_input, df) {


df_analysis = df%>%filter (company_id == company_input)%>%
    select (-company_id)
  
  a = lm(salary~ dept + seniority, data = df_analysis) %>%
    residuals(.) 
  
  df_final = df_analysis %>%
    select (dept, seniority, quick_quit, join_date, salary) %>%
    mutate (salary = a) %>%
    mutate (dept = as.factor(dept)) 
  
  x = sparse.model.matrix( quick_quit ~ ., data=df_final)
                         
  cv.gamlr(x,  df_final[,'quick_quit'], family = 'binomial', standardize = TRUE)
  
}

coef(reg_analysis(7, df_reg))
