library(dplyr)
library(rpart)
library(randomForest)
library(isotree)
library(ROCR)
library(purrr)
library(caTools)
library(caret)  # a convenient library for doing machine learning. 
library(e1071)
library(tidyr)
library(ggplot2)
library(iml)



df_borrower = read.csv('borrower_table.csv')
df_loan = read.csv('loan_table.csv')

# First compute the current bank profit 
a = sum(df_loan$loan_granted == 1 & df_loan$loan_repaid == 1)
b = sum(df_loan$loan_granted == 1 & df_loan$loan_repaid == 0)
current_profit_avg = (a-b)/sum(df_loan$loan_granted ==1)

# First, delete the non-granted data, since they are unlabelled.
df_clean = df_borrower%>%
  inner_join(df_loan, by = 'loan_id') %>%
  filter ( !is.na(loan_repaid )) %>%
  mutate ( across(c(fully_repaid_previous_loans,currently_repaying_other_loans),
                  ~ifelse(is.na(.), 'Not applicable', .))) %>%
  mutate (wealth = saving_amount + checking_amount) %>%
  mutate (loan_repaid = factor(loan_repaid)) %>%
 # mutate (card_debt = total_credit_card_limit* avg_percentage_credit_card_limit_used_last_year) %>%
  mutate (has_loan = ifelse(fully_repaid_previous_loans == 0 | currently_repaying_other_loans ==1, 1, 0)) %>%
  mutate (across(c('is_employed','has_loan'), factor)) %>%
  select ( -date, -loan_granted, - loan_id, -is_first_loan,
           -saving_amount, -checking_amount,
           -total_credit_card_limit,
           - fully_repaid_previous_loans, -currently_repaying_other_loans) %>%
  mutate (yearly_salary = log(yearly_salary + 1),
          wealth = log(wealth + 1)) %>%
  na.omit(.)

# Check the distribution of some variables.
ggplot(df_clean, aes(x = wealth)) + geom_density()
ggplot(df_clean, aes(x = yearly_salary)) + geom_density()
# yearly_salary shows a cluster at 0. and almost no instances for salary<7.
df_final = df_clean%>%
  filter (wealth >= 6) 

# May also try isolation forest to get rid of the abnormal values

# Split the data
set.seed(109)
train_ind <- sample(nrow(df_final), size =  nrow(df_final)*0.8 )
df_train <- df_final[train_ind, ]
df_test <- df_final[-train_ind, ]
train_x = df_train%>%select (-loan_repaid)
train_y = df_train[,'loan_repaid']
test_x = df_test%>%select (-loan_repaid)
test_y = df_test[,'loan_repaid']

# Even After our feature transformation, there are still many correlated features in this dataset.
# employed status + age affects the salary
# employed status + age + salary affects the number of kids.
# employed status + age + salary + number of kids affect the up-to-date debt status.(loan and credit card)
# employed status + age + salary + number of kids affect the wealth

# With so many factors correlated with each other, 
# we may want to use ale plot when doing interpretation

# Answer to question 2.
## Train the model.
rf = randomForest(loan_repaid ~ ., data = df_train, 
                 ntree = 50, mtry = 3)

## Now we calculate the profitability of our model.
## We write a function
model_profitability = function(threshold, trained_model,test_data){
pred = predict(trained_model, newdata = test_data, type = 'vote') %>%
  as.data.frame(.) %>%
  setNames(c('zero_vote','one_vote')) %>%
  select (one_vote) %>%
  bind_cols(loan_repaid = df_test$loan_repaid ) %>%
  mutate (threshold = threshold) %>%
  mutate (grant = ifelse (one_vote> threshold, 1, 0 ))

a = sum(pred$grant == 1 & pred$loan_repaid == 1)
b = sum(pred$grant == 1 & pred$loan_repaid == 0)
 a-b}

model_profitability(0.3, rf, df_test)
max_profit_avg = map(seq(0.1,0.9, by = 0.05), ~model_profitability(., rf,df_test))%>%
  unlist(.)%>%max(.)/nrow(df_test)
# This result in an avg profit of 0.53, nearly double the profit of the current model, which is 0.28
# We can also tune the hyper-parameters on randomforest to achieve higher profit.

# Finally, do the interpretation of the random forest 
# since the features are mostly independent, we only need to plot the pdp

## Have a glance at the feature importance (not very reliable)
fimp = rf[["importance"]]
fimp/sum(fimp)

## Do ALE plot
predictor = df_test%>%dplyr::select(- loan_repaid) %>%
  iml::Predictor$new(rf, data = ., type = 'vote') # we equip with our trained model with our data.

ale_wealth= iml::FeatureEffect$new(predictor, feature = "wealth",method = 'ale')
plot(ale_wealth) 

ale_card_ratio = iml::FeatureEffect$new(predictor, feature = "avg_percentage_credit_card_limit_used_last_year",method = 'ale')
plot(ale_card_ratio)

ale_age = iml::FeatureEffect$new(predictor, feature = "age",method = 'ale')
plot(ale_age)

# when checking the effect of salary, it is better to limit the value of salary to positive value
predictor_for_salary = df_test%>%
  dplyr::filter (yearly_salary >7) %>%
  dplyr::select(- loan_repaid) %>%
  iml::Predictor$new(rf, data = ., type = 'vote') # we equip with our trained model with our data.

ale_salary = iml::FeatureEffect$new(predictor_for_salary, feature = "yearly_salary",method = 'ale')
plot(ale_salary)

# It seems that wealth and salary has a non-linear effect on the possibility of loan default. 


## Optional: Try the Caret package.
# See the document of caret:
# https://topepo.github.io/caret/model-training-and-tuning.html#customizing-the-tuning-process

# caret works much similar to the gridsearchCV in sklearn.
# However, it seems that in caret, not all hyperparameters can be tuned.
# For example, in randomforest, the mtry is tunable, while ntree is not.
# This implies that we cannot put the grid of ntree into our search space.
# Sounds a bit weird. 
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

paramsGrid <-  expand.grid( mtry = c(2,4,6))


rf_fit_res <- caret::train(loan_repaid  ~ ., data = df_train, 
                           method = "rf", 
                           trControl = fitControl, 
                           verbose = FALSE, 
                           ntree = 50,
                           ## Now specify the exact models 
                           ## to evaluate:
                           tuneGrid = paramsGrid)
