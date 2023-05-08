# Import packages

library(tidyverse)
library(caret)
library(skimr)
library(PRROC)

# normalise function

normalise = function(x){
  return(
    (x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))
  )
}


# Load the necessary data
tuts = read_csv('./data/training_unit_test_scores.csv')
euts = read_csv('./data/evaluation_unit_test_scores.csv')
ar = read_csv('./data/assignment_relationships.csv')
al = read_csv('./data/action_logs.csv')
ass_details = read_csv('./data/assignment_details.csv')

# set.seed(123)
# selected_students = sample(unique(ass_details$student_id), 100)

# Associate the action logs for each in unit assignment with their unit test assignment
df = ar %>% 
  left_join(al, by = c('in_unit_assignment_log_id' = 'assignment_log_id')) %>% 
  select(unit_test_assignment_log_id, action)

# Create student level data

df_student = al %>%
  left_join(ass_details, by = "assignment_log_id") %>% 
  relocate(student_id)

set.seed(123)
selected_students = sample(unique(df_student$student_id), 1000)


df_student_selected = df_student %>%
  filter(student_id %in% selected_students)

df_student_selected_actions = df_student_selected %>%
  group_by(student_id, assignment_log_id, action) %>%
  tally() %>%
  ungroup()

df_student_selected_unique_actions = df_student_selected_actions %>%
  group_by(student_id) %>%
  n_distinct(action)

df_student_selected_features = df_student_selected %>%
  ungroup() %>%
  # mutate(
  #   assignment_duration = ifelse(
  #     is.na(assignment_end_time),
  #     NA,
  #     assignment_end_time - assignment_start_time)
  # )
  group_by(student_id) %>%
  summarise(
    total_wrong_response = sum(action == "wrong_response"), # total wrong
    total_assignments = n_distinct(assignment_log_id), # total assignments
    total_problem = n_distinct(problem_id), # total problem
    total_action = sum(!is.na(action)) # total action
    # mean_assignment_duration = mean(assignment_duration)
    #
  ) %>%
  ungroup()

df_student_selected_features = df_student_selected_features %>%
  mutate(
    prop_wrong_response = total_wrong_response / total_problem, # ratio
    prop_action = total_action / total_assignments # ratio
  )


# Get the total number of times each action was taken within the in unit assignments corresponding to each unit test assignment
df = df %>%
  group_by(unit_test_assignment_log_id, action) %>%
  tally() %>%
  ungroup()

df = df %>% 
  pivot_wider(names_from = action,
              values_from = n,
              values_fill = 0)


# skim variables
skim(df)

# df1 = df %>% get_dummies(cols = 'action') %>%
#   group_by(unit_test_assignment_log_id) %>%
#   summarise(across(everything(),~sum(.)))


# Create a feature for the total action count, then scale it between 0 and 1
#action_count = df.sum(axis=1)

# Convert the individual action counts into a fraction of total actions taken
#df = df.div(action_count, axis=0)

df$action_count = rowSums(df[-1])

df = df %>%
  rowwise() %>%
  mutate(across(assignment_finished:live_tutor_requested, ~(.x/action_count))) %>%
  ungroup()


# Add the scaled total action count to the dataframe
df_norm = df %>%
  mutate(
    across(assignment_finished:action_count, normalise)
  )

#df['action_count'] = (action_count - action_count.min()) / (action_count.max() - action_count.min())

# Merge action count features with the training unit test scores
# tuts = tuts.merge(df, how='left', left_on='assignment_log_id', right_index=True)

tuts = tuts %>%
  left_join(df_norm, by = c('assignment_log_id' = 'unit_test_assignment_log_id')) %>%
  left_join(ass_details[,c('assignment_log_id','student_id')], by = c('assignment_log_id')) %>%
  left_join(df_student_selected_features, by = c('student_id'))

# Merge action count features with the evaluation unit test scores
# euts = euts.merge(df, how='left', left_on='assignment_log_id', right_index=True)
euts = euts %>%
  left_join(df_norm, by = c('assignment_log_id' = 'unit_test_assignment_log_id'))

# Collect the input and target columns for the regression
input_cols = c('action_count')
target_col = 'score'


# Initialize a logistic regression
glm_model = glm(formula = paste0(target_col,'~',paste0(input_cols)), data = tuts)
# lr = LogisticRegression(max_iter=1000)
# Fit the regression on all the training data
# lr = lr.fit(tuts[input_cols], tuts[target_col])

# Predict the score for each evaluation problem
euts$score = predict(glm_model, 
                          newdata = euts)

# Export the id and score columns of the evaluation unit test scores file for uploading to Kaggle
euts_submit = euts %>% select(id, score)

# Second attempt at model #### Adding 14 action types to model


  

input_cols2 = c(colnames(tuts)[4:18])

glm_model2 = glm(formula = paste0(target_col,'~',paste0(input_cols2, collapse = "+")), data = tuts)

boxplot(tuts$score, tuts$action_count)

bp_plot = ggplot(tuts, aes(x = as.character(score), y = assignment_finished)) +
  geom_boxplot()
ggsave(bp_plot, "boxplot.png")

do_boxplot_score = function(variable){
  bp_plot = ggplot(slice_sample(tuts, n = 1000), aes_string(x = "as.character(score)", y = variable)) +
    geom_boxplot()
  ggsave(paste0("./plots", variable, "_boxplot.png"), bp_plot)
}

input_cols2 %>%
  map(~ do_boxplot_score(.))


do_boxplot_score("action_count")

class(tuts$score)

# predict
euts$score = predict(glm_model2, 
                     newdata = euts)
summary(glm_model2)


euts_submit = euts %>% select(id, score)

# train-test-split
train_students = sample(selected_students, 0.7*length(selected_students))
test_students = selected_students[!selected_students %in% train_students]

tuts_train = tuts %>%
  filter(student_id %in% train_students)

tuts_test = tuts %>%
  filter(student_id %in% test_students)

#
input_cols3 = colnames(tuts %>% 
                         select(-c('assignment_log_id',
                                   'problem_id',
                                   'score',
                                   'student_id')))
input_cols3 = 'action_count'

glm_model3 = glm(formula = paste0(target_col,'~',paste0(input_cols3, collapse = "+")), data = tuts_train)


# train models using cross validation
train.control <- trainControl(method          = "cv", 
                              number          = 3,
                              classProbs      = TRUE,
                              summaryFunction = twoClassSummary)

model_rf <- train(
  as.factor(make.names(score)) ~ .,
  data      = tuts_train, 
  method    = "rf",
  trControl = train.control,
  metric    = "ROC")

# rf_model3 = rf(formula = paste0(target_col,'~',paste0(input_cols3, collapse = "+")), data = tuts_train)


tuts_test_score = predict(model_rf, 
                     newdata = tuts_test)
test_rf_roc = roc.curve(tuts_test_score[tuts_test$score == 1], 
                        tuts_test_score[tuts_test$score == 0], 
                        curve = TRUE)
plot(test_rf_roc)



write_csv(euts_submit,
          file = './ushered_example_submission.csv')

