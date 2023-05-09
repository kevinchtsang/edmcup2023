# Import packages

library(tidyverse)
library(tidytable)
library(caret)

# Load the necessary data
tuts = read_csv('./data/training_unit_test_scores.csv')
euts = read_csv('./data/evaluation_unit_test_scores.csv')
ar = read_csv('./data/assignment_relationships.csv')
al = read_csv('./data/action_logs.csv')
#ass_details = read_csv('./data/assignment_details.csv')

# Associate the action logs for each in unit assignment with their unit test assignment
df = ar %>% 
  left_join(al, by = c('in_unit_assignment_log_id' = 'assignment_log_id')) %>% 
  select(unit_test_assignment_log_id, action)

# Get the total number of times each action was taken within the in unit assignments corresponding to each unit test assignment
df = df %>%
  group_by(unit_test_assignment_log_id, action) %>%
  tally() %>%
  ungroup()

df = df %>% 
  pivot_wider(names_from = action,
              values_from = n,
              values_fill = 0)

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
df = df %>%
  mutate(action_count_norm = (action_count - min(action_count,na.rm=T)) / (max(action_count,na.rm=T) - min(action_count,na.rm=T)))

#df['action_count'] = (action_count - action_count.min()) / (action_count.max() - action_count.min())

# Merge action count features with the training unit test scores
# tuts = tuts.merge(df, how='left', left_on='assignment_log_id', right_index=True)

tuts = tuts %>%
  left_join(df, by = c('assignment_log_id' = 'unit_test_assignment_log_id'))

# Merge action count features with the evaluation unit test scores
# euts = euts.merge(df, how='left', left_on='assignment_log_id', right_index=True)
euts = euts %>%
  left_join(df, by = c('assignment_log_id' = 'unit_test_assignment_log_id'))

# Collect the input and target columns for the regression
input_cols = c('action_count_norm')
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

write_csv(euts_submit,
          file = './ushered_example_submission.csv')

# Python code
# euts[['id', 'score']].to_csv('./working/example_submission.csv', index=False)