# Import packages

library(tidyverse)
library(tidytable)


# Load the necessary data
tuts = read_csv('./data/training_unit_test_scores.csv')
euts = read_csv('./data/evaluation_unit_test_scores.csv')
ar = read_csv('./data/assignment_relationships.csv')
al = read_csv('./data/action_logs.csv')

# Associate the action logs for each in unit assignment with their unit test assignment
df = left_join(ar, al, by = c('in_unit_assignment_log_id' = 'assignment_log_id'))
df = df %>% 
  select(unit_test_assignment_log_id, action)

# Get the total number of times each action was taken within the in unit assignments corresponding to each unit test assignment
df = get_dummies


df = pd.get_dummies(df, columns=['action'])
df = df.groupby('unit_test_assignment_log_id').sum()

# Create a feature for the total action count, then scale it between 0 and 1
action_count = df.sum(axis=1)

# Convert the individual action counts into a fraction of total actions taken
df = df.div(action_count, axis=0)

# Add the scaled total action count to the dataframe
df['action_count'] = (action_count - action_count.min()) / (action_count.max() - action_count.min())

# Merge action count features with the training unit test scores
tuts = tuts.merge(df, how='left', left_on='assignment_log_id', right_index=True)

# Merge action count features with the evaluation unit test scores
euts = euts.merge(df, how='left', left_on='assignment_log_id', right_index=True)
# Collect the input and target columns for the regression
input_cols = [c for c in tuts.columns if 'action' in c]
target_col = 'score'

# Initialize a logistic regression
lr = LogisticRegression(max_iter=1000)
# Fit the regression on all the training data
lr = lr.fit(tuts[input_cols], tuts[target_col])
# Predict the score for each evaluation problem
euts[target_col] = lr.predict_proba(euts[input_cols])[:,1]

# Export the id and score columns of the evaluation unit test scores file for uploading to Kaggle
euts[['id', 'score']].to_csv('/kaggle/working/example_submission.csv', index=False)