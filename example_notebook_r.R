# Import packages

# library(tidytable)
library(tidyverse)
library(caret)
library(skimr)
library(PRROC)
library(stringr)
library(tidyr)

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
prob_details = read_csv('./data/problem_details.csv')

# create new variables for prob_details - 1 for prob_skill_code,
#1 for length of problem skill description

#1 - create grades
prob_details <- prob_details %>%
  arrange(problem_id,problem_multipart_position) %>%
  fill(problem_skill_code)
prob_details <- prob_details %>%
  tidytable::separate_wider_delim(problem_skill_code, ".", names = c("problem_grade", "problem_cat", "problem_sub_cat", "problem_num"))

prob_details$problem_grade <- as.factor(prob_details$problem_grade)
prob_details$problem_grade_num <- recode(prob_details$problem_grade,
                                         "K" = "0",
                                         "HSA" = "9",
                                         "HSF" = "9",
                                         "HSG" = "9",
                                         "HSN" = "9",
                                         "HSS" = "9")
prob_details$problem_grade_num = as.numeric(as.character(prob_details$problem_grade_num))

levels(prob_details$problem_grade)

# HS: introduction, number and quantity, algebra



#2 - longest word

prob_details$longest_word <- sapply(base::strsplit(prob_details$problem_skill_description, " |/|-"), function(x) x[which.max(nchar(x))])
prob_details$longest_word <- gsub(",", "", prob_details$longest_word)



prob_details <- prob_details %>%
  group_by(problem_id) %>%
  mutate(
    length_longest = nchar(longest_word)
  ) %>%
  ungroup()

# ungraded questions
prob_details$ungraded <- prob_details$problem_type == "Ungraded Open Response"

# View(prob_details[is.na(prob_details$problem_grade),])

##checking variables

# skim(prob_details$length_longest) # range 3-15, we good
max(prob_details$length_longest)
min(prob_details$length_longest)

prob_details_selected = prob_details %>%
  select(problem_id, problem_type, problem_multipart_position,
         problem_contains_equation,
        problem_grade_num, length_longest,ungraded)

# make problem_type as dummies
prob_details_selected$problem_type_i = 1
prob_details_selected = prob_details_selected %>% 
  pivot_wider(names_from = problem_type,
              values_from = problem_type_i,
              values_fill = 0)

# from the prob_details
# - problem_multipart_position
# - problem_type
# - problem_contains_equations
# - problem_grade
# - length_longest


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

# df_student_selected_unique_actions = df_student_selected_actions %>%
#   group_by(student_id) %>%
#   n_distinct(action)

df_student_selected_features = df_student_selected %>%
  ungroup() %>%
  mutate(
    assignment_duration = ifelse(
      is.na(assignment_end_time),
      NA,
      as.numeric(assignment_end_time) - as.numeric(assignment_start_time))
  ) %>%
  group_by(student_id) %>%
  summarise(
    total_wrong_response = sum(action == "wrong_response"), # total wrong
    total_assignments = n_distinct(assignment_log_id), # total assignments
    total_problem = n_distinct(problem_id), # total problem
    total_action = sum(!is.na(action)), # total action
    mean_assignment_duration = mean(assignment_duration, na.rm=T)
    #
  ) %>%
  ungroup()

df_student_selected_features = df_student_selected_features %>%
  mutate(
    prop_wrong_response = total_wrong_response / total_problem, # ratio
    prop_action = total_action / total_assignments # ratio
  )


# df_student_all

df_student_features = df_student %>%
  ungroup() %>%
  mutate(
    assignment_duration = ifelse(
      is.na(assignment_end_time),
      NA,
      as.numeric(assignment_end_time) - as.numeric(assignment_start_time))
  ) %>%
  group_by(student_id) %>%
  summarise(
    total_wrong_response = sum(action == "wrong_response"), # total wrong
    total_assignments = n_distinct(assignment_log_id), # total assignments
    total_problem = n_distinct(problem_id), # total problem
    total_action = sum(!is.na(action)), # total action
    mean_assignment_duration = mean(assignment_duration, na.rm=T)
    #
  ) %>%
  ungroup()

df_student_features = df_student_features %>%
  mutate(
    prop_wrong_response = total_wrong_response / total_problem, # ratio
    prop_action = total_action / total_assignments # ratio
  )


# Get the total number of times each action was taken within the in unit assignments corresponding to each unit test assignment
# dummies
df = df %>%
  group_by(unit_test_assignment_log_id, action) %>%
  tally() %>%
  ungroup()

df = df %>% 
  pivot_wider(names_from = action,
              values_from = n,
              values_fill = 0)


# skim variables
# skim(df)

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
  left_join(df_student_features, by = c('student_id')) %>%
  # left_join(prob_details[c('problem_id', 
  #                          'problem_type', 
  #                          'problem_multipart_position',
  #                          'problem_contains_equation',
  #                          'problem_grade_num',
  #                          'length_longest',
  #                          'ungraded'
  #                          )], 
  #           by = c("problem_id"))
  left_join(prob_details_selected, by = c("problem_id"))

# tuts$problem_type = tuts$problem_type %>%
#   replace_na("unknown")
  

# Merge action count features with the evaluation unit test scores
# euts = euts.merge(df, how='left', left_on='assignment_log_id', right_index=True)
euts = euts %>%
  left_join(df_norm, by = c('assignment_log_id' = 'unit_test_assignment_log_id')) %>%
  left_join(ass_details[,c('assignment_log_id','student_id')], by = c('assignment_log_id')) %>%
  left_join(df_student_features, by = c('student_id')) %>%
  left_join(prob_details_selected, by = c("problem_id"))

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

# tuts$problem_type = as.factor(tuts$problem_type)

tuts_ml_df = tuts %>%
  mutate(across(c(
    total_wrong_response,
    total_action,
    total_assignments,
    total_action,
    total_problem,
    prop_action,
    problem_multipart_position,
    problem_grade_num,
    length_longest,
    mean_assignment_duration), normalise))

tuts_train = tuts_ml_df %>%
  filter(student_id %in% train_students) %>%
  select(-student_id)

tuts_test = tuts_ml_df %>%
  filter(student_id %in% test_students) %>%
  select(-student_id)



#
input_cols3 = colnames(tuts %>% 
                         select(-c('assignment_log_id',
                                   'problem_id',
                                   'score',
                                   'student_id')))

# glm_model3 = glm(formula = paste0(target_col,'~',paste0(input_cols3, collapse = "+")), data = tuts_train)
# glm_model3 = glm(formula = paste0(target_col,'~.'), data = tuts_train %>% select(c(score,all_of(input_cols3))))
glm_model3 = glm(formula = paste0(target_col,'~.'), data = tuts_ml_everything %>% select(c(score,all_of(input_cols3))))


# train models using cross validation
train.control <- trainControl(method          = "cv", 
                              number          = 3,
                              classProbs      = TRUE,
                              summaryFunction = twoClassSummary)

# fill NA with 0.5
tuts_train_filled = tuts_train %>%
  mutate(across(!assignment_log_id & !problem_id, as.numeric))
tuts_train_filled[is.na(tuts_train_filled)] = 0.5

tuts_test_filled = tuts_test %>%
  mutate(across(!assignment_log_id & !problem_id, as.numeric))
tuts_test_filled[is.na(tuts_test_filled)] = 0.5

# make ML df for everything to train for evaluation
# tuts
tuts_ml_everything = tuts_ml_df %>%
  select(-student_id) %>%
  mutate(across(!assignment_log_id & !problem_id, as.numeric))
tuts_ml_everything[is.na(tuts_ml_everything)] = 0.5

# euts
euts_ml_df = euts %>%
  mutate(across(c(
    total_wrong_response,
    total_action,
    total_assignments,
    total_action,
    total_problem,
    prop_action,
    problem_multipart_position,
    problem_grade_num,
    length_longest,
    mean_assignment_duration), normalise))
euts_ml_everything = euts_ml_df %>%
  select(-student_id) %>%
  mutate(across(!assignment_log_id & !problem_id, as.numeric))
euts_ml_everything[is.na(euts_ml_everything)] = 0.5


model_rf <- train(
  as.factor(make.names(score)) ~ .,
  data      = tuts_ml_everything %>% select(c(score,all_of(input_cols3))), 
  method    = "rf",
  ntree     = 5,
  trControl = train.control,
  metric    = "ROC")

# rf_model3 = rf(formula = paste0(target_col,'~',paste0(input_cols3, collapse = "+")), data = tuts_train)


tuts_test_score = predict(model_rf,
                     newdata = tuts_test_filled,
                     "prob")[,2]
# tuts_test_score = predict(glm_model3,
#                           newdata = tuts_test_filled)
tuts_test_score[is.na(tuts_test_score)] = 0.5
test_rf_roc = roc.curve(tuts_test_score[tuts_test$score == 1], 
                        tuts_test_score[tuts_test$score == 0], 
                        curve = TRUE)
plot(test_rf_roc)
test_rf_roc$auc


# eval scores
euts$score = predict(model_rf,
                          newdata = euts_ml_everything,
                          "prob")[,2]

euts_submit = euts %>% select(id, score)

write_csv(euts_submit,
          file = './ushered_example_submission.csv')
