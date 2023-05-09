#############################
## Explore problem details ##
#############################

# there are 10 different problem types
unique(prob_details$problem_type)
# [1] "Multiple Choice"              "Ungraded Open Response"       "Number"                      
# [4] "Algebraic Expression"         "Numeric Expression"           "Check All That Apply"        
# [7] "Exact Match (ignore case)"    "Exact Fraction"               "Exact Match (case sensitive)"
# [10] "Ordering"  

aa = tuts %>%
  left_join(prob_details[c('problem_id', 'problem_type', 'problem_skill_description')], by = c("problem_id"))

table(aa$problem_type, aa$score)
#                                   0      1
# Algebraic Expression           2966   2381
# Check All That Apply          10608   7043
# Exact Match (case sensitive)     98    288
# Exact Match (ignore case)     11654   7321
# Multiple Choice               27741  55276
# Number                       120334 163850
# Numeric Expression             1639   1446
# Ordering                        218    158

# some problem types seem to be easier than others

summary(lm(score ~ problem_type, aa))


###########################
## Explore euts and tuts ##
###########################

# look at using average scores to predict scores
bb = euts %>%
  left_join(ass_details, by = "assignment_log_id")

dd = tuts %>%
  left_join(ass_details, by = "assignment_log_id")

bb_students = unique(bb$student_id)
dd_students = unique(dd$student_id)
sum(!bb_students %in% dd_students) # 7428
length(bb_students) # 7761
# there are many new students in the euts dataset


bb_classes = unique(bb$class_id)
dd_classes = unique(dd$class_id)
sum(!dd_classes %in% dd_classes) # 0
length(dd_classes) # 2116
# there are no new classes in the euts dataset (i.e. all in tuts)


# student average scores
student_average = dd %>%
  select(student_id, score) %>%
  group_by(student_id)%>%
  summarise(mean_score = mean(score))

cc = dd %>% select(score, student_id) %>%
  left_join(student_average, by = "student_id")
cor(cc$score, cc$mean_score) # 0.51

cor(tuts$score, tuts$wrong_response) # -0.18

# student average on train data
dd_train = dd %>%
  filter(student_id %in% train_students)

dd_test = dd %>%
  filter(student_id %in% test_students)

student_average_train = dd_train %>%
  select(student_id, score) %>%
  group_by(student_id)%>%
  summarise(mean_score = mean(score))

cc = dd_test %>% select(score, student_id) %>%
  left_join(student_average_train, by = "student_id")
cor(cc$score, cc$mean_score) # no correlation and the students in train and test set are independent


# class average scores
class_average = dd %>%
  select(class_id, score) %>%
  group_by(class_id)%>%
  summarise(mean_score = mean(score))  

cc = dd %>% select(score, class_id) %>%
  left_join(class_average, by = "class_id")
cor(cc$score, cc$mean_score) # 0.34
# some correlation between class average score and score in tuts

# class average on train data
class_average_train = dd_train %>%
  select(class_id, score) %>%
  group_by(class_id)%>%
  summarise(mean_score = mean(score))

cc = dd_test %>% select(score, class_id) %>%
  left_join(class_average_train, by = "class_id")
cor(cc$score, cc$mean_score, use="complete.obs") # 0.077
# there is much lower correlation when class average score calculated separately in training data
# might not be too useful to get the class average score as a predictor


##############################
## Explore sequence details ##
##############################

seq_details = read_csv('./data/sequence_details.csv')

# calculate problem average score (average across multiple students)
# calculate sequence average score (averaged across multiple problems)

problem_average = dd %>%
  group_by(problem_id) %>%
  summarise(mean_score = mean(score))

sequence_average = seq_details %>%
  select(sequence_id, sequence_problem_ids) %>%
  mutate(sequence_problem_score = sequence_problem_ids)

# use string replace (1.8k rows)
for(problem_i in 1:nrow(problem_average)){
  prob = problem_average[problem_i,]$problem_id
  score = toString(problem_average[problem_i,]$mean_score)
  sequence_average$sequence_problem_score = str_replace(sequence_average$sequence_problem_score,
                                                        pattern = prob,
                                                        replacement = score)
  if (problem_i %% 500 == 0){print(paste0("replaced ", problem_i))}
}

# replace ungraded problems as NA score
ungraded_questions = prob_details %>%
  filter(problem_type == "Ungraded Open Response") %>%
  select(problem_id)
# 47k rows
for(ungraded_problem_i in ungraded_questions$problem_id){
  score = "NA"
  sequence_average$sequence_problem_score = str_replace(sequence_average$sequence_problem_score,
                                                        pattern = ungraded_problem_i,
                                                        replacement = score)
  if (problem_i %% 500 == 0){print(paste0("replaced ", problem_i))}
}

# remove brackets [x,y,z] to become x,y,z
sequence_average$sequence_problem_score = str_replace(sequence_average$sequence_problem_score,
                                                      pattern = "\\[",
                                                      replacement = "")
sequence_average$sequence_problem_score = str_replace(sequence_average$sequence_problem_score,
                                                      pattern = "]",
                                                      replacement = "")


# function to calculate mean from vector written as string
str_vector_mean = function(string){
  num_list = lapply(str_split(string,","),as.numeric)[[1]]
  if (any(!is.na(num_list))){
    return(mean(num_list, na.rm=T))
  } else{
    return(NA)
  }
  
}
sequence_average$mean_score = lapply(sequence_average$sequence_problem_score,
                                                 str_vector_mean)


# sequence_average = sequence_average %>%
#   mutate(sequence_problem_score = str_vector_mean(sequence_problem_score))


# prob: 104GN803C7
# score: 0.495
#seq: 1N3LZJ9L23

  
  