## ----connect, message=FALSE----------------------------------------------
library(qtoolkit)
library(ggplot2)
library(dplyr)
library(tidyr)

## ----eval = FALSE--------------------------------------------------------
#  ## Connect to Qualtrics API
#  qapi_connect()

## ----view-survey-list----------------------------------------------------
# get a list of all surveys connected to your account / api token
all_surveys <- list_surveys()
# define survey id variable, then get survey object
survey_id <- "SV_1SUpa4C4UGkZnWB"
my_survey_ob <- qsurvey(survey_id,
                        clean_html = TRUE)
# view all questions
all_questions <- my_survey_ob$questionList
all_questions

## ------------------------------------------------------------------------
# get responses and question information for qid71.
q54_obj <- my_survey_ob$questions$QID54
str(q54_obj)

## ------------------------------------------------------------------------
# get responses and question information for qid71.
my_survey_ob$questions$QID54$choices

## ------------------------------------------------------------------------
# get responses and question information for qid71.
my_survey_ob$questions$QID54$responses


## ------------------------------------------------------------------------
# get responses and question information for qid71.
my_survey_ob$questions$QID54$subquestions

## ------------------------------------------------------------------------
# here, we just grab a df that has the data stacked in a way that is easily plottable
q54 <- get_question_resp(q54_obj)
head(q54)

## ---- fig.height=6, fig.width = 8----------------------------------------

# it could be nice to calculate a percentage to with some function or argument
q54 %>%
  group_by(quest_text, choice_text, matrix_num) %>%
  count() %>%
  ggplot(aes(x = choice_text, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~matrix_num, nrow = 2) +
  labs(title = "How Often You Use a Tool",
       x = "Frequency of Use",
       y = "Count")


## ---- fig.height=6, fig.width = 8----------------------------------------

# here, we just grab a df that has the data stacked in a way that is easily plottable
q54 <- get_question_resp(q54_obj,
                         choice_factor = TRUE, choice_rev = TRUE)
# it could be nice to calculate a percentage to with some function or argument
q54 %>%
  group_by(quest_text, choice_text, matrix_num) %>%
  count() %>%
  ggplot(aes(x = choice_text, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~matrix_num, nrow = 2) +
  labs(title = "How Often You Use a Tool\n Factors turned on",
       x = "Frequency of Use",
       y = "Count")


## ---- fig.height=6, fig.width = 8----------------------------------------

# here, we just grab a df that has the data stacked in a way that is easily plottable
q55 <- get_question_resp(my_survey_ob$questions$QID55)
# it could be nice to calculate a percentage to with some function or argument
q55 %>%
  group_by(quest_text, choice_text, matrix_num) %>%
  count() %>%
  ggplot(aes(x = choice_text, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~matrix_num, nrow = 2) +
  labs(title = "How Often You Use a Tool\n Factors turned on",
       x = "Frequency of Use",
       y = "Count")


## ---- fig.height=6, fig.width = 8----------------------------------------

# here, we just grab a df that has the data stacked in a way that is easily plottable
q55 <- get_question_resp(my_survey_ob$questions$QID55,
                         choice_factor = TRUE)
# it could be nice to calculate a percentage to with some function or argument
q55[[1]] %>%
  group_by(quest_text, choice_text) %>%
  count() %>%
  ggplot(aes(x = choice_text, y = n)) +
  geom_bar(stat = "identity") +
  labs(title = "How Often You Use a Tool\n Factors turned on",
       x = "Frequency of Use",
       y = "Count")


## ---- fig.height=6, fig.width = 8----------------------------------------

# here, we just grab a df that has the data stacked in a way that is easily plottable
q55 <- get_question_resp(my_survey_ob$questions$QID55,
                         choice_factor = TRUE,
                         choice_rev = TRUE)
# it could be nice to calculate a percentage to with some function or argument
q55[[1]] %>%
  group_by(quest_text, choice_text) %>%
  count() %>%
  ggplot(aes(x = choice_text, y = n)) +
  geom_bar(stat = "identity") +
  labs(title = "How Often You Use a Tool\n Factors turned on",
       x = "Frequency of Use",
       y = "Count")


