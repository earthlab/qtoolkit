## ----connect, message=FALSE----------------------------------------------
#options(stringsAsFactors = FALSE)
library(qtoolkit)
# still working on only importing the pipe element in the package.
# https://github.com/sckott/analogsea/issues/32
# note currently i need to just load the pipe and also stringsasfactors as false
library(ggplot2)
library(dplyr)
library(tidyr)

## ----eval = FALSE--------------------------------------------------------
#  ## Connect to Qualtrics API
#  qapi_connect()

## ----view-survey-list----------------------------------------------------
# get a list of all surveys connected to your account / api token
all_surveys <- list_surveys()
head(all_surveys)


## ----get-survey----------------------------------------------------------
# define survey id variable, then get survey object
survey_id <- "SV_1SUpa4C4UGkZnWB"
# note the warnings are OK. it's just a reminder to me that some questions are not
# processed "custom" i can turn these off once i'm confident that things work
# across all question types!
my_survey_ob <- qsurvey(survey_id)

## ----view-all-questions--------------------------------------------------
# view all questions
my_survey_ob$questionList


## ------------------------------------------------------------------------
my_survey_ob <- qsurvey(survey_id,
                        clean_html = FALSE)
# view all questions
my_survey_ob$questionList

## ------------------------------------------------------------------------
my_survey_ob <- qsurvey(survey_id,
                        clean_html = TRUE)
# view all questions
all_questions <- my_survey_ob$questionList
all_questions

## ------------------------------------------------------------------------
# get responses and question information for qid71.
q4 <- my_survey_ob$questions$QID4
str(q4)

## ------------------------------------------------------------------------
# get responses and question information for qid71.
my_survey_ob$questions$QID4$choices

## ------------------------------------------------------------------------
# get responses and question information for qid71.
my_survey_ob$questions$QID4$responses


## ------------------------------------------------------------------------
# get responses and question information for qid71.
my_survey_ob$questions$QID4$subquestions

## ------------------------------------------------------------------------
# here, we just grab a df that has the data stacked in a way that is easily plottable
question_responses <- get_question_resp(q4)
head(question_responses)

## ------------------------------------------------------------------------

# it could be nice to calculate a percentage to with some function or argument
question_responses %>%
  group_by(quest_text, choice_text) %>%
  count() %>%
  ggplot(aes(x = choice_text, y = n)) +
  geom_bar(stat = "identity") +
  labs(title = "How Often You Use a Tool",
       x = "Frequency of Use",
       y = "Count")


## ----ques-resp-plot, fig.height=6, fig.width = 8-------------------------
# it could be nice to calculate a percentage to with some function or argument
question_responses %>%
  group_by(quest_text, choice_text) %>%
  count() %>%
  ggplot(aes(x = choice_text, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~quest_text, ncol = 2) +
  labs(title = "How Often You Use a Tool",
       x = "Frequency of Use",
       y = "Count")


## ---- fig.height = 6, fig.width = 8--------------------------------------

question_responses <- get_question_resp(q4,
                                        quest_wrap = NULL,
                                        choice_wrap = 8,
                                        choice_factor = TRUE)
head(question_responses)

question_responses %>%
  group_by(quest_text, choice_text) %>%
  count() %>%
  ggplot(aes(x = choice_text, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~quest_text, ncol = 2) +
  labs(title = "How Often You Use a Tool",
       x = "Frequency of Use",
       y = "Count")

## ---- fig.height=6, fig.width = 8----------------------------------------
question_responses <- get_question_resp(q4,
                                        quest_wrap = NULL,
                                        choice_wrap = 8,
                                        choice_factor = TRUE,
                                        choice_rev = TRUE)
head(question_responses)

question_responses %>%
  group_by(quest_text, choice_text) %>%
  count() %>%
  ggplot(aes(x = choice_text, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~quest_text, ncol = 2) +
  labs(title = "How Often You Use a Tool",
       x = "Frequency of Use",
       y = "Count")

