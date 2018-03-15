## ----connect, message=FALSE----------------------------------------------
options(stringsAsFactors = FALSE)
library(qtoolkit)
# still working on only importing the pipe element in the package.
# https://github.com/sckott/analogsea/issues/32
# note currently i need to just load the pipe and also stringsasfactors as false
library(dplyr)
## Connect to Qualtrics API
qapi_connect()


## ----view-survey-list----------------------------------------------------
# get a list of all surveys connected to your account / api token
all_surveys <- list_surveys()
head(all_surveys)


## ----get-survey----------------------------------------------------------
# define survey id variable, then get survey object
survey_id <- "SV_1SUpa4C4UGkZnWB"
# currently there are some issues with factors that i need to fix via hard code
# if strings as factors isn't false, then you get warnings as R coerces to char
my_survey_ob <- qsurvey(survey_id)

## ----view-all-questions--------------------------------------------------
# view all questions
my_survey_ob$questionList


## ------------------------------------------------------------------------
my_survey_ob <- qsurvey(survey_id,
                        strip_html = FALSE)
# view all questions
my_survey_ob$questionList

## ------------------------------------------------------------------------
my_survey_ob <- qsurvey(survey_id,
                        strip_html = TRUE)
# view all questions
all_questions <- my_survey_ob$questionList
all_questions

## ------------------------------------------------------------------------
# get responses and question information for qid71.
q4 <- my_survey_ob$questions$QID4
str(q4)

## ------------------------------------------------------------------------
# it would be nice if this took the choices and made them factors
# should this be a default for a likert scale?
question_responses <- get_question_resp(q4)
head(question_responses)

## ------------------------------------------------------------------------
library(ggplot2)
question_responses %>%
  group_by(question, qchoice) %>%
  count() %>%
  ggplot(aes(x = qchoice, y = n)) +
  geom_bar(stat = "identity")


## ------------------------------------------------------------------------
question_responses %>%
  group_by(question, qchoice) %>%
  count() %>%
  ggplot(aes(x = qchoice, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~question, , ncol = 2)


