## ----connect, message=FALSE----------------------------------------------
options(stringsAsFactors = FALSE)
library(qtoolkit)
# still working on only importing the pipe element in the package.
# https://github.com/sckott/analogsea/issues/32
# note currently i need to just load the pipe and also stringsasfactors as false
library(dplyr)
library(tidyr)

## ----eval = FALSE--------------------------------------------------------
#  ## Connect to Qualtrics API
#  qapi_connect()

## ----view-survey-list----------------------------------------------------
# get a list of all surveys connected to your account / api token
all_surveys <- list_surveys()
head(all_surveys)


