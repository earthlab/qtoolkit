# note - i'm using tidyr stuff i think and dplyr... get it all straight

#' get_choices
#'
#' Cleaned data.frame all ready for plotting 
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.string
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#'
#' @param quest_df question object pulled from the qsurvey object. This contains all of the data that you want to plot
#'
#' @return Nice data.frame ready for plotting
#' @export
#' 


get_choices <- function(choices_df) {
  choices <- choices_df %>% 
    select(order, recode, text) %>% 
    mutate(text = strip_html(text),
           recode = as.integer(recode)) %>% 
    rename(qchoice = text)
  return(choices)
}

get_subq <- function(subquestions_df){
  fin_subq <- subquestions_df %>% 
    mutate(desc = strip_html(desc),
           recode = as.integer(recode)) %>% 
    rename(question = text, qdescription = desc)
  return(fin_subq)
}


#' get_data
#'
#' Cleaned data.frame all ready for plotting 
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.string
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#'
#' @param quest_df question object pulled from the qsurvey object. This contains all of the data that you want to plot
#'
#' @return Nice data.frame ready for plotting
#' @export
#' 

get_ques_resp <- function(question) {
  
  # then gather the data
  fin_resp <- question$responses %>% 
    gather(key = "qnum", value = "recode", -ResponseID)
  
  # if it has choices (define questions types then)
  # if a question has no choicses i may be able to test for that in the object?? not sure
  choices <- get_choices(question$choices)
  
  fin_resp <- fin_resp %>% 
    left_join(choices, by = c("recode" = "recode"))
  
  # if there are subquestions 
  subq <- get_subq(question$subquestions)
  
  # merge subquestions with df for plotting
  fin_resp <- fin_resp %>%
    separate(qnum, into = c("qnum", "subqnum"), sep = "_") %>% 
    mutate(subqnum = as.integer(subqnum)) %>% 
    left_join(subq, by = c("subqnum" = "recode"))
}