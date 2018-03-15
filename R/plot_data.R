# note - i'm using tidyr stuff i think and dplyr... get it all straight

#' get_choices
#'
#' Cleaned data.frame all ready for plotting
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.string
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom tidyr gather
#' @importFrom tidyr separate
#'
#' @param choices_df choices sub object pulled from the qsurvey object.
#'
#' @return data.frame of choices ready to be merged with the main question df
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


#' get_subq
#'
#' Cleaned data.frame all ready for plotting
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.string
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename
#'
#' @param subq_df choicse sub object pulled from the qsurvey object.
#'
#' @return data.frame of subquestions ready to be merged with the main question df
#' @export
#'


get_subq <- function(subq_df){
  fin_subq <- subq_df %>%
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
#' @param quest_obj the object within the qsurvey object that contains all relevant information to the question including choicses and subquestions IF those are relevant
#'
#' @return Nice data.frame ready for plotting. yeaaassss
#' @export
#'

get_question_resp <- function(quest_obj) {

  # then gather the data
  # note that "recode is the response value here (i *think*) TODO: look into json structure
  fin_resp <- quest_obj$responses %>%
    gather(key = "qnum", value = "response", -ResponseID)

  # if the object has choices then join the choices to the question for plotting annalysis
  if (!is.null(quest_obj$choices)){
    # if a question has no choicses i may be able to test for that in the object?? not sure
  choices <- get_choices(quest_obj$choices)

  fin_resp <- fin_resp %>%
    left_join(choices, by = c("response" = "recode"))
  }

  if (!is.null(quest_obj$subquestions)){
  # if there are subquestions
  subq <- get_subq(quest_obj$subquestions)

  # merge subquestions with df for plotting
  fin_resp <- fin_resp %>%
    separate(qnum, into = c("qnum", "subqnum"), sep = "_") %>%
    mutate(subqnum = as.integer(subqnum)) %>%
    left_join(subq, by = c("subqnum" = "recode"))
  }
  return(fin_resp)
}
