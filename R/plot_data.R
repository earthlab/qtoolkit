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

get_choices <- function(choices_df, choice_wrap = NULL, choice_factor = FALSE, choice_rev = FALSE) {
  choices <- choices_df %>%
    dplyr::select(quest_order, recode, choice_text) %>%
    mutate(recode = as.integer(recode)) %>%
    rename(choice_order = quest_order)
  
  # if they chose to wrap for choices for prettier plotting
  if (!is.null(choice_wrap)) {
    choices$choice_text <- split_strings(string = choices$choice_text,
                                         nchar = choice_wrap)
  }
  
  # if they chose to turn on factors for choices
  if (choice_factor) {
    # make sure df is sorted by order - but offer the ablity to reverse this order
    # there is probably a more clever way to handle this argument as the code below is redundant
    if (!choice_rev) {
    choices <- choices %>% 
      arrange(choice_order) %>% 
      mutate(choice_text = factor(choice_text, levels = choice_text))
      } else {
    choices <- choices %>% 
      arrange(desc(choice_order)) %>% 
      mutate(choice_text = factor(choice_text, levels = choice_text))
      }
  }
  
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
#' @param subq_df choices sub object pulled from the qsurvey object.
#'
#' @return data.frame of subquestions ready to be merged with the main question df
#' @export
#'

# this renaming should happen in the "auto reformat realm... but a hack is here
# so i can start using this package now.
get_subq <- function(subq_df){
  fin_subq <- subq_df %>%
    mutate(recode = as.integer(recode)) %>%
    rename(quest_text = choice_text, quest_description = choice_desc)
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
#' @param quest_wrap an integer value defining the number of characters at which you want to specify a line break for each question at for prettier plotting. default = NULL which means no wrap
#' @param choice_wrap an integer value defining the number of characters at which you want to specify a line break for each CHOICE at for prettier plotting. default = NULL which means no wrap
#' @param choice_factor boolean set to TRUE if you want choices to be turned into a factor and ordered by the order provided in qualtrics (this is the order that you provided to the user in the survey)
#' @param choice_rev boolean set to TRUE if you want to reverse the order of choices 
#' @return Nice data.frame ready for pretty plotting. 
#' @export
#'

get_question_resp <- function(quest_obj, 
                              quest_wrap = NULL, 
                              choice_wrap = NULL, 
                              choice_factor = FALSE,
                              choice_rev = FALSE) {

  # then gather the data
  # note that "recode is the response value here (i *think*) TODO: look into json structure
  fin_resp <- quest_obj$responses %>%
    gather(key = "qnum", value = "response", -ResponseID)

  # if the object has choices then join the choices to the question for plotting annalysis
  if (!is.null(quest_obj$choices)){
    # get choices and wrap / convert to factor if specified to do so
    ## TO DO:: we could consider adding a second column that is a factor with split text...
    ## need to consider all use cases here...split text could be annoying for some applications
  choices <- get_choices(quest_obj$choices, 
                         choice_wrap, 
                         choice_rev = choice_rev,
                         choice_factor = choice_factor)
  # if this is a multiple choice quqestion this fails because you get a 1 for each option thus you cant join on response
  # this function is becoming very long. might be better to try to consolidate just a bit into sub functions
  # note does this work with both types of MC questions?
  if (quest_obj$meta$type == "MC") {
    fin_resp <-  fin_resp %>%
      separate(qnum, sep = "_", c("quest", "recode")) %>% 
      mutate(recode = as.integer(recode)) %>% 
      left_join(choices, by = c("recode" = "recode"))
  } else {
  fin_resp <- fin_resp %>%
    left_join(choices, by = c("response" = "recode"))
  }
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
