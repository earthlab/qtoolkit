# note - i'm using tidyr stuff i think and dplyr... get it all straight


#' FUN: test_sbs_choices
#' This function will test to see whether the choicses for each side by side matrix are the same
#' If they are different, then it will not allow factors to be created. If they are the same for 
#' each matrix then factors can be applied.
#' 
#' @param choices_list choices list from the qsurvey object for a sbs (side-by-side) matrix.
#'
#' @return boolean TRUE if the choices in each matrix are the same FALSE if not
#' @export
#'
#'
test_sbs_choices <- function(choices_list){
  # first assume each sbs matrix has the same choices
  # then test that assumption
  all_same <- TRUE
  for (i in 1:(length(choices_list) - 1)) {
    if (all_same == FALSE) break
    # if the df are different lengths, they can't be converted
    # you can't compare 2 df of different lengths which is why this is here
    if (!nrow(choices_list[[i]]) == nrow(choices_list[[i+1]])){
      print("the choice df are different lengths - i can not create factors")
      all_same <- FALSE  
    } else {
      # if the df length is the same, test to see if the options are diff
      all_same <- all(choices_list[[i]] == choices_list[[i+1]])
    }
  }
  return(all_same)
}


#' FUN: get_choices_sbs
#' 
#' This function is specific for side by side matrices. These survey elemtns are more complex
#' than regular matrices because they contain several sub matrices and thus a different
#' structure that needs to be accounted for. This function handles factors associated with 
#' choices in each matrix that may or may not be the same.
#' 
#' @param choices_df choices sub object (DF) pulled from the qsurvey object.
#' @param choice_wrap int - the number of characters at which you want to wrap a line, DEFAULT = NULL
#' @param choice_factor BOOL, whether you want choicse to be a factor or not DEFAULT = FALSE 
#' @param choice_rev BOOL, whether you want to SWITCH the order of factored choices. REQUIRES factor = TRUE to be set DEFAULT = FALSE
#'
#' @return data.frame of choices ready to be merged with the main question df with ordered factors
#' applied if selected
#' @export
#'

get_choices_sbs <- function(choices_df, choice_wrap = NULL, choice_factor = FALSE, choice_rev = FALSE) {
  # get choices columns (including col which is the sbs matrix num)
  choices <- choices_df %>%
    dplyr::select(quest_order, recode, choice_text, col) %>%
    mutate(recode = as.integer(recode)) %>%
    rename(choice_order = quest_order) %>% 
    mutate(col_response = paste0(col, "_", recode)) # for joining
  
  # wrap text choicses if argument = T for prettier plotting
  if (!is.null(choice_wrap)) {
    choices$choice_text <- split_strings(string = choices$choice_text,
                                         nchar = choice_wrap)
  }
  
  # if factors=T, test to determine whether the choices are the same 
  # in each sub matrix if not, don't apply factors
  if (choice_factor) {
    # create a list of all df.s by matrix oder
    choices_list <- split(choices, choices$col)
    # determine if choices in each matrix are diff (all_same=F), don't apply factors if diff
    all_same <- test_sbs_choices(choices_list)
    
    # warning to the user if they turned on factors in a SBS matrix
    if (all_same == FALSE) {
      print("You are trying to process a side by side matrix that have multiple 
              sets of choices. Two sets of factors can not be stored in the 
              same column. Thus factors were not set. Please consider plotting 
              these matrices separately.")
    } else {
      # make sure df is sorted by order - but offer the ablity to reverse this order
      # This should be a function as it's repeated twice in this script
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
  }
    return(choices)
}

#' get_choices
#'
#' Cleaned data.frame of question choicses ready for plotting. This function 
#' works for regular matrix and multiple choices questions. It calls a SBS matrix specific
#' function if a sbs matrix question is provided 
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.string
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom tidyr gather
#' @importFrom tidyr separate
#'
#' @param choices_df choices sub object (DF) pulled from the qsurvey object.
#' @param choice_wrap int - the number of characters at which you want to wrap a line, DEFAULT = NULL
#' @param choice_factor BOOL, whether you want choicse to be a factor or not DEFAULT = FALSE 
#' @param choice_rev BOOL, whether you want to SWITCH the order of factored choices. REQUIRES factor = TRUE to be set DEFAULT = FALSE
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
  
  # Turn on factors if factors are selected as an arg
  if (choice_factor) {
    # make sure choices are sorted by order - rev this order if argument = TRUE
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
#' Get all of the sub questions associated with a question. Returns a cleaned data.frame all ready for plotting
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
  
  # first gather the response data 
  fin_resp_g <- quest_obj$responses %>%
    gather(key = "qnum", value = "response", -ResponseID)
  
  # if the object has choices then join the choices to question
  if (!is.null(quest_obj$choices)){
    # get choices and wrap / convert to factor if specified to do so
    ## TO DO:: we could consider adding a second column that is a factor with split text...
    ## need to consider all use cases here...split text could be annoying for some applications
    
    # here because we have a sbs - we have 2 sets of choices
    # but we don't have the column which will make it very hard to join
    if (quest_obj$meta$type == "SBS") {
      choices <- get_choices_sbs(quest_obj$choices, 
                                 choice_wrap = choice_wrap, 
                                 choice_rev = choice_rev,
                                 choice_factor = choice_factor)
    } else {  
      choices <- get_choices(quest_obj$choices, 
                             choice_wrap = choice_wrap, 
                             choice_rev = choice_rev,
                             choice_factor = choice_factor)
    }
    # if this is a multiple choice quqestion this fails because you get a 1 for each option thus you cant join on response
    # this function is becoming very long. might be better to try to consolidate just a bit into sub functions
    # note does this work with both types of MC questions?
    if (quest_obj$meta$type == "MC") {
      fin_resp <-  fin_resp_g %>%
        separate(qnum, sep = "_", c("quest", "recode")) %>% 
        mutate(recode = as.integer(recode)) %>% 
        left_join(choices, by = c("recode" = "recode"))
    } else if (quest_obj$meta$type == "SBS") {
      fin_resp <-  fin_resp_g %>%
        separate(qnum, sep = "_", c("quest", "subqnum")) %>% 
        separate(quest, sep = "\\.", c("quest", "col")) %>% 
        mutate(col_response = paste0(col, "_", response)) %>% 
        left_join(choices, by = c("col_response" = "col_response"))
    } else {
    fin_resp <- fin_resp %>%
      left_join(choices, by = c("response" = "recode"))
  }
}
if (!is.null(quest_obj$subquestions)){
  # if there are subquestions
  subq <- get_subq(quest_obj$subquestions) %>% 
    rename(subqnum = recode)
  
  # merge subquestions with df for plotting
  # this may need to be rewritten for various question types...
  fin_resp <- fin_resp %>%
    #separate(qnum, into = c("qnum", "subqnum"), sep = "_") %>%
    mutate(subqnum = as.integer(subqnum)) %>%
    
    left_join(subq, by = c("subqnum" = "subqnum"))
}
return(fin_resp)
}
