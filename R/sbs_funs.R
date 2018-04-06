
#' FUN: get_sbs
#' 
#' This function cleans up the columns in a SBS matrix so that is it really for 
#' joining and final plotting. If the matrices have difference choice options, AND factors are set to TRUE
#' then it returns a list containing a DF for each sub matrix. Otherwise it returns a regular df. 
#' It's not ideal for a function to return 2 diff data types but in this instance, there may not be a better way if you want factors
#' 
#' @param quest_obj the object within the qsurvey object that contains all relevant information to the question including choicses and subquestions IF those are relevant
#' @param quest_wrap an integer value defining the number of characters at which you want to specify a line break for each question at for prettier plotting. default = NULL which means no wrap
#' @param choice_wrap an integer value defining the number of characters at which you want to specify a line break for each CHOICE at for prettier plotting. default = NULL which means no wrap
#' @param choice_factor boolean set to TRUE if you want choices to be turned into a factor and ordered by the order provided in qualtrics (this is the order that you provided to the user in the survey)
#' @param choice_rev boolean set to TRUE if you want to reverse the order of choices 
#' @return data.frame of choices ready to be merged 
#' @export
#'

get_SBS <- function(quest_obj, 
                    quest_wrap = NULL, 
                    choice_wrap = NULL, 
                    choice_factor = FALSE,
                    choice_rev = FALSE) {
  # gather responses 
  fin_resp_g <- gather_responses(quest_obj)
  # get_choices
  choices <- get_choices_sbs(quest_obj$choices, 
                             choice_wrap = choice_wrap, 
                             choice_rev = choice_rev,
                             choice_factor = choice_factor)
  # clean up final response data 
  fin_resp <- clean_resp_sbs(fin_resp_g)
  
  #### GET SUBQUESTIONS ###
  subq <- get_subq(quest_obj$subquestions)
  
  # join choices to response data and handle factors if turned on and diff in each matrix
  if (class(choices) == "list") {
    # split out by sub matrix
    fin_resp_list <- split(fin_resp, fin_resp$col)
    # both lists should be the same length if this is correct
    assertthat::are_equal(length(choices), length(fin_resp_list))
    # this really does seem stupidly complex but loop through each matrix and join accordingly
    for (i in 1:length(choices)) {
      # loop through and join 
      fin_resp_list[[i]] <- fin_resp_list[[i]] %>% 
        left_join(choices[[i]], by = c("col_response" = "col_response")) %>% 
        rename(col = col.x) %>% 
        select(-col.y) %>% 
        left_join(subq, by = c("subqnum" = "subqnum"))
    }
    fin_resp <- fin_resp_list
  } else {
    fin_resp <- fin_resp %>% 
      left_join(choices, by = c("col_response" = "col_response")) %>% 
      left_join(subq, by = c("subqnum" = "subqnum"))
  }
  
  return(fin_resp)
}

#' FUN: clean_resp_sbs
#' 
#' This function cleans up the columns in a SBS matrix so that is it really for 
#' joining and final plotting 
#' 
#' @param df a df object derived from Qtoolkit with the question and response values
#'
#' @return data.frame of choices ready to be merged 
#' @export
#'

clean_resp_sbs <- function(df) {
  
  fin_resp <-  df %>%
    separate(qnum, sep = "_", c("quest", "subqnum")) %>% 
    separate(quest, sep = "\\.", c("quest", "col")) %>% 
    drop_na() %>% 
    mutate(col_response = paste0(col, "_", response),
           subqnum = as.integer(subqnum),
           col = as.integer(col))
  return(fin_resp)
  
}

#' FUN: get_choices_sbs
#' 
#' This function is specific for side by side (SBS) matrices. SBS matrices have a col
#' column which dilineates the sub matrix that the question choicses are in
#' Some use cases will have the same choicses in each submatrix. Others will have different
#' choices making the work flow more complex
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

get_choices_sbs <- function(choices_df, 
                            choice_wrap = NULL, choice_factor = FALSE, choice_rev = FALSE) {
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
      print("You are trying to process a side by side matrix that have multiple sets of choices. Two sets of factors can not be stored in the 
            same column. Thus i've created a list containing a df for each matrix in the sbs matrix.")
      #split_choices <- list()
      # then apply factors to the choice column for each element in the list
      # prolly can avoid a loop but... not a big time savings
      for (i in 1:length(choices_list)) {
        # print(i)
        choices_list[[i]] <- choices_list[[i]] %>% 
          choice_to_factor(choice_rev = choice_rev)  
      }
      choices <- choices_list
    } else {
      # make sure df is sorted by order - but offer the ablity to reverse this order
      choices <- choice_to_factor(choices, 
                                  choice_rev = choice_rev)
      
      # if (!choice_rev) {
      #   choices <- choices %>% 
      #     arrange(choice_order) %>% 
      #     mutate(choice_text = factor(choice_text, levels = choice_text))
      # } else {
      #   choices <- choices %>% 
      #     arrange(desc(choice_order)) %>% 
      #     mutate(choice_text = factor(choice_text, levels = choice_text))
      # }
    }
  }
  return(choices)
}