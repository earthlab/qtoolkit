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

#' choice_to_factor
#'
#' Cleaned data.frame of question choicses ready for plotting. This function
#' works for regular matrix and multiple choices questions. It calls a SBS matrix specific
#' function if a sbs matrix question is provided
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.string
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#'
#' @param choices_df choices sub object (DF) pulled from the qsurvey object.
#' @param choice_factor BOOL, whether you want choicse to be a factor or not DEFAULT = FALSE
#' @param choice_rev BOOL, whether you want to SWITCH the order of factored choices. REQUIRES factor = TRUE to be set DEFAULT = FALSE
#'
#' @return data.frame of choices ready to be merged with the main question df
#' @export
#'

choice_to_factor <- function(df,
                             choice_rev = FALSE) {

  # Turn on factors if factors are selected as an arg
  # make sure choices are sorted by order - rev this order if argument = TRUE
  if (!choice_rev) {
    df_fact <- df %>%
      arrange(choice_order)
  } else {
    df_fact <- df %>%
      arrange(desc(choice_order))
  }
  # apply factors to the df
  df_fact <- df %>%
    mutate(choice_text = factor(choice_text, levels = unique(choice_text)))
  return(df_fact)
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

get_choices <- function(choices_df,
                        choice_wrap = NULL,
                        choice_factor = FALSE,
                        choice_rev = FALSE) {

  choices <- choices_df %>%
    dplyr::select(quest_order, recode, choice_text) %>%
    mutate(recode = as.integer(recode)) %>%
    rename(choice_order = quest_order,
           choice_code = recode)

  # if they chose to wrap for choices for prettier plotting
  if (!is.null(choice_wrap)) {
    choices$choice_text <- split_strings(string = choices$choice_text,
                                         nchar = choice_wrap)
  }

  # i think this would be good to do at the end so it can be handled in the main function
  # it could be a sub function called choices to factors
  # Turn on factors if factors are selected as an arg
  if (choice_factor) {
    # apply factors and reorder if rev set to TRUE
    choices <- choice_to_factor(choices,
                                choice_rev = choice_rev)
    # make sure choices are sorted by order - rev this order if argument = TRUE
    # this can be deleted given the function above i think!!!
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
    rename(quest_text = choice_text,
           quest_description = choice_desc,
           subqnum = recode)
  return(fin_subq)
}

#' gather_responses
#'
#' Cleaned data.frame all ready for plotting
#'
#' @importFrom assertthat assert_that
#' @importFrom tidyr gather
#'
#' @param quest_obj the object within the qsurvey object that contains all relevant information to the question including choicses and subquestions IF those are relevant
#' @return Nice data.frame ready for pretty plotting.
#' @export
#'

gather_responses <- function(quest_obj) {
  # gather responses into 2 cols
  fin_resp_g <- quest_obj$responses %>%
    gather(key = "qnum", value = "response", -ResponseID) %>%
    drop_na()
  return(fin_resp_g)
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
  # this line of code should be run regardless -- add to end of if statement chain
  #fin_resp_g <- gather_responses(quest_obj)

  # then return responses based on question type
  # could use code to generate function based on question type

  ## Call a function based upon question type to add more parameters
  q_fn <- paste0("get_", quest_obj$meta$type)

  # this populates choices and subquestions FOR the correct question type (matrix, mc)
  # if (exists(q_fn)) {
  #   q_extra <- get(q_fn)(quest_obj, quest_wrap, choice_wrap, choice_factor, choice_rev)
  #   qq <- c(qq, q_extra)
  # } else {
  #   print("Warning: A function does not exist to get responses for question type. Returning responses only.")
  #   print(q_fn)
  #fin_resp_g <- gather_responses(quest_obj)
  # }

  # but starting with if statements to test all of the potential hangups
  # essentially the above can run as i build question specific functions and then
  # a generic function runs if there is no function to cover things to allow for
  # flexibility !!

  if (quest_obj$meta$type == "SBS") {
    fin_resp <- get_SBS(quest_obj, quest_wrap, choice_wrap, choice_factor, choice_rev)

  } else if (quest_obj$meta$type == "MC") {
    # gather responses
    fin_resp_g <- gather_responses(quest_obj)
    # get_choices
    choices <- get_choices(quest_obj$choices,
                               choice_wrap = choice_wrap,
                               choice_rev = choice_rev,
                               choice_factor = choice_factor)
    # clean multiple choice data
    # note a warning is returned here if there are text responses in the data
    # it would be good to make this more user friendly warning in the future!
    fin_resp <-  fin_resp_g %>%
      separate(qnum, sep = "_", c("quest", "recode")) %>%
      mutate(recode = as.integer(recode)) %>%
      left_join(choices, by = c("recode" = "recode"))

  } else {
    # if it's not MC or SBS... generic cleanup
    fin_resp <- gather_responses(quest_obj)
    if (!is.null(quest_obj$choices)) {
      # get_choices
      choices <- get_choices(quest_obj$choices,
                             choice_wrap = choice_wrap,
                             choice_rev = choice_rev,
                             choice_factor = choice_factor)
    # if there are choices join to the data
     fin_resp <- fin_resp %>%
       left_join(choices, by = c("response" = "choice_code"))
    }

    if (!is.null(quest_obj$subquestions)){
      # if there are subquestions join those too
      subq <- get_subq(quest_obj$subquestions)
      # merge subquestions with df for plotting
      # this may need to be rewritten for various question types...
      fin_resp <- fin_resp %>%
        separate(qnum, into =  c("qnum", "subqnum"), sep = "_") %>% 
        mutate(subqnum = as.integer(subqnum)) %>% 
        left_join(subq, by = c("subqnum" = "subqnum"))
    }
  }
  return(fin_resp)
}
