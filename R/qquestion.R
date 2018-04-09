#' qquestion
#'
#' Qualtrics Survey Question object
#'
#' @param q_meta Named list of question metadata
#' @param q_resp DF of question responses
#' @param clean_html BOOLEAN - determines whether to strip the html surrounding text or not
#'
#' @return Qualtrics Survey Question object
#' @export

qquestion <- function(q_meta, q_resp, clean_html) {

  ## Standard attributes that every question has
  qq <- list(
    meta       = list(
      qid         = q_meta$questionID,
      order       = q_meta$questionOrder,
      name        = q_meta$questionName,
      type        = q_meta$questionType$type,
      selector    = q_meta$questionType$selector,
      subselector = q_meta$questionType$subselector,
      text        = q_meta$questionText,
      label       = q_meta$questionLabel,
      required    = q_meta$validation$doesForceResponse
    ),
    responses  = q_resp,
    validation = q_meta$validation
  )
  
  ## Turn metadata list NULLs to NA and convert to dataframe
  qq$meta <- lapply(qq$meta, function(e) { if (is.null(e)) return(NA) else return(e) })
  qq$meta <- as.data.frame(qq$meta)

  ## Call a function based upon question type to add more parameters
  q_fn <- paste0("qquestion.", qq$meta$type)
  
  # this populates choices and subquestions FOR the correct question type (matrix, mc)
  if (exists(q_fn)) {
    q_extra <- get(q_fn)(q_meta, q_resp, clean_html)
    qq <- c(qq, q_extra)
  } else {
    print("Warning: A function does not exist to handle this object type but the data have been processed. Please check the output data carefully.")
    print(q_fn)
  }

  ## Build and return class
  class(qq) <- c("qquestion", qq$type)

  return(qq)
}

qquestion.MC <- function(q_meta, q_resp, clean_html) {

  choices <- nested_list_to_df(q_meta$choices)

  qq_extra <- list(
      choices = auto_reformat(choices,
                               reorder.rows = FALSE,
                               strip.html = clean_html)
  )

  return(qq_extra)
}

# the problem here is that strip_html is occuring in too many places
# thus it's very difficult to determine whether it's being implemented correctly
# throughout the survey 
# auto reformat really needs to be broken down into many sub functions and cleaned up
# so it's easier to manage all of this

qquestion.Matrix <- function(q_meta, q_resp, clean_html) {

  choices <- nested_list_to_df(q_meta$choices)
  subquestions <- nested_list_to_df(q_meta$subQuestions)
  
  qq_extra <- create_qq_extra(choices, subquestions, clean_html)
  
  return(qq_extra)
  
}

qquestion.SBS <- function(q_meta, q_resp, clean_html) {
  
  # The diff between structure is significant enough to warrant another function i think

  subquestions <- nested_list_to_df(q_meta$subQuestions)
  # within the columns we find the choices
  # loop through each column and get the choices  
  choice_list <- list()
  for (j in 1:length(q_meta$columns)){
    choices <- nested_list_to_df(q_meta$columns[[j]]$choices)
    # add column number to choices just in case there are dif between columns
    choices$col <- j
    choice_list[[j]] <- choices
  }
   choices <-  bind_rows(choice_list)
   
   qq_extra <- create_qq_extra(choices, subquestions, clean_html)
   # add columns to the list
   columns <- bind_rows(q_meta$columns[[j]]$questionType) %>% 
     mutate(question = q_meta$columns[[j]]$questionText,
            q_label = q_meta$columns[[j]]$questionLabel,
            validation = q_meta$columns[[j]]$validation$doesForceResponse,
            col_num = j)
   # could this ultimately be added to the create_qq function above?? 
   qq_extra$matrix_columns <- columns
  
  return(qq_extra)
}

create_qq_extra <- function(choices, subquestions, clean_html) {
  qq_extra <- list(
    choices = auto_reformat(choices,
                            reorder.rows = FALSE,
                            strip.html = clean_html),
    subquestions = auto_reformat(subquestions,
                                 reorder.rows = FALSE,
                                 strip.html = clean_html)
  )
    return(qq_extra)
}

