#' qquestion
#'
#' Qualtrics Survey Question object
#'
#' @param q_meta Named list of question metadata
#' @param q_resp DF of question responses
#'
#' @return Qualtrics Survey Question object
#' @export

qquestion <- function(q_meta, q_resp) {

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
    q_extra <- get(q_fn)(q_meta, q_resp)
    qq <- c(qq, q_extra)
  }

  ## Build and return class
  class(qq) <- c("qquestion", qq$type)

  return(qq)
}

qquestion.MC <- function(q_meta, q_resp) {

  choices <- nested_list_to_df(q_meta$choices)

  qq_extra <- list(
    # this should be an equal sign to create a list... i think
      choices = auto_reformat(choices,
                               reorder.rows = FALSE,
                               strip.html = TRUE)
  )

  return(qq_extra)
}

# the problem here is that strip_html is occuring in too many places
# thus it's very difficult to determine whether it's being implemented correctly
# throughout the survey 
# auto reformat really needs to be broken down into many sub functions and cleaned up
# so it's easier to manage all of this

qquestion.Matrix <- function(q_meta, q_resp) {

  choices <- nested_list_to_df(q_meta$choices)
  subquestions <- nested_list_to_df(q_meta$subQuestions)
  
  qq_extra <- list(
    choices = auto_reformat(choices,
                            reorder.rows = FALSE,
                            strip.html = TRUE),
    subquestions = auto_reformat(subquestions,
                                 reorder.rows = FALSE,
                                 strip.html = TRUE)
  )
  return(qq_extra)
  
}
