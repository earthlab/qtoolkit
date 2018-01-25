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

  ## Standard attribues that every question has
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
      choices = auto_reformat(choices)
  )

  return(qq_extra)
}

qquestion.Matrix <- function(q_meta, q_resp) {

  choices <- nested_list_to_df(q_meta$choices)
  subquestions <- nested_list_to_df(q_meta$subQuestions)
  
  qq_extra <- list(
    choices = auto_reformat(choices),
    subquestions = auto_reformat(subquestions)
  )
  return(qq_extra)
  
}
