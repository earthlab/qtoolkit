#' survey
#'
#' qToolkit survey object
#'
#' @param survey_id
#'
#' @return qToolkit survey object for survey_id

survey <- function(survey_id,
                   strip.html = TRUE) {

  s_meta <- qapi_get_survey(survey_id)
  s_resp <- qapi_response_export(survey_id)

  ## Strip HTML or keep text the same?
  parse_fn <- if (strip.html) strip_html else c

  ## Build the $questions dataframe and the 
  ## And a list of lists for the $choices dataframe
  parse_qs <- function(qid) {
    q <- s_meta$questions[[qid]]

    q_row <- list(
        id = qid,
        name = q$questionName,
        text = parse_fn(q$questionText),
        type = q$questionType$type,
        type_selector = q$questionType$selector,
        type_subselector = q$questionType$subSelector,
        required = q$validation$doesForceResponse
    )
    ## If the question we're parsing has choices, add those to
    ## choices_df
    if (!is.null(q$choices)) {
      s_cs <- t(sapply(q$choices, parse_cs, qid=qid))
      choices_df <<- rbind(choices_df, s_cs)
    }

    if (!is.null(q$subQuestions)) {
      s_sqs <- t(sapply(q$choices, parse_sqs, qid=qid))
      subqs_df <<- rbind(subqs_df, s_sqs)
    }
    
    return(q_row)
  }

  ## Parse choices function
  parse_cs <- function(c, qid) {
    c_row <- list(
        qid = qid,
        text = c$choiceText,
        desc = c$description,
        image_desc = c$imageDescription,
        recode = c$recode
    )
  }

  ## Parse subquestions function
  parse_sqs <- function(sq, qid) {
    sq_row <- list(
        qid = qid,
        text = sq$choiceText,
        desc = sq$description,
        image_desc = sq$imageDescription,
        recode = sq$recode,
        variable_name = sq$variableName
    )
  }

  ## Empty DF for choices & subquestions to be populated
  choices_df <- data.frame()
  subqs_df <- data.frame()
  
  s_qids <- names(s_meta$questions)
  s_qs <- as.data.frame(t(sapply(s_qids, parse_qs)))

  ## TODO:: define data types of the columns?? Right now columns
  ## of DF are of type list, not integer, ...etc
  
  ## TODO:: Build the $responses dataframe
}
