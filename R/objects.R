#' survey
#'
#' qToolkit survey object
#'
#' @param survey_id
#'
#' @return qToolkit survey object for survey_id
#' @export

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
    
    s_qs <- data.frame(
        id = qid,
        name = q$questionName,
        text = parse_fn(q$questionText),
        type = q$questionType$type,
        type_selector = q$questionType$selector,
        type_subselector = q$questionType$subSelector,
        required = q$validation$doesForceResponse
    )
    
    qs_df <<- rbind(qs_df, s_qs)

    ## If the question we're parsing has choices or subquestions, add
    ## those to choices_df or subqs_df, respectively, incl the order    
    if (!is.null(q$choices)) {    
      cs_order <<- 1
      cs_parse <- sapply(q$choices, parse_cs, qid = qid)
    }

    if (!is.null(q$subQuestions)) {
      sqs_order <<- 1
      sqs_parse <- sapply(q$subQuestions, parse_sqs, qid = qid)
    }
 
  }

  ## Parse choices function
  parse_cs <- function(c, qid) {
    c_row <- data.frame(
        qid = qid,
        order = cs_order,
        text = c$choiceText,
        desc = c$description,
        image_desc = c$imageDescription,
        recode = as.integer(c$recode)
    )

    choices_df <<- rbind(choices_df, c_row)
    cs_order <<- cs_order + 1
  }

  ## Parse subquestions function
  parse_sqs <- function(sq, qid) {    
    sq_row <- data.frame(
        qid = qid,
        order = sqs_order,
        text = sq$choiceText,
        desc = sq$description,
        image_desc = sq$imageDescription,
        recode = as.integer(sq$recode),
        variable_name = sq$variableName
    )

    subqs_df <<- rbind(subqs_df, sq_row)
    sqs_order <<- sqs_order + 1
  }

  ## Empty DF for choices & subquestions to be populated
  qs_df <- data.frame()
  choices_df <- data.frame()
  subqs_df <- data.frame()

  ## Run the recursive function parsing all the JSON data in to a DF
  s_qids <- names(s_meta$questions)
  qs_parse <- sapply(s_qids, parse_qs)

  ## Define survey class
  survey <- list(
      questions = qs_df,
      choices = choices_df,
      subquestions = subqs_df,
      responses = s_resp
  )

  class(survey) <- c("qsurvey", class(survey))
  return(survey)
}
