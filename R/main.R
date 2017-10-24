#' get_responses
#'
#' Get responses for a particular survey question
#'
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @export
#' 
#' @param survey Survey Object (or ID)
#' @param question Question Number
#' @param metadata Include metadata about each participant in DF
#'
#' @return DF of survey question answers

get_responses <- function(survey,
                          question = "Q",
                          metadata = FALSE) {

  ## Check if survey object or survey ID is passed
  if (!is.character(survey)) {
    survey <- survey$id
  }

  ## Get survey responses from Qualtrics API
  resp <- qsurvey::responses(survey)  
  q_resp <- select(resp, starts_with("ResponseID"),
                   starts_with(question))

  ## Check if any responses returned and if
  ## any of the columns match the relevant question
  if (nrow(q_resp) == 0) {
    stop("No responses returned")
  } else if (length(q_resp) < 2) {
    stop("No questions match '", question, "'")
  }

  ## Append metadata to the response if specified
  if (metadata == TRUE) {
    q_metadata <- select(resp, -starts_with("Q"))
    q <- merge(q_metadata, q_resp)
  } else {
    q <- select(q_resp, starts_with(question))
  }

  return(q)
}

#' get_questions
#'
#' Get info about questions asked in a particular survey
#'
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @export
#' 
#' @param survey Survey Design Object
#' @param question Question Number
#'
#' @return DF of matching questions

get_questions <- function(survey,
                          question = "Q") {

  ## Get survey questions from Qualtrics API
  qs <- qsurvey::questions(survey)
  q_resp <- select(qs, starts_with("export_name"),
                   starts_with("question_text"))

  ## Check if question has been matched or no
  if (length(q_resp) < 2) {
    stop("No questions match '", question, "'")
  }

  ## Rename column question_text to question_title
  ## Add column question_text as stripped HTML from question_title
  q_resp$question_title <- q_resp$question_text
  q_resp$question_text <- strip_html(q_resp$question_text)

  return(q_resp)
}
