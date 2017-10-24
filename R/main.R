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

#' get_question
#'
#' Get info about question(s) asked in a particular survey
#'
#' @importFrom dplyr filter
#' @export
#' 
#' @param survey Survey Design Object
#' @param question_name Question name (ie "Q10")
#' @param question_id Question ID (ie "QID9")
#'
#' @return DF of matching questions

get_question <- function(survey,
                         question_name = "Q",
                         question_id = "QID") {

  ## Get survey questions from qsurvey API
  qs <- qsurvey::questions(survey)

  ## Filter questions by names passed
  name_filter <- paste("^", question_name, sep="")
  id_filter <- paste("^", question_id, sep="")
  
  q_resp <- filter(qs, grepl(name_filter, export_name),
                   grepl(id_filter, question_id))

  ## Check if question has been matched or no
  if (nrow(q_resp) == 0) {
    stop("No questions match question_name~='", question_name,
         "' and question_id~='", question_id, "'")
  }

  ## Rename column question_text to question_title
  ## Add column question_text as stripped HTML from question_title
  q_resp$question_raw <- q_resp$question_text
  q_resp$question_text <- strip_html(q_resp$question_raw)

  return(q_resp)
}
