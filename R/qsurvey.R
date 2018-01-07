#' qsurvey
#'
#' Qualtrics survey object
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.string
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#'
#' @param id_or_name Survey ID or name
#' @param strip.html Auto-strip HTML from question names & choices
#' @param include.raw Include raw JSON in survey object
#'
#' @return Qualtrics survey object
#' @export

qsurvey <- function(id_or_name,
                    strip.html = TRUE,
                    include.raw = FALSE) {

  assert_that(is.string(id_or_name))

  ## Get a survey by its ID or name, depending on filter passed
  if (grepl("^SV_.+", id_or_name)) {
    survey_id <- id_or_name
  } else {
    survey_id <- get_survey_id_by_name(id_or_name,
                                       match.exact = TRUE)
  }
  
  ## Get survey metadata and responses from Qualtrics API
  s_meta <- qapi_get_survey(survey_id)
  s_resp <- qapi_response_export(survey_id)
  
  ## Initialize questionList and qquestionsb
  s_qquestions <- list()
  
  ## Loop thru all returned questions (use seq_along to keep track of order)
  qs <- s_meta$questions
  qids <- names(qs)

  for (i in seq_along(qids)) {
    ## Get question ID and question object
    qid <- qids[[i]]
    q_meta <- qs[[qid]]

    ## Add qid and order to question metadata
    q_meta$questionOrder <- as.integer(i)
    q_meta$questionID <- qid

    ## Select the responses (and ResponseID) for only this question,
    ## pass that to create new qquestion object along with metadata
    q_resp <- select(s_resp, "ResponseID", starts_with(q_meta$questionName))
    q_qquestion <- qquestion(q_meta, q_resp)

    ## Add qquestion to list of qquestions
    ##s_qquestions <- c(list(s_qquestions), q_qquestion)
    s_qquestions[[qid]] <- q_qquestion
  }

  ## Generate question list from qquestion objects
  s_question_list <- lapply(s_qquestions,
                            function(q) {
                              return(as.data.frame(q[c(1:9)]))
                            })
  s_question_list <- bind_rows(question_list)
  s_question_list <- auto_reformat(question_list)

  ## Generate DF of survey blocks
  s_blocks <- data.frame()
  
  bs <- s_meta$blocks
  bids <- names(bs)

  for (j in seq_along(bids)) {
    bid <- bids[[j]]
    b_meta <- bs[[bid]]

    b <- nested_list_to_df(b_meta$elements)
    
    b <- cbind(b_order = j,
               b_id = bid,
               b_desc = b_meta$description,
               b)

    s_blocks <- bind_rows(s_blocks, b)
  }

  s_blocks <- auto_reformat(s_blocks, reorder.cols = FALSE)
    
  ## Generate list of respondents
  respondent_cols <- names(sv$responses)[c(1:11)]
  s_respondents <- sv$responses[,respondent_cols]
  
  ## Class definition and return
  survey <- list(
      meta = list(
          id = s_meta$id,
          name = s_meta$name,
          owner_id = s_meta$ownerId,
          organization_id = s_meta$organizationId,
          is_active = s_meta$isActive,
          created = s_meta$creationDate,
          last_modified = s_meta$lastModifiedDate,
          expiration = list(
              start = s_meta$expiration$startDate,
              end = s_meta$expiration$endDate
          )
      ),
      questionList = s_question_list,
      questions = s_qquestions,
      respondents = s_respondents,
      responses = s_resp,
      blocks = s_blocks
  )

  ## Add raw JSON if specified
  if (include.raw) {
    survey$raw = s_meta
  }

  class(survey) <- "qsurvey"
  return(survey)
}

print.qsurvey <- function(x, ...) {
  c(sprintf("%s (%s)", s$meta$name, s$meta$id),
    sprintf("  $questions:")) 
}
