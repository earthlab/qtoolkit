#' qsurvey
#'
#' Qualtrics survey object
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows
#'
#' @param id_or_name Survey ID or name
#' @param strip.html Auto-strip HTML from question names & choices
#'
#' @return Qualtrics survey object
#' @export

qsurvey <- function(id_or_name,
                    strip.html = TRUE) {

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

  ## Initialize lists to be populated with one-row dataframes of
  ## question, choice, subquestion metadata
  s_questions <- list()
  s_choices <- list()
  s_subquestions <- list()

  ## Loop thru all returned questions
  ## (must use seq_along to keep track of order)
  qs <- s_meta$questions
  qids <- names(qs)

  for(i in seq_along(qids)) {
    qid <- qids[[i]]
    q <- qs[[qid]]

    ## Parse choices, if they exist
    if (!is.null(q$choices)) {
      cs <- q$choices
      cids <- names(cs)

      for (j in seq_along(cids)) {
        cid <- cids[[j]]
        c <- cs[[cid]]

        ## Filter out empty lists because they break everything
        c <- Filter(lengths, c)
        c_df <- as.data.frame(c)

        ## Add qid and order to choices dataframe
        c_df$qid <- qid
        c_df$order <- as.integer(j)

        s_choices <- c(list(c_df), s_choices)
      }   
    }

    ## Parse subquestions, if they exist
    if (!is.null(q$subQuestions)) {
      sqs <- q$subQuestions
      sqids <- names(sqs)

      for (k in seq_along(sqids)) {
        sqid <- sqids[[k]]
        sq <- sqs[[sqid]]

        ## Filter out empty lists because they break everything
        sq <- Filter(lengths, sq)
        sq_df <- as.data.frame(sq)

        ## Add qid and order to subquestions dataframe
        sq_df$qid <- qid
        sq_df$order <- as.integer(k)

        s_subquestions <- c(list(sq_df), s_subquestions)
      }
    }

    ## Remove choices & subquestions data so we can parse rest of
    ## questions metadata
    q$choices <- NULL
    q$subQuestions <- NULL

    ## Flatten nested list, turn into dataframe and append to list of
    ## dataframe of questions
    q_unlist <- unlist(q, recursive = FALSE)
    q_df <- as.data.frame(q_unlist)

    q_df$qid <- qid
    q_df$order <- as.integer(i)
    
    s_questions <- c(s_questions, list(q_df))
  }

  ## Concatenate the lists of DFs to one big ole' DF
  s_questions <- bind_rows(s_questions)
  s_choices <- bind_rows(s_choices)
  s_subquestions <- bind_rows(s_subquestions)

  ## Compile survey object and rename columns in the process
  ## 
  survey <- list(
      questions = auto_reformat(s_questions, "", strip.html),
      choices = auto_reformat(s_choices, "", strip.html),
      subquestions = auto_reformat(s_subquestions, "", strip.html),
      responses = s_resp
  )

  ## Class definition and return
  class(survey) <- "qsurvey"
  return(survey)
}

#' qquestion
#'
#' Qualtrics Survey Question object
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter_
#'
#' @param survey Qualtrics Survey Object
#' @param qid_or_name Question ID or name
#'
#' @return Qualtrics Survey Question object
#' @export

qquestion <- function(survey, qid_or_name) {

  ## User input validation
  assert_that(is.obj.type(survey, "qsurvey"))

  ## Determine what attribute to filter by
  filter <- if(grepl("^QID.+", qid_or_name)) "qid" else "name"

  ## Select row with particular filter
  filter_logic <- paste0(filter, "==", shQuote(qid_or_name))
  q_row <- filter_(survey$questions, filter_logic)

  ## Test if there's a question returned
  if (dim(q_row)[1] != 1) {
    stop("Cannot find question with ", filter_logic)
  }

  ## Build qquestion object
  q_qid <- q_row$qid

  ## Get choices and subqs if they exist
  choices <- empty_to_na(filter(survey$choices, qid == q_qid))
  subqs <- empty_to_na(filter(survey$subquestions, qid == q_qid))

  ## Build question object
  question <- as.list(q_row)
  
  class(question) <- "qquestion"
  
  return(question)
}
  
#' surveys
#'
#' @importFrom assertthat assert_that
#'
#' @param filter String filter to match surveys by ID / name, or a dataframe with survey IDs in `id` column
#'
#' @return qToolkit survey collection object

qsurveys <- function(filter) {
  
  ## User input validation
  assert_that(!missing(filter))

  ## Get list of survey IDs depending on what kind of filter is passed
  if (!is.character(filter) | !grepl("^SV_.+", filter)) {
    survey_ids <- find_surveys(filter, match.exact)
  } else if (is.data.frame(filter)) {
    survey_ids <- filter$id
  } else {
    survey_ids <- c(filter)
  }

  ## Loop thru returned survey IDs and create survey objects for each
  num_surveys <- length(survey_ids)
  surveys_vec <- vector(mode = "list",
                        length = num_surveys)

  for (i in 1:num_surveys) {
    surveys_vec[[i]] <- survey()

  }

}
