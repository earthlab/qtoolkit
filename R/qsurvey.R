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
  
  ## Initialize and qquestions list
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
   
    # if it's a matrix question, grab questions that have _
    # if not, then grab questions that equal the question name
    # note this is problematic for duplicates
    
   if (q_meta$questionType$type == "Matrix" | q_meta$questionType$type == "MC") {
     q_resp <- select(s_resp, "ResponseID", starts_with(paste0(q_meta$questionName, "_")))
   } else {
     q_resp <- select(s_resp, "ResponseID", q_meta$questionName)
   }
    
    #all_cols <- colnames(q_qquestion$responses)
    q_qquestion <- qquestion(q_meta, q_resp)
    # because the first column is the ID and we want to 
    # this will return both q4 and q42
    # this should explicetely tell it what columns to grab as associate with the qid avoiding duplicate issues
    #q_resp <- select(s_resp, "ResponseID", (all_cols[2:length(all_cols)]))

    ## Add qquestion to list of qquestions
    s_qquestions[[qid]] <- q_qquestion
  }

  ## Generate question list from qquestion objects
  s_question_list <- lapply(s_qquestions,
                            function(q) { return(q$meta) })
  s_question_list <- bind_rows(s_question_list)
  s_question_list <- auto_reformat(s_question_list)

  ## Parse survey "flow"
  s_flow <- nested_list_to_df(s_meta$flow)
  s_flow <- auto_reformat(s_flow, "b")
  s_flow <- cbind(s_flow, desc = NA)
  names(s_flow)[names(s_flow) == "id"] <- "bid" ## Change block 'id' to 'bid'
  
  ## Generate DF of survey blocks
  s_blocks <- data.frame()
  
  bs <- s_meta$blocks
  bids <- names(bs)
  # Added line to remove empty blocks
  for (j in seq_along(bids)) {
    bid <- bids[[j]]
    b_meta <- bs[[bid]]

    b <- nested_list_to_df(b_meta$elements)
    b <- cbind(bid = bid, b)

    ## Add the block description to the survey flow DF row with bid
    # but first, remove block rows where bid == NA
    # note that this is a quirk in qualtrics as i can see the empty blocks in the flow 
    # but they are not in the survey. Digging into why this happens
    s_flow <- s_flow[complete.cases(s_flow$bid), ]
    s_flow[s_flow$bid == bid,]$desc <- b_meta$description

    s_blocks <- bind_rows(s_blocks, b)
  }

  s_blocks <- auto_reformat(s_blocks, reorder.cols = FALSE)
    
  ## Generate list of respondents
  s_respondent_cols <- names(s_resp)[c(1:11)]
  s_respondents <- s_resp[,s_respondent_cols]
  
  ## Class definition and return
  survey <- list(
      meta = list(
          id              = s_meta$id,
          name            = s_meta$name,
          owner_id        = s_meta$ownerId,
          organization_id = s_meta$organizationId,
          is_active       = s_meta$isActive,
          created         = s_meta$creationDate,
          last_modified   = s_meta$lastModifiedDate,
          expiration      = list(
              start = s_meta$expiration$startDate,
              end   = s_meta$expiration$endDate
          )
      ),
      questionList = auto_reformat(s_question_list), ## TODO:: why must this be done twice ??
      questions    = s_qquestions,
      respondents  = s_respondents,
      responses    = s_resp,
      flow         = s_flow,
      blocks       = s_blocks
  )

  ## Check for duplicate questions
  check_duplicate_questions(survey)

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
