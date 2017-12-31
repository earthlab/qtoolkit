#' list_surveys
#'
#' Select surveys available, optionally matching survey name or ID.
#'
#' @param filter Complete or partial name of survey
#' @param match.exact Match exact survey name or partial
#'
#' @return DF of matched surveys
#' @export

list_surveys <- function(filter = "",
                         match.exact = FALSE) {
  
  # Detect filter type by filter string
  # If filter is an ID perform an exact match
  filter.prop <- if (grepl("^SV_.+", filter)) "id" else "name"
  match.exact <- if (filter.prop == "id") TRUE else match.exact

  # Get all surveys
  all_surveys <- qapi_list_surveys()

  # Build regex to match exact or not
  if (match.exact == TRUE) {
    regex <- paste0("^", filter, "$")
  } else {
    regex <- paste0(".*", filter, ".*")
  }
    
  # Get matches from all surveys, and sort by name
  survey_matches <- all_surveys[grep(regex, all_surveys[[filter.prop]]),]
  survey_matches <- survey_matches[order(survey_matches$name),]

  # Check if any surveys were returned
  if (dim(survey_matches)[1] == 0) {
    msg.txt <- if (match.exact) "exact " else "partial "
    stop("No surveys matched with ", msg.txt, filter.prop,
         " '", filter, "'")
  }
  
  return(survey_matches)
}

#' get_survey
#'
#' Get all metadata of specific survey(s) from API and return
#' survey design object
#'
#' @param filter DF of surveys with survey ids and names, or survey name
#' @param match.exact Match exact survey name or partial
#' @param verbose User will see which surveys are being loaded
#'
#' @return If one match, return design object of survey. If many, return list of design objects
#' @export

get_survey <- function(filter = "",
                       match.exact = TRUE,
                       verbose = TRUE) {

  # Handle different possible passed filters and validate
  if ( is.character(filter) ) {
    surveys <- list_surveys(filter,
                            match.exact = match.exact)
  }
  else if ( is.data.frame(filter) ) {
    surveys <- filter
    survey_cols <- names(surveys)

    # Error if passed DF has insufficient info
    if (!("id" %in% survey_cols) || !("name" %in% survey_cols)) {
      stop("`filter` must contain 'id' and 'name' columns")
    }

  } else {
    stop("filter is not a valid dataframe or string")
  }
  
  # How many surveys we are loading
  num_matched <- dim(surveys)[1]
  
  # Load design objects of each survey into list
  surveys_vec <- vector(mode = "list",
                        length = num_matched)
  
  for ( i in 1:num_matched ) {
    
    # Let user know of match if verbose
    if (verbose) {
      output <- paste0("Loading: ", surveys[i,]$name, "\n")
      cat(output)
    }

    # Get survey design from API
    surveys_vec[[i]] <- qapi_get_survey(surveys[i,]$id)

    # Check if survey has duplicate question number
    check_duplicate_question(surveys_vec[[i]])
  }

  # Return one survey design if only one match
  # If multiple matches, return a list
  if (num_matched == 1) {
    return(surveys_vec[[1]])
  } else {
    return(surveys_vec)
  }
}

#' get_responses
#'
#' Get responses for a particular survey question
#'
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom dplyr matches
#' @export
#' 
#' @param survey Survey Object
#' @param question_num Question Number or Vector of Question Numbers
#' @param metadata Include metadata about each participant in DF
#' @param melt Melt data from a DF into key=>value DF
#' @param rm.other Remove "Other" responses from MC questions
#' @param rm.blank Remove Blank responses from MC/matrix questions
#' 
#' @return DF of survey question answers

get_responses <- function(survey,
                          question_num = ".*",
                          metadata = TRUE,
                          melt = FALSE,
                          rm.other = TRUE,
                          rm.blank = TRUE) {

  ## Get survey responses from Qualtrics API
  question_num <- paste(question_num, collapse = "|")
  num_filter <- paste0("^(", question_num, ")(\\_|$)")
  
  q_resp <- select(resp, starts_with("ResponseID"),
                   matches(num_filter))

  ## Check if any responses returned and if
  ## any of the columns match the relevant question
  if (nrow(q_resp) == 0) {
    stop("No responses returned")
  } else if (length(q_resp) < 2) {
    stop("No questions match '", question_num, "'")
  }

  ## Append metadata to the response if specified
  if (metadata == TRUE) {
    q_metadata <- select(resp, -starts_with("Q"))
    q_resp<- merge(q_metadata, q_resp)
  } else {
    q_resp<- select(q_resp, matches(num_filter))
  }

  ## Melt the data if told so
  if (melt == TRUE) {
    q_resp <- tidyr::gather(q_resp)

    if (rm.other == TRUE) {
      q_resp <- filter(q_resp, !str_detect(value, "Other"),
                       !grepl("_TEXT$", key))
    }

    if (rm.blank == FALSE) {
      q_resp <- filter(q_resp, !(value == ""))
    }
  }    

  return(q_resp)
}

#' get_questions
#'
#' Get info about question(s) asked in a particular survey
#'
#' @importFrom dplyr filter
#' @importFrom tidyr gather
#' @export
#' 
#' @param survey Survey Design Object
#' @param question_num Question number (ie "Q10")
#' @param question_id Question ID (ie "QID9")
#'
#' @return DF of matching questions

get_questions <- function(survey,
                          question_num = ".*",
                          question_id = "QID.*") {

  ## Get survey questions from qsurvey API
  qs <- qsurvey::questions(survey)

  ## Filter questions by names passed
  num_filter <- paste0("^", question_num, "(\\_|$)")
  id_filter <- paste0("^", question_id, "(\\_|$)")
  
  q_resp <- filter(qs, grepl(num_filter, export_name),
                   grepl(id_filter, question_id))

  ## Check if question has been matched or no
  if (nrow(q_resp) == 0) {
    stop("No questions match question_num~='", question_num,
         "' and question_id~='", question_id, "'")
  }

  ## Rename column question_text to question_title
  ## Add column question_text as stripped HTML from question_title
  q_resp$question_raw <- q_resp$question_text
  q_resp$question_text <- strip_html(q_resp$question_raw)

  ## Add question number column (turns Q9_1 into Q9)
  q_resp$question_num <- sub("^(.*?)(\\_|#|$).*", "\\1",
                             q_resp$export_name)

  return(q_resp)
}


#' get_choices
#'
#' Get possible choices for a particular question
#'
#' @importFrom dplyr filter
#' @export
#'
#' @param survey Survey Design Object
#' @param question_num Question number (ie "Q10")
#' @param question_id Question ID (ie "QID9")
#'
#' @return DF of choices for each question
#'
#' @export

get_choices <- function(survey,
                        question_num = ".*",
                        question_id = "QID.*") {

  ## Get choices from qsurvey API
  cs <- qsurvey::choices(survey)

  ## Get DF question_id => question_num map & join to choices
  qs_map <- data.frame(question_id = names(survey$questionMap),
                       question_num = unlist(survey$questionMap),
                       row.names = NULL)
  cs <- merge(qs_map, cs)

  ## Filter choices by question num/id
  num_filter <- paste("^", question_num, "(\\_|$)",  sep="")
  id_filter <- paste("^", question_id, "(\\_|$)", sep="")

  c_resp <- filter(cs, grepl(id_filter, question_id),
                   grepl(num_filter, question_num))

  ## Return just a vector of choices if question num or id is specified
  ## Choices are returned in order they're displayed on survey
  if (question_num != "Q.*" || question_id != "QID.*") {
    c_resp <- unique(c_resp[order(c_resp$choice_id),]$choice_text)
  }

  return(c_resp)
}
