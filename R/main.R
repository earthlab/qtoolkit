#' get_survey_id_by_name
#'
#' Get a survey ID by name
#'
#' @importFrom assertthat assert_that
#' 
#' @param name Survey name
#' @param match.exact Search for exact name? (or partial)
#' @param surveys_df Dataframe of available surveys. If NULL, will load search surveys loaded from Qualtrics API
#'
#' @return Survey ID

get_survey_id_by_name <- function(name,
                                  match.exact = TRUE,
                                  surveys_df = NULL) {
  ## Input validation
  assert_that(!missing(name))
  cat(name)

  ## If the survey DF to search is passed, use that. Or, get survey list
  if (is.null(surveys_df)) {
    surveys_df <- qapi_list_surveys()
  }

  ## Build regex to match name
  if (match.exact == TRUE) {
    regex <- paste0("^", name, "$")
  } else {
    regex <- paste0(".*", name, ".*")
  }

  ## Find matched surveys & error if none are matched
  matches <- surveys_df[grep(regex, surveys_df[["name"]]),]

  if (dim(matches)[1] == 0) {
    msg.txt <- if (match.exact) "exact " else "partial "
    stop("No surveys matched with ", msg.txt, "name '", name, "'")
  }

  ## Order matches by name and return ids
  matches <- matches[order(matches$name),]
  return(matches$id)
}

#' list_surveys
#'
#' Select surveys available, optionally matching survey name or ID.
#'
#' @param filter Complete or partial name of survey
#' @param match.exact Match exact survey name or partial
#' @param id.only Return DF of survey results or just vector of their IDs
#'
#' @return DF of matched surveys
#' @export

list_surveys <- function(filter = "",
                         match.exact = FALSE,
                         id.only = FALSE) {
  
  ## Detect filter type by filter string
  ## If filter is an ID perform an exact match
  filter.prop <- if (grepl("^SV_.+", filter)) "id" else "name"
  match.exact <- if (filter.prop == "id") TRUE else match.exact

  ## Get all surveys
  all_surveys <- qapi_list_surveys()

  ## Build regex to match exact or not
  if (match.exact == TRUE) {
    regex <- paste0("^", filter, "$")
  } else {
    regex <- paste0(".*", filter, ".*")
  }
    
  ## Get matches from all surveys, and sort by name
  survey_matches <- all_surveys[grep(regex, all_surveys[[filter.prop]]),]
  survey_matches <- survey_matches[order(survey_matches$name),]
  rownames(survey_matches) <- NULL

  ## Check if any surveys were returned
  if (dim(survey_matches)[1] == 0) {
    msg.txt <- if (match.exact) "exact " else "partial "
    stop("No surveys matched with ", msg.txt, filter.prop,
         " '", filter, "'")
  }

  ## Return id only if specified; otherwise return the DF
  if (id.only) {
    return(survey_matches$id)
  } else {
    return(survey_matches)
  }
}
