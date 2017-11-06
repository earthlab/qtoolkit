#' connect_qualtrics
#'
#' Open a connection to Qualtrics API with login info
#'
#' @param file Filename of file with connection information
#' @param subdomain Qualtrics subdomain with which to get surveys
#' @param key Qualtrics API key for API access
#' @export

connect_qualtrics <- function(subdomain,
                              file = "~/.qualtrics_api_key",
                              key = FALSE) {
  
  # Try to load key from file if not specified
  if (key == FALSE) {
    if (file.exists(file)) {
      f <- readLines(file)
      
      if (length(f) && nchar(f[1])) {
        key <- f[1]
      } else {
        stop("Couldn't load key from ", file)
      }
    } else {
      stop("Couldn't find ", file)
    }
  }

  # Check if subdomain was specified
  if (missing(subdomain)) {
    stop("Must specify subdomain")
  }

  # Set env variables so that qsurvey() can use credentials
  Sys.setenv("QUALTRICS_SUBDOMAIN" = subdomain)
  Sys.setenv("QUALTRICS_KEY" = key)

  # Define error fn
  error_fn <- function(e) {
    if (grepl("HTTP 401", e)) {
      stop("HTTP 401 Unauthorized. Check credentials and try again.",
           call. = FALSE)
    } else {
      stop(e)
    }
  }

  # Test surveys() call
  test <- withCallingHandlers({ qsurvey::surveys() }, error = error_fn)
}

#' get_survey
#'
#' Select surveys available, optionally filtering by name
#'
#' @param name Complete or partial name of survey
#' @param id Survey ID if known
#' @param match.exact Match exact name of survey or use wildcards
#'
#' @return DF of matched surveys
#' @export

get_survey <- function(name = "",
                       id = "",
                       match.exact = TRUE) {

  # Whether to filter by ID or name depending what's passed
  filters <- list(name = name,
                  id = id)

  filter <- if (id != "") "id" else "name"
  
  # Get all surveys
  all_surveys <- qsurvey::surveys()

  # Build regex to match exact or not
  if (exact == TRUE) {
    regex <- paste("^", filters[[filter]], "$", sep = "")
  } else {
    regex <- paste(".*", filters[[filter]], ".*", sep = "")
  }
    
  # Get matches from all surveys, and sort by name
  survey_matches <- all_surveys[grep(regex, all_surveys[[filter]]),]
  survey_matches <- survey_matches[order(survey_matches$name),]

  # Check if any surveys were returned
  if (dim(survey_matches)[1] == 0) {
    stop("No surveys matched with ", filter, " '", filters[[filter]], "'")
  }
  
  return(survey_matches)
}

#' load_survey
#'
#' Select surveys with a fragment of the survey's name
#'
#' @param surveys DF of surveys with survey ids and names, or survey name
#' @param verbose Let user know full name & surveyID of selected survey
#' @param match.exact Match exact survey name or use wildcards
#'
#' @return If one match, return design object of survey. If many, return list of design objects
#' @export

load_survey <- function(surveys = "",
                        verbose = TRUE,
                        match.exact = TRUE) {

  # Check if user is specifying survey name or DF
  if (is.character(surveys)) {
    surveys <- get_survey(surveys,
                          match.exact = match.exact)
  } else if (!is.data.frame(surveys)) {
    stop("`surveys` must be a data frame or name")
  }

  # Check inputs
  survey_cols <- names(surveys)

  if (!("id" %in% survey_cols) || !("name" %in% survey_cols)) {
    stop("`surveys` must contain 'id' and 'name' columns")
  }
  
  # How many surveys we are loading
  num_matched <- dim(surveys)[1]
  
  # Load design objects of each survey into list
  designs_vector <- vector(mode = "list",
                           length = num_matched)
  
  for ( i in 1:num_matched ) {
    
    # Let user know of match if verbose
    if (verbose) {
      output <- paste("Loading: ", surveys[i,]$name, "\n", sep = "")
      cat(output)
    }

    # Get survey design from API
    designs_vector[[i]] <- qsurvey::design(surveys[i,]$id)

    # Check if survey has duplicate question number
    check_duplicate_question(designs_vector[[i]])
  }

  # Return one survey design if only one match
  # If multiple matches, return a list
  if (num_matched == 1) {
    return(designs_vector[[1]])
  } else {
    return(designs_vector)
  }
  
}
