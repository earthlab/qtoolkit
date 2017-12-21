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

#' get_surveys
#'
#' Select surveys available, optionally matching survey name or ID.
#'
#' @param filter Complete or partial name of survey
#' @param match.exact Match exact survey name or partial
#'
#' @return DF of matched surveys
#' @export

get_surveys <- function(filter = "",
                        match.exact = FALSE) {
  
  # Detect filter type by filter string
  # If filter is an ID perform an exact match
  filter.prop <- if (grepl("^SV_.+", filter)) "id" else "name"
  match.exact <- if (filter.prop == "id") TRUE else match.exact

  # Get all surveys
  all_surveys <- qsurvey::surveys()

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

#' load_survey
#'
#' Load all metadata of specific survey(s) from API and return
#' survey design object
#'
#' @param filter DF of surveys with survey ids and names, or survey name
#' @param match.exact Match exact survey name or partial
#' @param verbose User will see which surveys are being loaded
#'
#' @return If one match, return design object of survey. If many, return list of design objects
#' @export

load_survey <- function(filter = "",
                        match.exact = TRUE,
                        verbose = TRUE) {

  # Handle different possible passed filters and validate
  if ( is.character(filter) ) {
    surveys <- get_surveys(filter,
                           match.exact = match.exact)
  }
  else if ( is.data.frame(filter) ) {
    surveys <- filter
    survey_cols <- names(surveys)

    # Error if passed DF has insufficient info
    if (!("id" %in% survey_cols) || !("name" %in% survey_cols)) {
      stop("`surveys` must contain 'id' and 'name' columns")
    }

  } else {
    stop("filter is not a valid dataframe or string")
  }
  
  # How many surveys we are loading
  num_matched <- dim(surveys)[1]
  
  # Load design objects of each survey into list
  designs_vector <- vector(mode = "list",
                           length = num_matched)
  
  for ( i in 1:num_matched ) {
    
    # Let user know of match if verbose
    if (verbose) {
      output <- paste0("Loading: ", surveys[i,]$name, "\n")
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
