#' qapi_base_url
#'
#' Return a Qualtrics API base URL based upon subdomain
#'
#' @importFrom assert_that assert_that
#' 
#' @param subdomain Qualtrics subdomain
#'
#' @return String of Qualtrics API base URL

qapi_get_base_url <- function(subdomain) {
  assert_that(is.string(subdomain))
  base_url <- paste0("https://", subdomain, ".qualtrics.com/API/v3/")

  return(base_url)
}

#' qapi_connect
#'
#' Open a connection to Qualtrics API with login info
#'
#' @importFrom assert_that assert_that
#' 
#' @param subdomain Qualtrics subdomain with which to get surveys
#' @param key Qualtrics API key for API access
#' @param file File from which to load Qualtrics API Auth info
#' @export

qapi_connect <- function(subdomain,
                         key,
                         auth_file = ".qapi_auth.R") {
  
  ## If subdomain and key are provided, attempt to connect with that
  if (!missing(subdomain) & !missing(key)) {
    assert_that(is.string(subdomain))
    assert_that(is.string(key))

    if (qapi_test(subdomain, key)) {
      qapi_auth <- list(subdomain = subdomain,
                        api_key = key)

      options(`QAPI_AUTH` = qapi_auth)
    } else {
      cat("Connection successful!")
    }
  } else {
    ## If the auth_file exists, source it to get user-defined auth
    ## values stored in options(); if not, see if those values are set
    ## anyways (perhaps w/ .Rprofile) and attempt connection with those
    
    if (file.exists(auth_file)) source(auth_file)

    qapi_subd <- getOption("QAPI_SUBDOMAIN")
    qapi_key <- getOption("QAPI_KEY")

    if (!is.null(qapi_subd) & !is.null(qapi_key)) {
      qapi_connect(qapi_subd, qapi_key)
    } else {
      stop("No Qualtrics API authentication info found")
    }
  }
}

#' qapi_test
#'
#' Test Qualtrics API connection
#' 
#' @importFrom assert_that assert_that
#' 
#' @param subdomain Qualtrics subdomain to test
#' @param key Qualtrics API key to test
#' 
#' @return True if successful, or will error if not
#' @export

qapi_test <- function(subdomain,
                      key) {

  assert_that(is.string(subdomain))
  assert_that(is.string(key))

  test_auth <- list(subdomain = subdomain,
                    api_key = key)

  test_req <- qapi_request("GET", "surveys", auth = test_auth,
                           all.results = FALSE)

  if (!is.null(test_req) & !identical(test_req, FALSE)) {
    cat("Connection successful!\n")
    return(TRUE)
  }
}

#' qapi_get_auth
#'
#' Get the stored authentication parameters for Qualtrics API
#'
#' @return Named list of authentication parameters
#' @export

qapi_get_auth <- function() {
  
  auth_keys <- c("api_key", "subdomain")
  qapi_auth <- getOption("QAPI_AUTH")

  if (is.null(qapi_auth)) {
    stop("Qualtrics API authentication not stored in options()")
  }

  ## Test if all auth keys necessary exist
  for (key in auth_keys) {
    if (is.null(qapi_auth[[key]])) {
      stop("Qualtrics API authentication params don't include ", key)
    }
  }
  
  return(qapi_auth)
}


#' qapi_request
#'
#' Send request to Qualtrics API
#'
#' @importFrom assert_that assert_that
#'
#' @param verb Request type (GET, POST, ...)
#' @param method API call method (surveys, reponseexports, ...) or full API URL
#' @param data Named list with request payload data
#' @param all.results T or F, to return all results when paginated or just one page
#' 
#' @return Named list of JSON decoded response content
#' @export

qapi_request <- function(verb,
                         method,
                         data = list(),
                         all.results = TRUE,
                         auth = NULL) {

  ## Input Validation
  assert_that(is.string(verb))
  assert_that(is.string(method))
  assert_that(is.list(data))
  
  verb <- toupper(verb)

  if (is.null(auth)) {
    auth <- qapi_get_auth()
  }

  ## If method string has full address use that, otherwise build API URL
  if (grepl("^https*://", method)) {
    qapi_url <- method
  } else {
    qapi_url <- paste0(qapi_get_base_url(auth$subdomain), method)
  }

  ## Set up & send API Request, parse response
  qapi_dat <- jsonlite::toJSON(data)
  qapi_hdr <- httr::add_headers(`X-API-TOKEN` = auth$api_key,
                                `User-Agent` = "qtoolkit",
                                `Content-type` = "application/json")

  httr_req <- getFromNamespace(verb, "httr")
  qapi_req <- httr_req(qapi_url, qapi_hdr, query = data)

  qapi_raw <- httr::content(qapi_req, "text", encoding = "UTF-8")
  qapi_resp <- jsonlite::fromJSON(qapi_raw)

  ## Check for errors :: If we get back some JSON with error info,
  ## display that; if not, use the error info given to us by httr
  if (httr::http_error(qapi_req)) {
    
    if (!is.null(qapi_resp$meta$httpStatus)) {
      err_status <- qapi_resp$meta$httpStatus
      err_msg <- qapi_resp$meta$error$errorMessage

      stop("HTTP Error (", err_status, "): ", err_msg)
    } else {
      ## httr error info
    }
  }

  ## If list is paginated, request more if chosen
  if (!is.null(qapi_resp$result$nextPage)) {
    new_resp <- qapi_request(verb, qapi_resp$result$nextPage, data, all.results)

    qapi_resp$result$elements <- rbind(qapi_resp$result$elements,
                                       new_resp$result$elements)
    qapi_resp$result$nextPage <- NULL
  }

  return(qapi_resp)
}

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
