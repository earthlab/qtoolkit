#' qapi_base_url
#'
#' Return a Qualtrics API base URL based upon subdomain
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.string
#' 
#' @param subdomain Qualtrics subdomain
#'
#' @return String of Qualtrics API base URL
#' @export

qapi_get_base_url <- function(subdomain) {
  assert_that(is.string(subdomain))
  base_url <- paste0("https://", subdomain, ".qualtrics.com/API/v3/")

  return(base_url)
}

#' qapi_connect
#'
#' Open a connection to Qualtrics API with login info
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.string
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

      options(QAPI_AUTH = qapi_auth)
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
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.string
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
    cat("Connection successful! (subdomain='", subdomain, "')\n",
        sep = "")
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
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.string
#'
#' @param verb Request type (GET, POST, ...)
#' @param method API call method (surveys, reponseexports, ...) or full API URL
#' @param data Named list with request payload data
#' @param content.as "text" or "raw" depending on if JSON or raw data returned
#' @param auth Qualtrics API authentication to use
#' @param all.results Return all results if paginated, or just one page
#' 
#' @return Named list of JSON decoded response content
#' @export

qapi_request <- function(verb,
                         method,
                         data = list(),
                         content.as = "text",
                         auth = NULL,
                         all.results = TRUE) {
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

  ## Set up & send API Request
  qapi_dat <- RJSONIO::toJSON(data, asIs = FALSE)
  qapi_hdr <- httr::add_headers(`X-API-TOKEN` = auth$api_key,
                                `User-Agent` = "qtoolkit",
                                `Content-type` = "application/json")

  httr_req <- getFromNamespace(verb, "httr")
  qapi_req <- httr_req(qapi_url, qapi_hdr, body = qapi_dat)

  ## Check for response errors.
  if (httr::http_error(qapi_req)) {
    qapi_error(qapi_req)
  }

  ## Parse (or not) response content
  qapi_resp <- httr::content(qapi_req, as = content.as,
                             encoding = "UTF-8")

  if (content.as == "raw") {
    return(qapi_resp)
  } else {
    qapi_resp <- RJSONIO::fromJSON(qapi_resp, nullValue = NA,
                                   simplifyWithNames = FALSE)
  }
  
  ## If list is paginated, request more if chosen
  if (!is.na(qapi_resp$result$nextPage) & all.results) {
    new_resp <- qapi_request(verb, qapi_resp$result$nextPage, data,
                             auth = auth, all.results = all.results)

    qapi_resp$result$elements <- c(qapi_resp$result$elements,
                                   new_resp$result$elements)
    qapi_resp$result$nextPage <- NULL
  }

  return(qapi_resp)
}

#' qapi_error
#'
#' Handle errors caused by Qualtrics API request
#'
#' @param request httr request object of the Qualtrics API request

qapi_error <- function(request) {
  req_hdrs <- httr::headers(request)

  if (httr::http_type(request) == "application/json") {
    resp_raw <- httr::content(request, "text", encoding = "UTF-8")
    resp_json <- RJSONIO::fromJSON(resp_raw, nullValue = NA,
                                   simplifyWithNames = FALSE)
    
    if (!is.null(resp_json$meta$httpStatus)) {
      err_status <- resp_json$meta$httpStatus
      err_msg <- resp_json$meta$error$errorMessage

      stop("QAPI Error (", err_status, "): ", err_msg)
    }
  }
  
  stop("HTTP Error: ", httr::http_status(request)$message)
}

#' qapi_response_export
#'
#' Get DF of survey responses from Qualtrics API
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.string
#' 
#' @param survey_id ID of survey to get responses
#'
#' @return DF of survey responses
#' @export

qapi_response_export <- function(survey_id) {

  ## Input Validation
  assert_that(is.string(survey_id))

  ## Send request to start survey response export
  create_data <- list(surveyId = survey_id,
                      format = "csv")
  
  create_resp <- qapi_request("POST",
                              "responseexports",
                              create_data)

  create_id <- create_resp$result$id

  ## Keep pinging Qualtrics to see if export is complete
  while (TRUE) {
    check_resp <- qapi_request("GET",
                               paste0("responseexports/", create_id))

    check_status <- check_resp$result$status

    switch(check_status,
           "complete" = {break},
           "in progress" = {Sys.sleep(0.2)},
           {
             err_info <- check_resp$result$info
             
             stop("Response Export Error: ",
                  err_info$reason, " ", err_info$nextStep)
           })
  }
  
  ## Once export is complete, download export zip file, read its
  ## contents and then load the csv data from inside
  dl_resp <- qapi_request("GET",
                          check_resp$result$file,
                          content.as = "raw")

  ## Download and write temp file
  zip_file <- tempfile()
  writeBin(dl_resp, zip_file)

  ## Get list of inside zip file, and select the csv file's name
  csv_file <- unzip(zip_file, list = TRUE)[1, "Name"]

  ## Get col names from csv file
  csv_colnames <- read.csv(unz(zip_file, csv_file), header = TRUE,
                     quote="\"", sep=",")
  csv_colnames <- names(csv_colnames)

  ## Get csv data and append col names. This is so the DF will have all
  ## cols be the correct data type as in the csv the first two rows
  ## of each column are a string which will throw off the parser
  csv_df <- read.csv(unz(zip_file, csv_file), header = TRUE,
                     quote="\"", sep=",", skip = 2)
  names(csv_df) <- csv_colnames

  ## Skip first two lines of DF; we don't need 'em!
  return(csv_df)
}

#' qapi_list_surveys
#'
#' QAPI call to list all surveys that a user owns
#'
#' @return DF of surveys
#' @export

qapi_list_surveys <- function() {
  list_resp <- qapi_request("GET",
                            "surveys")

  list_df <- do.call(rbind.data.frame, list_resp$result$elements)

  return(list_df)
}

#' qapi_get_survey
#'
#' QAPI call to get metadata about a particular survey1
#'
#' @param survey_id
#'
#' @return Named list of metadata
#' @export

qapi_get_survey <- function(survey_id) {
  get_resp <- qapi_request("GET",
                          paste0("surveys/", survey_id))

  ## TODO more user friendly error if survey ID is invalid
  
  return(get_resp$result)
}
