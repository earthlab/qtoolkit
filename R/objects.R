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

qquestion <- function(meta,
                      resp,
                      strip.html = TRUE) {

  
  
}

qquestion.MC <- function(meta,
                         resp,
                         strip.html = TRUE) {

}

qquestion.Matrix <- function(meta,
                             resp,
                             strip.html = TRUE) {
  
}
 
  
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
  choices <- filter(survey$choices, qid == q_qid)
  subqs <- filter(survey$subquestions, qid == q_qid)

  ## Build question object
  question <- as.list(q_row)
  
  class(question) <- "qquestion"
  
  return(question)
}

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
  
  ## Initialize lists to be populated with subelement data
  survey <- list(
      questions = list(),
      choices = list(),
      subQuestions = list(),
      columns = list()
  )

  ## Sublists under each question to be parsed
  #parse_lists <- c("choices", "subQuestions", "columns")
  parse_lists <- c("columns")

  
  ## Loop thru all returned questions (use seq_along to keep track of order)
  qs <- s_meta$questions
  qids <- names(qs)

  for (i in seq_along(qids)) {
    ## Get question ID and question object
    qid <- qids[[i]]
    q <- qs[[qid]]

    for (list_name in parse_lists) {
      ## If question doesn't have particular list, skip it
      if (is.null(q[[list_name]])) next

      cat(list_name, qid, "\n")

      ## Get elements of list and their names
      es <- q[[list_name]]
      eids <- names(es)

      ## Initialize list to populate with choices, subqs, ...
      es_list <- list()

      ## Loop thru elements keeping track of order with seq_along
      for (j in seq_along(eids)) {
        eid <- eids[[j]]
        e <- es[[eid]]

        print(e)

        ## Filter out empty lists if they exist cause that breaks everything
        e <- Filter(lengths, e)
        e_df <- as.data.frame(e)

        ## Add qid and order to df
        e_df$qid <- qid
        e_df$order <- as.integer(j)

        ## Add element to elements list
        print(es_list)
        print(e_df)
        es_list <- c(list(e_df), es_list)
      }

      ## Add elements list to overall list of all question elements
      survey[[list_name]] <- c(es_list, survey[[list_name]])

      ## Remove element list data from object
      q[[list_name]] <- NULL
    }

    ## Now parse the question metadata with sublists removed
    q_unlist <- unlist(q, recursive = FALSE)
    q_df <- as.data.frame(q_unlist)

    ## Add qid and order to question DF
    q_df$qid <- qid
    q_df$order <- as.integer(i)

    ## Add question to questions list
    survey[["questions"]] <- c(list(q_df), survey[["questions"]])
  }

  ## Once all questions are parsed, turn all the parsed lists into a
  ## big ole' DF and in the process rename and reformat the columns

  survey <- empty_to_na(survey)
  return(survey)
  
  for (list_name in names(survey)) {
    if (is.na(survey[[list_name]])) break
    
    survey[[list_name]] <- bind_rows(survey[[list_name]])
    survey[[list_name]] <- auto_reformat(survey[[list_name]],
                                         prefix = "",
                                         strip.html)
  }

  ## Now onto parsing the metadata
  survey$responses <- s_resp

  ## Class definition and return
  class(survey) <- "qsurvey"
  return(survey)
}

print.survey <- function(x, ...) {

  c(sprintf("%s (%s)", s$meta$name, s$meta$id),
    sprintf("  $questions:"))
  
  ## [meta$survey_name] ([meta$survey_id])
  ##   created: [meta$creationDate]
  ##   $questions:
  ##     total: #35
  ##     

  
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
  choices <- filter(survey$choices, qid == q_qid)
  subqs <- filter(survey$subquestions, qid == q_qid)

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



question_subtypes <- list(
    "DB"    = "Graphic / Text Box",
    "MC"    = "Multiple Choice",
    "TE"    = "Text Entry",
    "SBS"   = "Side by Side"
    "PTB"   = "Plain Text Box",
    "TB"    = "Text Box",
    "GRB"   = "Graphics Box",
    "SB"    = "Select Box",
    "MSB"   = "Multiple Select Box",
    "DL"    = "Dropdown List",
    "SAHR"  = "Single Answer Horizontal",
    "SAVR"  = "Single Answer Vertical",
    "SACOL" = "Single Answer Column",
    "MACOL" = "Multiple Answer Column",
    "MAHR"  = "Multiple Answer Horizontal",
    "MAVR"  = "Multiple Answer Vertical",
    "CS"    = "Constant Sum",
    "WTB"   = "With Total Box",
    "WOTB"  = "Without Total Box",
    "RO"    = "Rank Order",
    "SL"    = "Single Line",
    "ML"    = "Multiple Line",
    "PW"    = "Password",
    "ESTB"  = "Essay",
    "FORM"  = "Form"
)
