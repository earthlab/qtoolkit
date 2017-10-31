#' strip_html
#'
#' Strip HTML tags from string, with RegEx
#' 
#' @param text Text to strip HTML from
#' @param consolidate Consolidate stripped HTML; Turn line break into a space, collapse extra spaces.
#' 
#' @return Text with HTML stripped
#' @export

strip_html <- function(text, consolidate = TRUE) {
  ## Strip HTML tags
  stripped <- gsub("<.*?>", "", text)

  ## Turn &#39; into apostrophe
  stripped <- gsub("&#39;","'", stripped)

  ## Turn &amp; to amperstand
  stripped <- gsub("&amp;", "&", stripped)

  if (consolidate) {
    ## Turn all newlines into //
    stripped <- gsub("\n", " // ", stripped)

    ## Consolidate 2 whitespace characters into a one space
    stripped <- gsub("\\s{2,}", " ", stripped)

    ## Strip whitespace from start and end of string
    stripped <- trimws(stripped)
  }

  return(stripped)
}

#' check_duplicate_question
#'
#' Check whether a survey has a duplicate question number
#'
#' @param survey Survey to check
#'
#' @export

check_duplicate_question <- function(survey) {
  
  # Get questions
  qs_test <- get_question(survey)
  
  # Select just what we're looking at
  qs_test <- dplyr::select(qs_test, question_id, question_num)

  # Get rows with distinct question_id
  qs_test <- dplyr::distinct(qs_test, question_id, question_num)
  
  # Count distinct question_ids
  qs_test <- dplyr::count(qs_test, question_num)

  # Select if any questions have over 1 record and error if so
  qs_test <- dplyr::filter(qs_test, n>1)

  if ( dim(qs_test)[1] > 0 ) {
    err <- paste("Error: ", survey$name, " has duplicate question ",
                 qs_test[1,1], sep="")
    stop(err)
  }
}
