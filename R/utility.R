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
  
  stripped <- gsub("<.*?>", "", text)      ## Strip HTML tags
  stripped <- gsub("&#39;","'", stripped)  ## Turn &#39; into apostrophe
  stripped <- gsub("&amp;", "&", stripped) ## Turn &amp; to amperstand

  if (consolidate) {
    stripped <- gsub("\n", " ", stripped)      ## Turn all newlines into //
    stripped <- gsub("\\s{2,}", " ", stripped) ## Consolidate 2 whitespace characters into a one space
    stripped <- trimws(stripped)               ## Strip whitespace from start and end of string
  }

  return(stripped)
}

#' check_duplicate_question
#'
#' Check whether a survey has a duplicate question number.
#'
#' @param survey Survey to check
#'
#' @export

check_duplicate_question <- function(survey) {

  # Get DF of question_ids and question_nums
  qs_map <- data.frame(question_id = names(survey$questionMap),
                       question_num = unlist(survey$questionMap),
                       row.names = NULL)
  
  # Count distinct question_ids
  qs_test <- dplyr::count(qs_map, question_num)

  # Select if any questions have over 1 record and error if so
  qs_test <- dplyr::filter(qs_test, n>1)

  if ( dim(qs_test)[1] > 0 ) {
    err <- paste("Error: ", survey$name, " has duplicate question ",
                 qs_test[1,1], sep="")
    stop(err)
  }
}
