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
#' @param fatal Stop execution if duplicate question, or no
#'
#' @export

check_duplicate_question <- function(survey, fatal = FALSE) {

  # Get DF of question_ids and question_nums
  qs_map <- data.frame(question_id = names(survey$questionMap),
                       question_num = unlist(survey$questionMap),
                       row.names = NULL)
  
  # Count distinct question_ids
  qs_test <- dplyr::count(qs_map, question_num)

  # Select if any questions have over 1 record and error if so
  qs_test <- dplyr::filter(qs_test, n>1)
  num_dupes <- dim(qs_test)[1]
  
  if ( num_dupes > 0 ) {
    for ( dupe_num in 1:num_dupes ) {
      record <- qs_test[dupe_num,]
      msg <- paste0(survey$name, " has ", record$n,
                    " entries for question ", record$question_num)
      
      if ( fatal == TRUE ) {
        stop(msg, call. = FALSE)
      } else {
        warning(msg, call. = FALSE)
      }
    }
  }
}
