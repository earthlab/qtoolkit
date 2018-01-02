#' auto_rename_and_reorder
#'
#' Automatically rename and reorder columns according to map
#'
#' @importFrom assertthat assertthat
#'
#' @param df Dataframe to rename and reorder columns
#' @param prefix Optional prefix to add before renamed columns
#'
#' @return DF with renamed and reordered columns
#' @export

auto_rename_and_reorder <- function(df,
                                    prefix = "") {

  rename.map <- list(
      ## For (Sub)Questions dataframe
      "questionType.type"             = "type",
      "questionType.selector"         = "selector",
      "questionType.subSelector"      = "subselector",
      "questionText"                  = "text",
      "questionLabel"                 = "label",
      "validation.doesForceResponse"  = "required",
      "questionName"                  = "name",
      "order"                         = "order",

      ## For Choices dataframe
      "description"                   = "desc",
      "choiceText"                    = "text",
      "imageDescription"              = "image_desc",
      "variableName"                  = "var_name"
  )

  reorder <- c("name", "text", "type", "selector", "subselector",
               "required")

  ## Add optional prefix before name
  if (prefix != "") {
    rename.map <- lapply(rename.map,
                         function(i) { return(paste0(prefix,"_",i)) })
    
    reorder <- sapply(reorder,
                      function(j) { return(paste0(prefix,"_",j)) },
                      USE.NAMES = FALSE)

    ## Sneak `qid` in at the front, with no prefix
    reorder <- c("qid", reorder)
  }

  ## Rename the colnames
  all_renames <- names(rename.map)
  df_names <- names(df)
  rename_names <- match(all_renames, df_names)

  names(df)[na.omit(rename_names)] <- unlist(rename.map[which(!is.na(rename_names))],
                                             use.names = FALSE)

  ## Reorder the columns
  reorder_order <- match(reorder, names(df))
  dfcols_order <- match(names(df), reorder)
  
  matched <- na.omit(reorder_order)
  unmatched <- which(is.na(dfcols_order))

  df <- df[,c(matched, unmatched)]

  ## Return renamed and reordered DF
  return(df)
}

#' strip_html
#'
#' Strip HTML tags from string, with RegEx
#' 
#' @param text Text to strip HTML from
#' @param consolidate Consolidate stripped HTML; Turn line break into a space, collapse extra spaces.
#' 
#' @return Text with HTML stripped
#' @export

strip_html <- function(text,
                       consolidate = TRUE) {

  if (consolidate == FALSE) {
    stripped <- gsub("<.*?>", "", text)
  } else {
    stripped <- gsub("<.*?>", " ", text)       ## Strip HTML tags
    stripped <- gsub("\n", " ", stripped)      ## Turn all newlines into spaces
    stripped <- gsub("\\s{2,}", " ", stripped) ## Consolidate 2 whitespace characters into a one space
    stripped <- trimws(stripped)               ## Strip whitespace from start and end of string
  }

  stripped <- gsub("&#39;","'", stripped)  ## Turn &#39; into apostrophe
  stripped <- gsub("&amp;", "&", stripped) ## Turn &amp; to amperstand

  return(stripped)
}

#' check_duplicate_question
#'
#' Check whether a survey has a duplicate question number and report
#' to user if it does
#'
#' @param survey Survey object to check
#' @param fatal Stop execution if duplicate question, or no
#'
#' @export

check_duplicate_question <- function(survey,
                                     fatal = FALSE) {

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
      msg <- paste0("'", survey$name, "' has ", record$n,
                    " questions named '", record$question_num, "'")
      
      if ( fatal == TRUE ) {
        stop(msg, call. = FALSE)
      } else {
        warning(msg, call. = FALSE)
      }
    }
  }
}
