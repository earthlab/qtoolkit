#' is.obj.type
#'
#' Test if passed variable is particular object
#'
#' @param var Object to be tested
#' @param type Type of object to be tested
#' 
#' @return TRUE if object is qsurvey object; otherwise FALSE
#' @export

is.obj.type <- function(var, type) {
  return( if (class(var) == type) TRUE else FALSE )
}

#' empty_to_na
#'
#' If dataframe is empty return a NA, otherwise return the dataframe
#'
#' @param df Dataframe to test
#'
#' @return NA if df is empty; original df if not
#' @export

empty_to_na <- function(df) {
  return( if (dim(df)[1] == 0) NA else df )
}

#' auto_format
#'
#' Automatically rename, reorder, according to map and optionally
#' strip HTML from relevant columns
#'
#' @importFrom assertthat assert_that
#'
#' @param df Dataframe to rename and reorder columns
#' @param prefix Optional prefix to add before renamed columns
#'
#' @return DF with renamed and reordered columns
#' @export

auto_reformat <- function(df,
                          prefix = "",
                          strip.html = FALSE) {

  ## User input validation
  assert_that(is.data.frame(df))

  ## Map of previous list names to what we want them renamed to
  rename_map <- list(
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

  ## List of the ideal order of columns
  reorder_cols<- c("qid", "name", "order","text", "type", "selector",
               "subselector", "required")

  ## Rows to be ordered by
  reorder_rows <- c("order", "qid")
  
  ## Cols to strip HTML if they exist
  strip_html_cols <- c("name", "text")

  ## Add optional prefix before name
  if (prefix != "") {

    ## Prefix function will prefix all fields but qid
    prefix_fn <- function(field, prefix) {
      if (field == "qid") {
        return(field)
      } else {
        return(paste0(prefix, "_", field))
      }
    }

    ## Apply prefixes to above fields
    rename_map <- lapply(rename_map,
                         function(i) { prefix_fn(i, prefix) })
    
    reorder_cols<- sapply(reorder_cols,
                          function(j) { prefix_fn(j, prefix) },
                          USE.NAMES = FALSE)

    strip_html_cols <- sapply(strip_html_cols,
                              function(k) { prefix_fn(k, prefix) },
                              USE.NAMES = FALSE)

    reorder_rows <- sapply(reorder_rows,
                           function(l) { prefix_fn(l, prefix) },
                           USE.NAMES = FALSE)
  }

  ## Rename the colnames
  all_renames <- names(rename_map)
  df_names <- names(df)
  rename_names <- match(all_renames, df_names)

  names(df)[na.omit(rename_names)] <- unlist(rename_map[which(!is.na(rename_names))],
                                             use.names = FALSE)

  renamed_cols <- names(df)
  
  ## Reorder the columns
  reorder_order <- match(reorder_cols, renamed_cols)
  dfcols_order <- match(renamed_cols, reorder_cols)
  
  matched <- na.omit(reorder_order)
  unmatched <- which(is.na(dfcols_order))

  df <- df[,c(matched, unmatched)]

  ## Reorder the rows based upon columns
  reorder_rows <- na.omit(match(reorder_rows, renamed_cols))
  
  row_order <- do.call(order, as.list(df, reorder_rows))
  df <- df[row_order,]

  ## Strip HTML if specified, strip html from the relevant columns
  if (strip.html) {
    strip_cols <- na.omit(match(strip_html_cols, renamed_cols))

    for (col in strip_cols) {
      df[, col] <- strip_html(df[, col])
    }
  }

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
