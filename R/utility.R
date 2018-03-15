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

#' split_strings
#'
#' Wrap long strings at x characters for pretty plotting and data viz.
#'
#' @param string of type char: the string that needs to be wrapped
#' @param nchar number of characters to insert a \n after - default: 35
#'
#' @return a string with line breaks at every n_char characters
#' @export

split_strings <- function(string, nchar = 35) {
  paste(strwrap(string, width = nchar), collapse = "\n")
}

split_strings <-  Vectorize(split_strings)

#' empty_to_na
#'
#' Loop thru list and turn empty dataframes and lists into NA
#'
#' @param list List to test
#'
#' @return original list with empty elements NA
#' @export

empty_to_na <- function(lst) {
  lst <- lapply(lst,
                function(el) {
                  if (is.list(el) && length(el) == 0) return(NA)
                  if (is.data.frame(el) && dim(el)[1] == 0) return(NA)
                  return(el)
                })

  return(lst)
}

#' auto_reformat
#'
#' Automatically rename, reorder, according to map and optionally
#' strip HTML from relevant columns
#'
#' @importFrom assertthat assert_that
#'
#' @param df data.frame to rename and reorder columns
#' @param prefix Optional prefix to add before renamed columns
#'
#' @return DF with renamed and reordered columns
#' @export

auto_reformat <- function(df,
                          prefix = "",
                          rename.cols = TRUE,
                          reorder.rows = TRUE,
                          reorder.cols = TRUE,
                          strip.html = FALSE) {

  ## User input validation
  assert_that(is.data.frame(df))

  ## Map of previous list names to what we want them renamed to
  rename_map <- list(
      ## For (Sub)Questions
      "questionType.type"             = "type",
      "questionType.selector"         = "selector",
      "questionType.subSelector"      = "subselector",
      "questionText"                  = "text",
      "questionLabel"                 = "label",
      "validation.doesForceResponse"  = "required",
      "questionName"                  = "name",
      "order"                         = "order",

      ## For Choices
      "description"                   = "desc",
      "choiceText"                    = "text",
      "imageDescription"              = "image_desc",
      "variableName"                  = "var_name",

      ## For Blocks
      "questionId"                    = "qid"
  )

  ## List of the ideal order of columns
  reorder_cols <- c("qid", "name", "order","text", "type", "selector",
                    "subselector", "required")

  ## Rows to be ordered by
  reorder_rows_cols <- c("bid", "b_order", "order", "qid")

  ## Cols to strip HTML if they exist
  strip_html_cols <- c("name", "text")

  ## Add optional prefix before column names
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

    reorder_rows_cols <- sapply(reorder_rows_cols,
                                function(l) { prefix_fn(l, prefix) },
                                USE.NAMES = FALSE)
  }

  ## Rename the colnames
  if (rename.cols) {
    all_renames <- names(rename_map)
    df_names <- names(df)
    rename_names <- match(all_renames, df_names)

    names(df)[na.omit(rename_names)] <- unlist(rename_map[which(!is.na(rename_names))],
                                               use.names = FALSE)

    renamed_cols <- names(df)
  }

  ## Reorder the columns
  if (reorder.cols) {
    reorder_order <- match(reorder_cols, renamed_cols)
    dfcols_order <- match(renamed_cols, reorder_cols)

    matched <- na.omit(reorder_order)
    unmatched <- which(is.na(dfcols_order))

    df <- df[,c(matched, unmatched)]
  }

  ## Reorder the rows based upon columns
  ## In case anyone's reading, R is an unnecessarily diffict language
  if (reorder.rows) {
    reorder_rows <- na.omit(match(reorder_rows_cols, renamed_cols))
    reorder_rows_list <- lapply(reorder_rows, function(r) { return(df[,r]) })

    row_order <- do.call(order, reorder_rows_list)
    df <- df[row_order,]
  }

  ## If specified, strip html from the relevant columns
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

#' check_duplicate_questions
#'
#' Check whether a survey has a duplicate question number and report
#' to user if it does
#'
#' @param survey Survey object to check
#' @param fatal Stop execution if duplicate question, or no
#' @export

check_duplicate_questions <- function(survey,
                                     fatal = FALSE) {

  ## Get DF of question_ids and question_nums
  qs_list <- survey$questionList

  ## Count distinct question_ids
  qs_test <- dplyr::count(qs_list, name)

  ## Select if any questions have over 1 record and error if so
  qs_test <- dplyr::filter(qs_test, n>1)
  num_dupes <- dim(qs_test)[1]

  if ( num_dupes > 0 ) {
    for ( dupe_num in 1:num_dupes ) {
      record <- qs_test[dupe_num,]
      msg <- paste0("'", survey$name, "' has ", record$n,
                    " questions named '", record$name, "'")

      if ( fatal == TRUE ) {
        stop(msg, call. = FALSE)
      } else {
        warning(msg, call. = FALSE)
      }
    }
  }
}


#' type_acronym_to_text
#'
#' Convert acronym of question type/selector/subselector to human readable
#'
#' @param acronym Acronym to convert to human readable text
#'
#' @return Long text form of acronym if acronym is known, or empty string
#' @export

type_acronym_to_text <- function(acronym) {

  acronym_map <- list(
      "DB"    = "Graphic / Text Box",
      "MC"    = "Multiple Choice",
      "TE"    = "Text Entry",
      "SBS"   = "Side by Side",
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

  if (acronym %in% names(acroym_map)) {
    return(acronym_map[[acronym]])
  } else {
    return("")
  }
}

#' nested_list_to_df
#'
#' Convert a named list to dataframe and preserve type
#'
#' @importFrom dplyr bind_rows
#'
#' @param lst Named list
#'
#' @return Dataframe instead of nested List
#' @export

nested_list_to_df <- function(lst) {

  ## es:   list elements
  ## eids: list elements ids
  ## eid:  list element id
  ## e:    list element

  ## Get elements of list and their names
  es <- lst
  eids <- names(es)

  ## If list isn't named list, use indexes
  if (is.null(eids)) eids <- 1:length(es)

  ## Initialize list to populate with single-row dfs
  es_list <- list()

  ## Loop thru elements keeping track of order with seq_along
  for (j in seq_along(eids)) {
    eid <- eids[[j]]
    e <- es[[eid]]

    ## Filter out empty lists if they exist cause that breaks everything
    ## And turn named list into a dataframe
    e <- Filter(lengths, e)
    e_df <- as.data.frame(e, stringsAsFactors = FALSE)

    ## Add order to df
    e_df$order <- as.integer(j)

    ## Add element df to elements list
    es_list <- c(es_list, list(e_df))
  }

  ## Turn element list to DF with dplyr and return
  ## dplyr used b/c rows may not all have same column headings

  es_df <- bind_rows(es_list)

  return(es_df)
}
