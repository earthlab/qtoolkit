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
