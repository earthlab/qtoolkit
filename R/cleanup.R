split_strings = function(string, n_char = 35) {
  #' Wrap long strings at x characters for pretty plotting and data viz.
  #' @param string of type char: the string that needs to be wrapped
  #' @param n_char number of characters to insert a \n after - default: 35
  #' @return a string with line breaks at every n_char characters
  #' @examples
  #' a_long_string <- "this is a string that i'd like to wrap"
  #' split_strings(a_long_string, n_char = 15)

  paste(strwrap(string, width = nwrap), collapse = "\n")
}

split_strings <-  Vectorize(split_strings)
