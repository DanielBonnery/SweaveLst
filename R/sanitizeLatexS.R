#' sanitise latex
#' @param str a character string
sanitizeLatexS <-
function(str) {
  cat(paste(gsub('([#$%&~_\\^\\\\{}])', '\\\\\\\\\\1', str, perl = TRUE),collapse="\n"));
}
