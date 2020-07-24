compact_null <- function(x) {
  Filter(function(x) !is.null(x), x)
}
