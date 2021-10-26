parse_par <- function(x) {
  if (typeof(x) == 'character') {
    if_else(x == 'E', 0L, parse_integer(x))
  } else {
    as.integer(x)
  }
}
