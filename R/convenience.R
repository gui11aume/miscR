subs <- function(x, keys, values) {
   # Replace 'keys' by 'values' in 'x'.
   found <- match(x, keys, nomatch=0)
   replace(x, found > 0, values[found])
}
