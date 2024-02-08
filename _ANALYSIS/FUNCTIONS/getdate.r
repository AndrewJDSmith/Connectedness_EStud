getdate <- function(x) {
  substr(x, 1, nchar(x)-4)
}
