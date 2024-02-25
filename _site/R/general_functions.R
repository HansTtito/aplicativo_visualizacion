ucfirst <- function(x) {
  x = gsub('_',' ',x)
  x = paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "")
  return(x)
}
