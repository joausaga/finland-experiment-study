# Remove Finnish special characters
remover <- function(x){
  x <- gsub('\x8a', 'a', x)
  x <- gsub('\x9a', 'o', x)
  return(x)
}