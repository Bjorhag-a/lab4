euclidean <-
function(a , b){
  
  if (!is.numeric(a) | !is.numeric(b)) {
    stop("test")
  }
  
  while (b != 0) {
    
    t <- b
    b <- a %% b
    a <- t
  }
  
  return(a)
  
}
