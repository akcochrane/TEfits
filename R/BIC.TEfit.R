


#' Extract BIC from a TEfit model
#'
#' @param TEs3 A TEfit model
#'
#' @export
#'
BIC.TEfit <- function(TEs3){

  if(exists('BIC', TEs3$model$GoF)){
  return(TEs3$model$GoF['BIC'])}else{
    cat('\nThis object does not have a BIC. ')
  }
}
