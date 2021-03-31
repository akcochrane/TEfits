#' Create an integer vector coding a saturating function of time
#'
#' @param N Number of time values
#' @param style Should the integer "blocks" be saturating (increasing in length) ("increasing") or equal in size ("equal")
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # # Make a temporary data set
#' d_tmp <- anstrain_s1
#' 
#' # # Get an integer-coded time variable
#' d_tmp$trialNum_mono <- tef_mono_expo(nrow(d_tmp))
#' 
#' # # Look at the new coding of time
#' plot(d_tmp$trialNum,d_tmp$trialNum_mono)
#' 
#' # # Fit a model predicting accuracy using a monotonic measure of time; see documentation for brms::mo
#' library(brms)
#' m <- brm(acc ~ mo(trialNum_mono), d_tmp)
#' }
tef_mono_expo <- function(N,style="increasing"){
  
  maxMono <- ceiling(log(N))
  
  if(style=='increasing'){
    
  allMono_preAdjust <- exp(1:maxMono)
  
  allMono <- round(allMono_preAdjust/(exp(maxMono)/N))
  
  }else if(style=='equal'){
    
    allMono <- seq(1,N,length.out = maxMono+1)
    allMono <- allMono[2:length(allMono)]
    
  }else{stop('Your style is invalid.')}
  
  monoDat <- c() ; for(curMono in 1:maxMono){
    monoDat[length(monoDat):allMono[curMono]] <- curMono
  }
  if(sum(monoDat==1) < 3){
  monoDat[monoDat>1] <- monoDat[monoDat>1] - 1
  }
  return(monoDat)
}
