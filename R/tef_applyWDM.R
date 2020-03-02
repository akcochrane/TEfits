

#' Calculate the Wiener negative log likelihood of a vector of data using a vector of drift rates.
#'
#' Within a TEfit call, errFun='wiener_df' is likely to be extremely slow, e.g., 70 seconds per run.
#'
#' @param dat Vector of data (e.g., RT). Hitting upper bound must be positively signed; hitting lower bound must be negatively signed.
#' @param DR Vector of drift rates. Must be the same length as dat.
#' @param BS Boundary separation parameter. Positive scalar.
#' @param NDT Non-decision time parameter. Positive scalar.
#' @param Bias Bias parameter. Positive scalar.
#'
#' @export
#' @examples
#' dat <- c(-1,1,-.5,.5)
#' driftRate <- c(.2,.3,.4,.5)
#' tef_applyWDM(dat,DR=driftRate,BS=1,NDT=.3,Bias=.5)
#'
tef_applyWDM <- function(dat,DR,BS,NDT,Bias=.5){

  nonNA <- sum(!is.na(dat))

  if(length(dat) != length(DR)){
    cat('\nYour data and drift rate vectors are not the same length.\n')
  }


  boundary <- NA
  boundary[sign(dat)==-1] <- 'lower'
  boundary[sign(dat)==1] <- 'upper'


  wdm_negloglik <- rep(NA,length(dat))
  for(curRT in 1:length(dat)){
          # print(c(abs(dat[curRT]),
          # alpha=BS,
          # tau=NDT,
          # beta=Bias,
          # delta=DR[curRT],
          # resp=boundary[curRT]))
    try({

    wdm_negloglik[curRT] <- -dwiener(as.wiener(abs(dat[curRT])),
                                               alpha=BS,
                                               tau=NDT,
                                               beta=Bias,
                                               delta=DR[curRT],
                                     resp=boundary[curRT],
                                     give_log = T)
    },silent=T)
  }
  negloglik <- sum(wdm_negloglik,na.rm = T)

  if(sum(!is.na(wdm_negloglik)) < sum(!is.na(dat))){
    # cat('your evaluate likelihoods have more NA than your data')
    negloglik <- 1E12
  }
  return(negloglik)
}
