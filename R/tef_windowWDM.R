#' Find a data frame of Wiener Diffusion Model parameters
#'
#' Estimates a set of Wiener Diffusion Model parameters for a vector of formatted
#' data. Estimation uses \code{\link[RWiener]{wdm}} and data must be formatted as a
#' 1-dimensional vector according the to \code{RWiener} package (i.e., RT * 2 * (boundary=='upper' - 0.5) ).
#' Assumes that RTs are associated with equally-spaced and sorted timepoints (e.g., trials of a behavioral study).
#' First fits a \code{\link[RWiener]{wdm}} for the entire RT vector, and applies some of these parameters
#' (NDT, bias, and possibly BS) to the entire RT vector. Next takes a set of windows, equally spaced and centered at
#' \code{windowSize/2}, and estimates the DR (and possibly BS) for each window. Averages overlapping windows.
#' Returns a data frame of estimated parameters, including 95\% CI.
#'
#' @param dat Response time vector. Positive values indicates upper-boundary while negative values indicate lower-boundary.
#' @param windowSize Total width of each window for estimating parameters.
#' @param fit_BS Logical. If \code{FALSE} only drift rate is estimated, if \code{TRUE} both drift rate and boundary separation are estimated.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dat <- (rnorm(200,.2,.05) + rexp(200,1 / .2))*sample(c(-1,1),200,replace=T)
#'     m_drY_bsN <- tef_windowWDM(dat,fit_BS = F)
#'     m_drY_bsY <- tef_windowWDM(dat,fit_BS = T)
#'     psych::pairs.panels(cbind(trialNum = 1:200, RT = abs(dat),m_drY_bsY[,1:6]))
#'     }
tef_windowWDM <- function(dat,windowSize=20,fit_BS=FALSE){

  fit_DR=T ## ideally this would be an argument, but only fitting BS always returns errors.

  require(RWiener)

  overallMod <- wdm(dat[!is.na(dat)])
  overallPars <- overallMod$coefficients
  overallCI <- summary(overallMod)$cint

  if(fit_DR){drEst <- data.frame( Estimate = rep(NA,length(dat)), ci975 = NA , ci025 = NA)
  }else{drEst <- data.frame(Estimate = overallPars['delta'], ci025 = overallCI['delta',"2.5 %"], ci975 = overallCI['delta',"97.5 %"])}
  if(fit_BS){bsEst <- data.frame( Estimate = rep(NA,length(dat)), ci975 = NA , ci025 = NA)
  }else{bsEst <- data.frame(Estimate = overallPars['alpha'], ci025 = overallCI['alpha',"2.5 %"], ci975 = overallCI['alpha',"97.5 %"])}

  half_kernel <- round(windowSize/2)

  kernel_centers <- seq(half_kernel,length(dat) - half_kernel, half_kernel)

  for(curKernel in kernel_centers){

    curIndices <-  max(curKernel - half_kernel + 1, 1) : min(curKernel+half_kernel, length(dat))

    curDat <- dat[curIndices]
    curDat <- curDat[!is.na(curDat)]

  try({ ## because optimization will inevitably fail for some windows, if the window size is too small

    switch(paste(fit_DR,fit_BS)

           ,'TRUE TRUE' = {
             curMod <- wdm(curDat,tau = overallPars['tau'],beta = overallPars['beta'])

             drEst[curIndices,'Estimate'] <-
               rowMeans(cbind(drEst[curIndices,'Estimate'],curMod$coefficients['delta']),na.rm=T )
    drEst[curIndices,'ci025'] <-
      rowMeans(cbind(drEst[curIndices,'ci025'],summary(curMod)$cint['delta',"2.5 %"]),na.rm=T )
    drEst[curIndices,'ci975'] <-
      rowMeans(cbind(drEst[curIndices,'ci975'],summary(curMod)$cint['delta',"97.5 %"]),na.rm=T )

    bsEst[curIndices,'Estimate'] <-
      rowMeans(cbind(bsEst[curIndices,'Estimate'],curMod$coefficients['alpha']),na.rm=T )
bsEst[curIndices,'ci025'] <-
  rowMeans(cbind(bsEst[curIndices,'ci025'],summary(curMod)$cint['alpha',"2.5 %"]),na.rm=T )
bsEst[curIndices,'ci975'] <-
  rowMeans(cbind(bsEst[curIndices,'ci975'],summary(curMod)$cint['alpha',"97.5 %"]),na.rm=T )
           }

,'TRUE FALSE' = {
  curMod <- wdm(curDat,alpha = overallPars['alpha'],tau = overallPars['tau'],beta = overallPars['beta'])

  drEst[curIndices,'Estimate'] <-
    rowMeans(cbind(drEst[curIndices,'Estimate'],curMod$coefficients['delta']),na.rm=T )
drEst[curIndices,'ci025'] <-
  rowMeans(cbind(drEst[curIndices,'ci025'],summary(curMod)$cint['delta',"2.5 %"]),na.rm=T )
drEst[curIndices,'ci975'] <-
  rowMeans(cbind(drEst[curIndices,'ci975'],summary(curMod)$cint['delta',"97.5 %"]),na.rm=T )

}

,'FALSE TRUE' = {
  curMod <- wdm(curDat,delta = overallPars['delta'],tau = overallPars['tau'],beta = overallPars['beta'])

  bsEst[curIndices,'Estimate'] <-
    rowMeans(cbind(bsEst[curIndices,'Estimate'],curMod$coefficients['alpha']),na.rm=T )
bsEst[curIndices,'ci025'] <-
  rowMeans(cbind(bsEst[curIndices,'ci025'],summary(curMod)$cint['alpha',"2.5 %"]),na.rm=T )
bsEst[curIndices,'ci975'] <-
  rowMeans(cbind(bsEst[curIndices,'ci975'],summary(curMod)$cint['alpha',"97.5 %"]),na.rm=T )

}
)

   },silent=T) # the kernel-fitting try()

} # close kernel loop

  colnames(drEst) <- paste0('dr_',colnames(drEst))
  colnames(bsEst) <- paste0('bs_',colnames(bsEst))

  suppressWarnings({
  outTable <- data.frame(
    drEst
    ,bsEst
    ,ndt_Estimate = overallPars['tau']
    ,ndt_c025 = overallCI['tau',"2.5 %"]
    ,ndt_c975 = overallCI['tau',"97.5 %"]
    ,bias_Estimate = overallPars['beta']
    ,bias_c025 = overallCI['beta',"2.5 %"]
    ,bias_c975 = overallCI['beta',"97.5 %"]
  )
  })

  if(any(apply(outTable,2,function(x){mean(is.na(x))>.25}))){
    warning('Estimation often failed. Using a larger window size is recommended.')
  }

  outTable$original_data <- dat

  attr(outTable,'overall_model') <- overallMod
  attr(outTable,'overall_parameters') <- overallPars
  attr(outTable,'overall_CI') <- overallCI

  return(outTable)
}
