#' Define parameter boundaries and guessing ranges
#'
#' \code{\link{TEfit}} internal. Future versions will systematize this more thoroughly.
#' Designed to variation in the starting points for optimization runs.
#'
#'
#' @param modList TEfit modList
#' @param whichPnames which parameter set?
#' @param linkFunX If applicable, the variable along which the link function varies
#'
#' @export
#'
tef_getBounds <- function(modList,whichPnames='pNames',linkFunX=NA){
  modList$guessNames <- modList[[whichPnames]]

  modList$parLims <- list(parMax = rep(1E10,length(modList$guessNames)),parMin=rep(-1E10,length(modList$guessNames)))
  modList$parGuessBounds <-  list(parMax = rep(.5,length(modList$guessNames)),parMin=rep(-.5,length(modList$guessNames)))

  if(nchar(modList$explicit)==0){
  ##
  guessGroups <- list()
  ##
  rateInds    <- grep('Rate',modList$guessNames)
  asymInds    <- grep('Asym',modList$guessNames)
  startInds   <- grep('Start',modList$guessNames)
  shapeInds   <- grep('Shape',modList$guessNames,ignore.case = T)
  fatigueInds <- grep('fatigue',modList$guessNames,ignore.case = T)
  prevTimeInds <- grep('prevTime',modList$guessNames,ignore.case = T)
  weibLinkShapeInds <- grep('weibull_shape',modList$guessNames,ignore.case = T)
  ##
  if(modList$errFun=='wiener_dr'){
    modList$wdModel <- wdm(modList$varIn[,modList$respVar])
    # print(modList$wdModel$coefficients)
  }
  ##
  if(length(rateInds)>0){

    guessGroups$rateInds_interc   <- c()

    guessGroups$rateInds_interc <- sort(
      which(substr(modList$guessNames,nchar(modList$guessNames)-2,nchar(modList$guessNames))=='teA' |
              substr(modList$guessNames,nchar(modList$guessNames)-2,nchar(modList$guessNames))=='teB' |
              substr(modList$guessNames,nchar(modList$guessNames)-2,nchar(modList$guessNames))=='ate' |
              substr(modList$guessNames,nchar(modList$guessNames)-2,nchar(modList$guessNames))=='e_0' |
              substr(modList$guessNames,nchar(modList$guessNames)-2,nchar(modList$guessNames))=='A_0' |
              substr(modList$guessNames,nchar(modList$guessNames)-2,nchar(modList$guessNames))=='B_0'
            ))

  modList$parLims$parMin[guessGroups$rateInds_interc] <- modList$rate_lim[1]
  modList$parLims$parMax[guessGroups$rateInds_interc] <- modList$rate_lim[2]
  modList$parGuessBounds$parMin[guessGroups$rateInds_interc] <- modList$rate_lim[1]
  modList$parGuessBounds$parMax[guessGroups$rateInds_interc] <- modList$rate_lim[2]
  if(length(rateInds)>1){guessGroups$rateInds_covars    <- rateInds[2:length(rateInds)]}}

  if(length(asymInds)>0){guessGroups$asymInds_interc    <- asymInds[1]
    modList$parGuessBounds$parMin[guessGroups$asymInds_interc] <- min(modList$varIn[,modList$respVar],na.rm=T)
  modList$parGuessBounds$parMax[guessGroups$asymInds_interc] <- max(modList$varIn[,modList$respVar],na.rm=T)

  if(modList$errFun=='exGauss_mu' || modList$errFun=='exGauss_tau'){
    modList$parGuessBounds$parMin[guessGroups$asymInds_interc] <- 0
    modList$parGuessBounds$parMax[guessGroups$asymInds_interc] <- max(modList$varIn[,modList$respVar],na.rm=T)*2
    modList$parLims$parMin[guessGroups$asymInds_interc] <- 0
    modList$parLims$parMax[guessGroups$asymInds_interc] <- max(modList$varIn[,modList$respVar],na.rm=T)*2
  }
  if(modList$errFun=='wiener_dr'){
    modList$parGuessBounds$parMin[guessGroups$asymInds_interc] <- -abs(modList$wdModel$coefficients['delta']*3)
    modList$parGuessBounds$parMax[guessGroups$asymInds_interc] <- abs(modList$wdModel$coefficients['delta']*3)
    modList$parLims$parMin[guessGroups$asymInds_interc] <- -abs(modList$wdModel$coefficients['delta']*3)
    modList$parLims$parMax[guessGroups$asymInds_interc] <- abs(modList$wdModel$coefficients['delta']*3)
  }
  if(modList$linkFun$link=='weibull' || modList$linkFun$link=='logit'){
    modList$parGuessBounds$parMin[guessGroups$asymInds_interc] <- 0
    modList$parGuessBounds$parMax[guessGroups$asymInds_interc] <- max(abs(modList$varIn[,linkFunX]),na.rm=T)*2
    modList$parLims$parMin[guessGroups$asymInds_interc] <- 0
    modList$parLims$parMax[guessGroups$asymInds_interc] <- max(abs(modList$varIn[,linkFunX]),na.rm=T)*2
  }
  if(length(asymInds)>1){guessGroups$asymInds_covars    <- asymInds[2:length(asymInds)]}}

  if(length(startInds)>0){guessGroups$startInds_interc   <- startInds[1]
  modList$parGuessBounds$parMin[guessGroups$startInds_interc] <- min(modList$varIn[,modList$respVar],na.rm=T)
  modList$parGuessBounds$parMax[guessGroups$startInds_interc] <- max(modList$varIn[,modList$respVar],na.rm=T)
  if(modList$errFun=='exGauss_mu' || modList$errFun=='exGauss_tau'){
    modList$parGuessBounds$parMin[guessGroups$startInds_interc] <- 0
    modList$parGuessBounds$parMax[guessGroups$startInds_interc] <- max(modList$varIn[,modList$respVar],na.rm=T)*2
    modList$parLims$parMin[guessGroups$startInds_interc] <- 0
    modList$parLims$parMax[guessGroups$startInds_interc] <- max(modList$varIn[,modList$respVar],na.rm=T)*2
  }
  if(modList$errFun=='wiener_dr'){
    modList$parGuessBounds$parMin[guessGroups$startInds_interc] <- -abs(modList$wdModel$coefficients['delta']*3)
    modList$parGuessBounds$parMax[guessGroups$startInds_interc] <- abs(modList$wdModel$coefficients['delta']*3)
    modList$parLims$parMin[guessGroups$startInds_interc] <- -abs(modList$wdModel$coefficients['delta']*3)
    modList$parLims$parMax[guessGroups$startInds_interc] <- abs(modList$wdModel$coefficients['delta']*3)
  }
  if(modList$linkFun$link=='weibull' || modList$linkFun$link=='logit'){
    modList$parGuessBounds$parMin[guessGroups$startInds_interc] <- 0
    modList$parGuessBounds$parMax[guessGroups$startInds_interc] <- max(abs(modList$varIn[,linkFunX]),na.rm=T)*2
    modList$parLims$parMin[guessGroups$startInds_interc] <- 0
    modList$parLims$parMax[guessGroups$startInds_interc] <- max(abs(modList$varIn[,linkFunX]),na.rm=T)*2
  }
  if(length(startInds)>1){guessGroups$startInds_covars   <- startInds[2:length(startInds)]}}

  if(length(shapeInds)>0){guessGroups$shapeInds_interc   <- shapeInds[1]
  if(exists('shape_lim',modList)){shape_lim <- modList$shape_lim}else{shape_lim <- modList$shape_lim <- c(-1E1,1E1)}
  meanShape <- mean(modList$shape_lim)
  modList$parLims$parMin[guessGroups$shapeInds_interc] <- modList$shape_lim[1]
  modList$parLims$parMax[guessGroups$shapeInds_interc] <- modList$shape_lim[2]
  modList$parGuessBounds$parMin[guessGroups$shapeInds_interc] <- mean(c(modList$shape_lim[1],meanShape))
  modList$parGuessBounds$parMax[guessGroups$shapeInds_interc] <- mean(c(modList$shape_lim[2],meanShape))
  if(length(shapeInds)>1){guessGroups$shapeInds_covars   <- shapeInds[2:length(shapeInds)]}}

  if(length(fatigueInds)>0){guessGroups$fatigueInds_interc <- fatigueInds[1]
  if(exists('fatigue_lim',modList)){fatigue_lim <- modList$fatigue_lim}else{fatigue_lim <- modList$fatigue_lim <- c(nrow(modList$varIn)/2,nrow(modList$varIn))}
  modList$parLims$parMin[guessGroups$fatigueInds_interc] <- modList$fatigue_lim[1]
  modList$parLims$parMax[guessGroups$fatigueInds_interc] <- modList$fatigue_lim[2]
  modList$parGuessBounds$parMin[guessGroups$fatigueInds_interc] <- modList$fatigue_lim[1]
  modList$parGuessBounds$parMax[guessGroups$fatigueInds_interc] <- modList$fatigue_lim[2]
  if(length(fatigueInds)>1){guessGroups$fatigueInds_covars <- fatigueInds[2:length(fatigueInds)]}}

  if(length(prevTimeInds)>0){guessGroups$prevTimeInds_interc <- prevTimeInds[1]
  if(exists('prevTime_lim',modList)){prevTime_lim <- modList$prevTime_lim}else{prevTime_lim <- modList$prevTime_lim <- c(0,nrow(modList$varIn)/2)}
  modList$parLims$parMin[guessGroups$prevTimeInds_interc] <- modList$prevTime_lim[1]
  modList$parLims$parMax[guessGroups$prevTimeInds_interc] <- modList$prevTime_lim[2]
  modList$parGuessBounds$parMin[guessGroups$prevTimeInds_interc] <- modList$prevTime_lim[1]
  modList$parGuessBounds$parMax[guessGroups$prevTimeInds_interc] <- modList$prevTime_lim[2]
  if(length(prevTimeInds)>1){guessGroups$prevTimeInds_covars <- prevTimeInds[2:length(prevTimeInds)]}}


  if(modList$errFun=='wiener_dr'){
    modList$parLims$parMin[grep("Intercept",modList$guessNames)] <- -abs(modList$wdModel$coefficients['delta']*3)
    modList$parLims$parMax[grep("Intercept",modList$guessNames)] <-  abs(modList$wdModel$coefficients['delta']*3)
    modList$parLims$parMin[grep('bs_param',modList$guessNames)] <- 1E-6
    modList$parLims$parMin[grep('ndt_param',modList$guessNames)] <- 1E-6
    modList$parLims$parMax[grep('ndt_param',modList$guessNames)] <- modList$wdModel$coefficients['tau']*3
    modList$parLims$parMin[grep('bias_param',modList$guessNames)] <- 0
    modList$parLims$parMax[grep('bias_param',modList$guessNames)] <- 1
    modList$parLims$parMax[grep('bs_param',modList$guessNames)] <- modList$wdModel$coefficients['alpha']*3
    modList$parGuessBounds <- modList$parLims

  }
  if(length(weibLinkShapeInds)>0){guessGroups$weibLinkShapeInds_interc <- weibLinkShapeInds[1]
  if(exists('fatigue_lim',modList)){fatigue_lim <- modList$fatigue_lim}else{fatigue_lim <- modList$fatigue_lim <- c(nrow(modList$varIn)/2,nrow(modList$varIn))}
  modList$parLims$parMin[guessGroups$weibLinkShapeInds_interc] <- 0
  modList$parLims$parMax[guessGroups$weibLinkShapeInds_interc] <- 10
  modList$parGuessBounds$parMin[guessGroups$weibLinkShapeInds_interc] <- 0
  modList$parGuessBounds$parMax[guessGroups$weibLinkShapeInds_interc] <- 10
  if(length(weibLinkShapeInds)>1){guessGroups$weibLinkShapeInds_covars <- weibLinkShapeInds[2:length(weibLinkShapeInds)]}}

  }

  return(modList)
}
