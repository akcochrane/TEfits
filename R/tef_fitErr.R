#' Calculate the error of data, given the model and certain parameters
#'
#' \code{\link{TEfit}} internal.
#'
#' @param guesses proposed parameter values
#' @param varIn Data to be fit
#' @param pNames Parameter names
#' @param evalFun Model function
#' @param errFun Error function
#' @param respVar The name of the response variable
#' @param y_lim Bounds for the predicted values
#' @param rate_lim Bounds for the rate parameter
#' @param shape_lim Bounds for the shape parameter, if needed.
#' @param penalizeMean two-element vector. The first element is logical, the second is the mean predicted value from the null fit
#' @param penalizeRate logical, whether or not to penalize rate parameter
#' @param parLims parameter limits
#' @param thresh_fun if relevant, the threshold function
#' @param paramTerms the terms for parameters
#'
#' @export
#'
#'
tef_fitErr <- function(guesses,varIn,pNames,evalFun,errFun,respVar,linkFunX=NA,
                       y_lim,rate_lim,shape_lim,penalizeMean,penalizeRate,
                       parLims,thresh_fun,paramTerms){

  guessDat <- as.data.frame(matrix(guesses,dim(varIn)[1],length(guesses),byrow=T))
  colnames(guessDat) <- pNames

  # cat(guesses,'\n')

  # cat(evalFun)

  ## generate the model predictions
  curDat <- data.frame(varIn,guessDat)
  yHat <- eval(expr=evalFun,env=curDat)

  # cat(' >.yhat: ',yHat[1:2],' .< ')
 # cat(guessDat[1:2,])
 #  cat(as.character(evalFun),'\n')

  if(sum(is.na(yHat))>sum(is.na(curDat[,respVar]))){ # check for more predicted NAs than original NAs
    err <- 1E15}else{

## any error function that needs more than y and yHat:
      if(errFun=='exGauss_mu'||
         errFun=='exGauss_tau' ||
         errFun=='wiener_dr'){
        err <- tef_err(y=curDat[,respVar],yHat,errFun,curDat)
      }else{
        err <- tef_err(y=curDat[,respVar],yHat,errFun)
      }

      ## see what the model would predict at an asymptotic time
      asymDat <- curDat ; asymDat[,2] <- max(curDat[,2],na.rm = T)*10 # "Asymptotic" time point
      asym_yHat <- eval(expr=evalFun,env=asymDat)

      ## should be able to streamline this calculation of "previous time" predicted values
      if(any(names(guesses)=='threshPrevTime_Intercept')){prev_time <- unique(guesses['threshPrevTime_Intercept'])}
      if(any(names(guesses)=='pPrevTime_Intercept')){prev_time <- unique(guesses['pPrevTime_Intercept'])}
      if(any(names(guesses)=='threshPrevTime')){prev_time <- unique(guesses['threshPrevTime'])}
      if(any(names(guesses)=='pPrevTime')){prev_time <- unique(guesses['pPrevTime'])}
      if(exists('prev_time')){asymDat[,2] <- curDat[,2]-prev_time
      asym_yHat <- rbind(asym_yHat,eval(expr=evalFun,env=asymDat))}
      max_yHat <- max(c(yHat,asym_yHat),na.rm=T)
      min_yHat <- min(c(yHat,asym_yHat),na.rm=T)

      if(max_yHat > y_lim[2] || min_yHat < y_lim[1]){err <- 1E15} # check for prediction outside of Y boundaries
      if(any(guesses<parLims$parMin) || any(guesses>parLims$parMax)){err <- 1E15}

      if(penalizeMean[1]){err <- err*(1+(penalizeMean[2]-mean(yHat,na.rm=T))^2)} # err * 1 (as long as mean of yhat = mean of null model)


      if(err < 1E14){
        # # # ensure reasonable thresholds, if relevant
        if(!is.na(thresh_fun)){
          threshHat <- eval(as.formula(paste('~',thresh_fun))[[2]], envir=curDat)
          # print(fivenum(threshHat))
          if (min(threshHat,na.rm=T)<0 || max(threshHat,na.rm=T) > max(abs(varIn[,linkFunX]),na.rm=T)*2){
            err <- 1E15
          }
        }
        err <- tef_checkPars(err,guesses,curDat,pNames,evalFun,errFun,respVar,linkFunX,
                             y_lim,rate_lim,shape_lim,penalizeRate,paramTerms,guessGroups=NA)
      }
    } # close "too many NAs" if statement


  return(err)

}

