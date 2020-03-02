#' Get the TEfit model formula
#'
#' @param modList List of TEfit model details
#'
#' @export
#'
tef_vars2forms <- function(modList){
  ## ## >> need to modularize this, and have it interact with the link fun: <<<

  # # set up covariates
  {
  if(dim(modList$varIn)[2] >2){
    modList$covars <- names(modList$varIn)[3:ncol(modList$varIn)]
  }

  if(exists('blockTimeVar',modList)){
    modList$covars <- modList$covars[modList$covars!=modList$blockTimeVar]
  }

  if(modList$linkFun$link=='d_prime'){
    modList$covars <- modList$covars[modList$covars!= modList$linkFun$presence]
  }

  if(modList$linkFun$link=='logit'){
    if(!exists('logistX',modList$linkFun)){cat('\nYou need to define a logistX for your logit link')}
    modList$covars <- modList$covars[modList$covars!= modList$linkFun$logistX]
  }

  if(modList$linkFun$link=='weibull'){
    if(!exists('weibullX',modList$linkFun)){cat('\nYou need to define a weibullX for your weibull link')}
    modList$covars <- modList$covars[modList$covars!= modList$linkFun$weibullX]
  }

  ##
  ## ##
  ## ## ##
  # define your change function:
  modList <-  tef_getLinkedFun(modList)
}
  if(nchar(modList$explicit)>0){
    modList$modl_fun <- as.formula(modList$explicit)
}

  # # get your data and parameter names out
  if (length(modList$modl_fun) == 2L) {
    modList$modl_fun[[3L]] <- modList$modl_fun[[2L]]
    modList$modl_fun[[2L]] <- 0
  }

  varNames <- all.vars(modList$modl_fun)
  modList$dnames <- names(modList$varIn)
  modList$pNames <- varNames[is.na(match(varNames, modList$dnames))]
  ## get special pNames:
  if(modList$errFun=='exGauss_mu'){
    require(retimes)
    modList$pNames <- c(modList$pNames,'sigma_param','tau_param')
    modList$null_pNames <- c(modList$null_pNames,'sigma_param','tau_param')
  }
  if(modList$errFun=='exGauss_tau'){
    require(retimes)
    modList$pNames <- c(modList$pNames,'mu_param','sigma_param')
    modList$null_pNames <- c(modList$null_pNames,'mu_param','sigma_param')
  }
  if(modList$errFun=='wiener_dr'){
    require(RWiener)
    modList$pNames <- c(modList$pNames,'bs_param','ndt_param','bias_param')
    modList$null_pNames <- c(modList$null_pNames,'bs_param','ndt_param','bias_param')
  }
  ##

  modList$evalFun <- modList$modl_fun[[3]]
  if (length(modList$null_fun) == 2L) {
    modList$null_fun[[3L]] <- modList$null_fun[[2L]]
    modList$null_fun[[2L]] <- 0
  }
  modList$null_fun <- modList$null_fun[[3]]

  return(modList)
}
