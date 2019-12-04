

#' Gets link for for TEFit
#'
#' This function also calculates default rate and [weibull] shape bounds
#'
#' @param modList List of TEfit model details
#'
#' @export
#'
tef_getLinkedFun <- function(modList){


  ## set up, for the link funs:
{
  if(modList$changeFun=='expo' ||
     modList$changeFun=='expo_block' ||
     modList$changeFun=='expo_double' ||
     modList$changeFun=='expo_fatigue' ||
     modList$changeFun=='weibull'){
    # # default to min=50% @ sd(timeVar[1:10]) and max=80% @ max(timeVar)
  if(all(modList$rate_lim == 0)){
    modList$rate_lim[1] <- log(sd(
      sort(unique(na.omit(modList$varIn[,2])))[1:min(10,length(unique(na.omit(modList$varIn[,2]))))]
      )-1,base=modList$expBase)
    modList$rate_lim[2] <- log(
      (1-max(modList$varIn[,2],na.rm=T))/log(.25,base=modList$rateBase)
      ,base=modList$expBase)
  }
  }

  if(modList$changeFun=='weibull'){
    if(all(modList$shape_lim == 0)){
   modList$shape_lim <-  c(-2,2)
  }}

   if(modList$changeFun=='power' || modList$changeFun=='power4'){
    if(!exists('propRemain',modList)){modList$propRemain <- .25}
     if(all(modList$rate_lim == 0)){modList$rate_lim <-
       c(log(4,base=modList$rateBase),log(max(modList$varIn[,2],na.rm=T)*2,base=modList$rateBase))
     }}
}

  ## the funs:
  if(modList$linkFun$link=='identity'){
    modList$covarTerms <- tef_getDatTerms(modList,pPrefix='p',whichChange = modList$changeFun)

    modList$modl_fun <- as.formula(paste0(modList$respVar,'~',tef_changeFun(modList,
                                                 covarVects=modList$covarTerms)))
    modList$null_fun <- as.formula(paste0(modList$respVar,'~Intercept'))
    modList$null_pNames <- c('Intercept')
  }

  if(modList$linkFun$link=='logit'){

    ## ## default to changing threshold
    thresh_covars <- tef_getDatTerms(modList,pPrefix='thresh',whichChange=modList$changeFun)
    modList$thresh_fun    <-tef_changeFun(modList,covarVects = thresh_covars)
    if(exists('threshChange',modList$linkFun)){
      if(modList$linkFun$threshChange==F){
        modList$thresh_fun <- 'thresh'
      }}
    ## ## default to stable bias
    modList$bias_fun <- 'bias' ; bias_covars = c()
    if(exists('biasChange',modList$linkFun)){
      if(modList$linkFun$biasChange==T){
        bias_covars <- tef_getDatTerms(modList,pPrefix='bias',whichChange=modList$changeFun)
        modList$bias_fun    <- tef_changeFun(modList,covarVects = bias_covars)
      }}

    if(exists('lapseRate',modList$linkFun)){
      lapseRate <- modList$linkFun$lapseRate
    }else{lapseRate <- modList$linkFun$lapseRate <- .005}
    if(exists('fitThresh',modList$linkFun)){
      fitThresh <- modList$linkFun$fitThresh
    }else{fitThresh <- modList$linkFun$fitThresh <- .75}

    fit_thresh <- fitThresh/(1-fitThresh)

    modList$covarTerms <- list(bias_covars=bias_covars,thresh_covars=thresh_covars)

    modList$modl_fun <-     as.formula(paste0(modList$respVar,'~',lapseRate,' + (1-2*',lapseRate,')/(1+',fit_thresh,'^(((',
                                              modList$bias_fun,')-',modList$linkFun$logistX,')/(',
            modList$thresh_fun,')))'
         )
      )
    modList$null_fun <-     as.formula(paste0(modList$respVar,'~',lapseRate,' + (1-2*',lapseRate,')/(1+',fit_thresh,'^(((',
                                              'bias)-',modList$linkFun$logistX,')/(',
                                              'thresh)))'
    )
    )
    modList$null_pNames <- c('bias','thresh')
  }

  if(modList$linkFun$link=='weibull'){ #### #### ## BARELY TESTED

    if(exists('yIntercept',modList$linkFun)){
      yIntercept <- modList$linkFun$yIntercept
    }else{yIntercept <- modList$linkFun$yIntercept <- .5}
    if(exists('rhAsymptote',modList$linkFun)){
      rhAsymptote <- modList$linkFun$rhAsymptote
    }else{rhAsymptote <- modList$linkFun$rhAsymptote <- 1}
    if(exists('lapseRate',modList$linkFun)){
      lapseRate <- modList$linkFun$lapseRate
    }else{lapseRate <- modList$linkFun$lapseRate <- .005}
    if(exists('fitThresh',modList$linkFun)){
      fitThresh <- modList$linkFun$fitThresh
    }else{fitThresh <- modList$linkFun$fitThresh <- .75}

    expBase <- round(
      uniroot(function(expBase) fitThresh-(yIntercept+((1-yIntercept)-lapseRate)*(1-expBase^(-1))),c(.0001,1000))$root
      ,4)

    modList$covarTerms <- tef_getDatTerms(modList,pPrefix='thresh',whichChange=modList$changeFun)
    modList$thresh_fun    <-tef_changeFun(modList,covarVects = modList$covarTerms)

    modList$modl_fun <-     as.formula(paste0(modList$respVar,'~',
                                              yIntercept,'+((',rhAsymptote,'-',yIntercept,')-',lapseRate,')*(1-',
                                              expBase,'^(-(',modList$linkFun$weibullX,'/(',
                                              modList$thresh_fun,'))^weibull_shape))'

    )
    )

    modList$null_fun <-     as.formula(paste0(modList$respVar,'~',
                                              yIntercept,'+((1-',yIntercept,')-',lapseRate,')*(1-',
                                              expBase,'^(-(',modList$linkFun$weibullX,
                                              '/thresh)^weibull_shape))'

    )
    )
    modList$null_pNames <- c('thresh','weibull_shape')

    # dat <- data.frame(yv = c(seq(.5,.8,length=50),rep(.8,50)),tv = 1:100,xv=rep(c(.5,1,1.5,2),25)) ; dat$yv <- dat$yv * plogis(dat$xv)
    # m <- TEfit(dat,linkFun=list(link='weibull',yIntercept = .25,weibullX='xv'),bootPars=list(nBoots=20))
    # plot(m)

    # 0.5 + (0.5 - lapseRate)(1 - expBase^(-(weibullX/threshold)^shape))
    # [in GLCB2010 expBase=2.0851 so threshold point is at d'=1]
    # **why?? shouldn't it be around 1.62, solving for x^(-1)=1-(pnorm(.5)-.5)/.5 ?**
    # because we want pnorm(.5) = .5+.5*(1-expBase^(-1))
    #
    ## ## note differences from changefun: (a) "threshold" is "rate" (b) exp term should be (a/b)^c but Gold Law Connolly Bennur 2010 say (a/b)*c
  }


  if(modList$linkFun$link=='d_prime'){ #### #### ## BARELY TESTED

    if(exists('max_d_prime',modList$linkFun)){
      max_d_prime <- modList$linkFun$max_d_prime
    }else{max_d_prime <- modList$linkFun$max_d_prime <- .75}

    if(length(unique(na.omit(as.numeric(
      modList$varIn[,modList$linkFun$presence]
    ))))!=2){cat('\nPlease fix your "presence" variable.\n')}

    modList$varIn[,1] <- tef_acc2dprime(modList$varIn[,modList$respVar],
                                        as.numeric(modList$varIn[,modList$linkFun$presence]),
                                        max_dprime=modList$linkFun$max_d_prime)
    names(modList$varIn)[1] <- modList$respVar <- 'd_prime'

    modList$covarTerms <- tef_getDatTerms(modList,pPrefix='p',whichChange = modList$changeFun)

    modList$modl_fun <- as.formula(paste0(modList$respVar,'~',tef_changeFun(modList,
                                                                            covarVects=modList$covarTerms)))
    modList$null_fun <- as.formula(paste0(modList$respVar,'~Intercept'))
    modList$null_pNames <- c('Intercept')

  }

  if(!exists('null_pNames',modList)){cat('\nYou may have had a problem entering your link function. ')}

  return(modList)
}


