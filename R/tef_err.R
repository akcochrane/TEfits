

#' Calculate error for TEFit::tef_fitErr
#'
#' @param y Actual response variable values
#' @param yHat Predicted response variabel values
#' @param errFun Error function (e.g., OLS, logcosh)
#' @param curDat Data input. Only really relevant when calculating a full likelihood function and needing info beyond yHat
#'
#' @export
#'
tef_err <- function(y,yHat,errFun,curDat=NA){

  y_n <- min(c(sum(!is.na(y)),sum(!is.na(yHat))))

  switch(errFun
         ########%#
         ,ols={
           errVect <- (y-yHat)^2
           err <- sum(errVect,na.rm=T)
           if(sum(!is.na(errVect))<y_n){err <- 1E12}
         }
         ########%#
         ,logcosh={
           errVect <- log(cosh(y-yHat))
           err <- sum(errVect,na.rm=T)
           if(sum(!is.na(errVect))<y_n){err <- 1E12}
         }
         ########%#
         ,rmse={
           errVect <- (y-yHat)^2
           err <- sqrt(mean(errVect,na.rm=T))
           if(sum(!is.na(errVect))<y_n){err <- 1E12}
         }
         ########%# Is really pretty bad:
         ,median={
           fourParts <- cut(seq_along(y), 4, labels = FALSE)
           threeParts <- cut(seq_along(y), 3, labels = FALSE)
           err <- (
             (abs(sum(sign(y-yHat),na.rm=T))/length(y)+1)* ## some median line

               (abs(sum(sign(y[fourParts==1]-yHat[fourParts==1]),na.rm=T))/sum(fourParts==1)+1)* ## some first-quartile median line
               (abs(sum(sign(y[fourParts==2]-yHat[fourParts==2]),na.rm=T))/sum(fourParts==2)+1)* ## some second-quartile median line
               (abs(sum(sign(y[fourParts==3]-yHat[fourParts==3]),na.rm=T))/sum(fourParts==3)+1)* ## some third-quartile median line
               (abs(sum(sign(y[fourParts==4]-yHat[fourParts==4]),na.rm=T))/sum(fourParts==4)+1)* ## some fourth-quartile median line

               2^(min(c(abs(cor(y[fourParts==1]-yHat[fourParts==1],yHat[fourParts==1],method='spearman',use='complete')),1),na.rm=T)) * ## that minimizes rank correlation between prediction and residuals
               2^(min(c(abs(cor(y[fourParts==2]-yHat[fourParts==2],yHat[fourParts==2],method='spearman',use='complete')),1),na.rm=T)) * ## that minimizes rank correlation between prediction and residuals
               2^(min(c(abs(cor(y[fourParts==3]-yHat[fourParts==3],yHat[fourParts==3],method='spearman',use='complete')),1),na.rm=T)) * ## that minimizes rank correlation between prediction and residuals
               2^(min(c(abs(cor(y[fourParts==4]-yHat[fourParts==4],yHat[fourParts==4],method='spearman',use='complete')),1),na.rm=T)) * ## that minimizes rank correlation between prediction and residuals

               (abs(sum(sign(y[threeParts==2]-yHat[threeParts==1]),na.rm=T))/sum(threeParts==1)+1)* ## some second-tertile median line
               (abs(sum(sign(y[threeParts==3]-yHat[threeParts==2]),na.rm=T))/sum(threeParts==2)+1)* ## some third-tertile median line
               (abs(sum(sign(y[threeParts==4]-yHat[threeParts==3]),na.rm=T))/sum(threeParts==3)+1)* ## some fourth-tertile median line

               2^(min(c(abs(cor(y[threeParts==1]-yHat[threeParts==1],yHat[threeParts==1],method='spearman',use='complete')),1),na.rm=T)) * ## that minimizes rank correlation between prediction and residuals
               2^(min(c(abs(cor(y[threeParts==2]-yHat[threeParts==2],yHat[threeParts==2],method='spearman',use='complete')),1),na.rm=T)) * ## that minimizes rank correlation between prediction and residuals
               2^(min(c(abs(cor(y[threeParts==3]-yHat[threeParts==3],yHat[threeParts==3],method='spearman',use='complete')),1),na.rm=T)) * ## that minimizes rank correlation between prediction and residuals


               2^(min(c(abs(cor(y-yHat,yHat,method='spearman',use='complete')),1),na.rm=T)) * ## that minimizes rank correlation between prediction and residuals
               2^(min(c((1-cor(y,yHat,method='spearman',use='complete')),2),na.rm=T))  ## and maximizes rank correlation between prediction and actual values.
           )/(2^17)

         }
         ########%#
         ,bernoulli={
           if(any(y>1,na.rm=T) || any(y<0,na.rm=T)){
             cat('\nYour data are outside the bounds of zero and one.\n')
             err <- 1E12
           }
           errVect <- y*log(yHat) + (1-y)*log(1-yHat)
           err <- -sum(errVect,na.rm=T)

           if(sum(!is.na(errVect)) < y_n ||
              max(abs(yHat-.5),na.rm=T)<.02){err <- 1E12} ## check for flat fits
         }
         ########%#
         ,exGauss_mu={
           exGlik <- dexgauss(
             y,
             mu=yHat,sigma=curDat$sigma_param,tau=curDat$tau_param
           )
           err <- -sum(log(exGlik),na.rm=T)
           if(curDat$sigma_param < 0 ||
              curDat$sigma_param > mad(y,na.rm=T)*2 ||
              curDat$tau_param < 0 ||
              sum(!is.na(exGlik))<y_n
           ){err <- 1E12}
         }
         ########%#
         ,exGauss_tau={
           exGlik <- dexgauss(
             y,
             mu=curDat$mu_param,sigma=curDat$sigma_param,tau=yHat
           )
           err <- -sum(log(exGlik),na.rm=T)
           if(curDat$sigma_param < 0 ||
              curDat$sigma_param > mad(y,na.rm=T)*2 ||
              curDat$mu_param < 0||
              sum(!is.na(exGlik))<y_n
           ){err <- 1E12}
         }
  )
  return(err)
}
