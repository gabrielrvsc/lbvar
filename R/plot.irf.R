#' Plot impulse response funtion
#'
#' Plot impulse response function for the chosen impulse variable and response variable.
#'
#'
#' @param x A irf object.
#' @param impulse Impulse variable name or index.
#' @param response Response Variable name or index.
#' @param alpha Significance level for the confidence intervals. May be more than one value in a vector.
#' @param lty.cb Confidence bands graphic control.
#' @param lwd.cb Confidence bands graphic control.
#' @param ... Other graphical parameters.
#' @export
#' @keywords VAR, irf, High-dimension, Bayesian models
#' @examples
#' ## == This example uses the Brazilian inflation data from
#' #Garcia, Medeiros and Vasconcelos (2017) == ##
#'
#' # = This is an ilustrative example = #
#' # = The identification ignores which variables are more exogenous = #
#' data("BRinf")
#' Y=BRinf[,1:59]# remove expectation variables
#' modelB=lbvar(Y,p=3)
#' identB=identification(modelB)
#' irfB=irf(modelB,identB,h=12,M=100)
#' plot(irfB,1,2,alpha=0.1)
#'
#'
#' @references
#' Garcia, Medeiros and Vasconcelos (2017).
#' @seealso \code{\link{predict}}, \code{\link{lbvar}}, \code{\link{identification}}, \code{\link{irf}}



plot.irf=function(x,impulse,response,alpha=0.05,lty.cb=2,lwd.cb=1,...){
  drawirf=x$density[[impulse]][[response]]
  #ir=x$point.irf[[impulse]][,response]
  aux=round(stats::quantile(1:ncol(drawirf),probs=0.5))
  len=0:(nrow(drawirf)-1)
  graphics::plot(len,drawirf[,aux],type="l",...)
  graphics::abline(h=0,col="yellow",lty=2)
  if(!is.null(drawirf)){
    for(i in 1:length(alpha)){
      aux=round(stats::quantile(1:ncol(drawirf),probs=c(alpha[i]/2,1-alpha[i]/2)))
      graphics::lines(len,drawirf[,aux[1]],col=i+1,lty=lty.cb,lwd=lwd.cb)
      graphics::lines(len,drawirf[,aux[2]],col=i+1,lty=lty.cb,lwd=lwd.cb)
    }
  }
}
