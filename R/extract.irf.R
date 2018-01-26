#' Extract impulse-response series from irf objects
#'
#' Extracts the desired impulse-response serie from irf objects for a given quantile.
#'
#'
#' @param object A irf object.
#' @param impulse Impulse variable name or index.
#' @param response Response Variable name or index.
#' @param probs numeric vector of probabilities with values in [0,1].
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
#' irfB=irf(modelB,identB,h=12,boot = TRUE,M=100)
#' extract.irf(irfB,1,2,probs=0.5)
#'
#'
#' @references
#' Garcia, Medeiros and Vasconcelos (2017).
#' @seealso \code{\link{predict}}, \code{\link{lbvar}}, \code{\link{identification}}, \code{\link{irf}}



extract.irf=function(object,impulse,response,probs=0.5){
  bootirf=object$density[[impulse]][[response]]
  aux=round(stats::quantile(1:ncol(bootirf),probs=0.5))
  irfs=bootirf[,aux]
  return(irfs)
}
