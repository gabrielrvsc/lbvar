#' VAR ipulse response functions
#'
#' Estimates impulse response coefficients of a lbvar model h steps ahead drawing from the posterior.
#'
#'
#' @param object A lbvar object.
#' @param ident A list with two elements: $A must be a matrix with the contemporaneus coefficients of a identified VAR and $sigma2u must be the structural shocks covariance matrix. This is the output of the function chol.identification.
#' @param h Number of steps ahead.
#' @param M Number of draws.
#' @param unity.shock If TRUE the impulses are equal 1. If FALSE the impulses are of one standard deviation (default=TRUE).
#' @return An object with S3 class "irf".
#' \item{point.irf}{A list with the point ir coefficients. Each element in the list is a matrix with the response on all variables cause by an impulse on the variable that gives name to the matrix.}
#' \item{density}{A list that stores the ir coefficients from each draw. Each element in the list is another list with the response on all variables cause by an impulse on the variable that gives name to the list.}
#' @keywords VAR, irf, High-dimension, Bayesian models
#' @export
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
#' @seealso \code{\link{predict}}, \code{\link{lbvar}}, \code{\link{identification}}, \code{\link{plot.irf}}


irf=function (object, ident, h, M=100, unity.shock = TRUE)
{
  #pointirf = irfaux(object, ident, h, unity.shock)

  Y = as.matrix(object$Y)
  xreg=object$xreg
  N = object$N
  p = object$p
  nvar = ncol(object$residuals)
  delta=object$delta
  lambda=object$lambda
  ps=object$ps
  tau=object$tau
  # == prep data == #
  save.irf = list()
  aux1 = list()
  aux = matrix(NA, h + 1, M)
  for (i in 1:ncol(Y)) {
    aux1[[i]] = aux
  }
  names(aux1) = colnames(Y)
  for (i in 1:ncol(Y)) {
    save.irf[[i]] = aux1
  }
  names(save.irf) = colnames(Y)
  for (m in 1:M) {
    identaux = identaux(object)
    irfdraw = irfaux(object, identaux, h = h, unity.shock = unity.shock)
    for (i in 1:nvar) {
      for (j in 1:nvar) {
        save.irf[[i]][[j]][, m] = irfdraw[[i]][, j]
      }
    }
  }
  for (i in 1:nvar) {
    for (j in 1:nvar) {
      save.irf[[i]][[j]] = t(apply(save.irf[[i]][[j]],
                                   1, sort))
    }
  }
  aux = irfaux(object, ident, h, unity.shock)
  result=list(density = save.irf, call=match.call())
  class(result)="irf"
  return(result)
}
