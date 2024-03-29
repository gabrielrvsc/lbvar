#' Functions for lbvar objects
#'
#' Functions for lbvar objects
#'
#' @param object lbvar object generated by the function lbvar.
#' @param h Forecasting horizon. If h=0 returns the fitted values.
#' @param newdata Exogenous variables to compute the predictions. If the model has exogenous controls (xreg) newdata must be supplied.
#' @param type equation to extract coefficients by equation and block to extract blocks like cons. lags and xreg coefficients.
#' @param M Number of simulations for density forecasts.
#' @param ... Arguments for other methods.
#' @export
#' @examples
#' ## == This example uses the Brazilian inflation data from
#' #Garcia, Medeiros and Vasconcelos (2017) == ##
#' data("BRinf")
#' Y=BRinf[,1:59]# remove expectation variables
#'
#' modelB=lbvar(Y,p=4)
#' predict(modelB,h=10)
#'
#' # take a look at the coefficients
#' eq=coef(modelB,type="equation")
#' block=coef(modelB,type="block")
#' block$Lag1
#'
#' @references
#' Garcia, Medeiros and Vasconcelos (2017).
#' @seealso  \code{\link{lbvar}}


predict.lbvar=function(object,h=0,newdata=NULL,...){
  if(h==0){
    return(stats::fitted(object))
  }

  p = object$p
  b = t(object$coef.by.equation)
  aux = stats::embed(object$Y, p)
  aux = aux[nrow(aux), ]
  N = ncol(object$Y)
  prev.store = matrix(NA, h, N)
  exog=length(object$xreg)

  if(!is.matrix(newdata) & exog!=0){
    newdata=as.matrix(newdata)
  }

  for (i in 1:h) {
    if(exog>0){
      prev = c(1, aux,newdata[i,]) %*% b
    }else{
      prev = c(1, aux) %*% b
    }
    prev.store[i, ] = prev
    aux = c(prev, utils::head(aux, length(aux) - N))
  }
  final.prediction = prev.store
  colnames(final.prediction) = colnames(object$Y)
  return(final.prediction)

}

#' @rdname predict.lbvar
#' @export
coef.lbvar=function(object,type="equation",...){
  if(type=="equation") return(object$coef.by.equation)
  if(type=="block") return(object$coef.by.block)
}

#' @rdname predict.lbvar
#' @export
density_predict = function(object,h=1,M=10,newdata = NULL){
  p=object$p
  Ts=object$Ts
  covmat = object$covmat
  Ystar = object$Ystar
  Xstar = object$Xstar
  xx = solve(t(Xstar)%*%Xstar)

  mataux = matrix(NA,h,M)
  reslist = vector(mode = "list",length = ncol(Ystar))
  for(i in 1:length(reslist))reslist[[i]] = mataux
  names(reslist) = colnames(object$Y)


  for(m in 1:M){
    cat(m,"\n")
    psi = MCMCpack::riwish(Ts[2]+2+Ts[1]-p*ncol(object$Y)-1,object$covmat)
    sigma_bdraw = kronecker(psi,xx)
    vecbeta = as.vector(t(object$coef.by.equation))
    beta_sim_vec = mvtnorm::rmvnorm(1,mean = vecbeta ,sigma = sigma_bdraw)
    beta_sim = beta_sim_vec
    dim(beta_sim) = dim(t(object$coef.by.equation))

    object_aux = object
    object_aux$coef.by.block = beta_sim

    b = t(object_aux$coef.by.equation)
    aux = stats::embed(object_aux$Y, p)
    aux = aux[nrow(aux), ]
    N = ncol(object_aux$Y)
    prev.store = matrix(NA, h, N)
    exog=length(object_aux$xreg)

    if(!is.matrix(newdata) & exog!=0){
      newdata=as.matrix(newdata)
    }

    for (i in 1:h) {
      if(exog>0){
        prev = c(1, aux,newdata[i,]) %*% b
      }else{
        prev = c(1, aux) %*% b
      }
      sigma_draw = mvtnorm::rmvnorm(1,sigma = object$covmat)
      prev = prev + sigma_draw
      prev.store[i, ] = prev
      aux = c(prev, utils::head(aux, length(aux) - N))
    }
    final.prediction = prev.store
    colnames(final.prediction) = colnames(object_aux$Y)

    for(i in 1:ncol(final.prediction)){
      reslist[[i]][,m] = final.prediction[,i]
    }
    ## separar os final predictions nas distribuicoes de densidade
  }
  return(reslist)
}
