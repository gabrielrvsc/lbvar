
irfaux=function(object,ident,h,unity.shock=TRUE){
  p=object$p
  k=ncol(object$fitted)
  J=matrix(0,k,k*p)
  diag(J[1:k,1:k])=1

  aux=diag(k)
  if(unity.shock==FALSE){
    aux=aux%*%sqrt(ident$covmatu)
  }
  phi0=solve(ident$A)%*%aux

  A=unlist(object$coef.by.block[2:(p+1)])

  companion <- matrix(0, nrow = k * p, ncol = k * p)
  companion[1:k, 1:(k * p)] = A
  if (p > 1) {
    j <- 0
    for (i in (k + 1):(k * p)) {
      j <- j + 1
      companion[i, j] <- 1
    }
  }
  store.phi=list()
  store.phi[[1]]=diag(k)
  aux=companion
  for(i in 2:(h+1)){
    store.phi[[i]]=J%*%aux%*%t(J)
    aux=companion%*%aux
  }
  irmat=lapply(store.phi,function(x) x %*% phi0)

  aux=matrix(NA,h+1,k)
  colnames(aux)=colnames(object$Y)
  ir=list()
  for(i in 1:k){
    ir[[i]]=aux
  }

  for(i in 1:k){
    for(j in 1:length(irmat)){
      ir[[i]][j,]=irmat[[j]][,i]
    }
  }
  names(ir)=colnames(object$Y)
  return(ir)
}

identaux=function(object){
  p=object$p
  Ts=object$Ts
  covmat_e=MCMCpack::riwish(Ts[2]+2+Ts[1]-p*ncol(object$Y),object$covmat*Ts[3])
  choles=chol(covmat_e)
  covmat_u_sqrt=diag(diag(choles))

  A=solve(t(solve(covmat_u_sqrt)%*%choles))
  sigma2u=covmat_u_sqrt^2
  colnames(A)=colnames(sigma2u)=rownames(sigma2u)=rownames(A)=rownames(covmat_e)
  A[upper.tri(A)]=0

  return(list("A"=A,"covmatu"=sigma2u))
}
